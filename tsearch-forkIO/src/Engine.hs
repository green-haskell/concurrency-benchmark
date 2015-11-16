module Engine ( processFiles
              , processSearch ) where

import Prelude hiding ( Word )
import Control.Monad ( forM, forM_ )
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Concurrent.STM.SSem as Sem
import Control.DeepSeq
import qualified Data.Set as Set
import qualified Data.Map as Map ( keysSet )
import System.CPUTime ( getCPUTime )

import Lexer
import Index
import Query
import Types
import Buffer
import Logger

createQueryIndex :: Index -> Buffer QueryIndex -> LogBuffer -> IO ()
createQueryIndex index buffer logBuffer = do
    let queryIndex = Index.buildQueryIndex index
    atomically $ writeBuffer buffer queryIndex
    Logger.subIndexCompleted logBuffer (id' index) (numberOfFiles index)


processRemaingIndices :: TChan Index -> Buffer QueryIndex -> LogBuffer -> IO ()
processRemaingIndices indexBuffer queryIndexBuffer logBuffer = do
    response <- atomically $ tryReadTChan indexBuffer
    case response of
        Nothing -> atomically $ enableFlag queryIndexBuffer
        Just index -> do
            finished <- atomically $ isEmptyTChan indexBuffer
            let queryIndex = Index.buildQueryIndex index
            atomically $ do
                if finished then (enableFlag queryIndexBuffer) else return ()
                writeBuffer queryIndexBuffer queryIndex

            Logger.subIndexCompleted logBuffer (id' index) (numberOfFiles index)
            processRemaingIndices indexBuffer queryIndexBuffer logBuffer


waiter :: SSem -> TChan Index -> Buffer QueryIndex -> LogBuffer -> IO ()
waiter finishProcessing indexBuffer queryIndexBuffer logBuffer = do
    atomically $ Sem.wait finishProcessing
    processRemaingIndices indexBuffer queryIndexBuffer logBuffer


processFile' :: Int -> FilePath -> Int -> TChan Index -> TVar Int -> TVar (Set.Set Word) -> Buffer QueryIndex -> LogBuffer -> IO ()
processFile' taskId filePath maxFiles indexBuffer indexIdVar wordSetVar queryIndexBuffer logBuffer = do
    content <- readFile filePath
    (words', occurrences) <- return $!! Lexer.processContent content

    wordSet <- atomically $ readTVar wordSetVar
    let newWordSet = Set.union wordSet $ Map.keysSet occurrences
    atomically $ writeTVar wordSetVar newWordSet

    index <- atomically $ readTChan indexBuffer
    let newIndex = Index.insert (filePath, occurrences) index

    indexedWords' <- return $! Set.size newWordSet
    Logger.fileProcessed logBuffer taskId filePath words' indexedWords'

    if ((numberOfFiles newIndex) < maxFiles)
        then atomically $ writeTChan indexBuffer newIndex
        else do
            _ <- forkIO $ createQueryIndex newIndex queryIndexBuffer logBuffer
            atomically $ do
                indexCounter <- readTVar indexIdVar
                let nextId = indexCounter + 1
                writeTVar indexIdVar nextId
                writeTChan indexBuffer $ Index.newEmptyIndex nextId


processFile :: Int -> Buffer FilePath -> Int -> TChan Index -> TVar Int -> TVar (Set.Set Word) -> Buffer QueryIndex -> LogBuffer -> SSem -> IO ()
processFile taskId fileBuffer maxFiles indexBuffer indexIdVar wordSetVar queryIndexBuffer logBuffer finishProcessing = do
    next <- atomically $ readBuffer fileBuffer
    case next of
        Nothing -> atomically $ Sem.signal finishProcessing
        Just filePath -> do
            processFile' taskId filePath maxFiles indexBuffer indexIdVar wordSetVar queryIndexBuffer logBuffer
            processFile taskId fileBuffer maxFiles indexBuffer indexIdVar wordSetVar queryIndexBuffer logBuffer finishProcessing


processFiles :: Int -> Int -> Int -> Buffer FilePath -> Buffer QueryIndex -> LogBuffer -> IO ()
processFiles initialSubIndices maxFiles nWorkers fileBuffer queryIndexBuffer logBuffer = do
    indexBuffer <- atomically newTChan
    forM_ [1..initialSubIndices] $ \i ->
        atomically $ writeTChan indexBuffer $ Index.newEmptyIndex i

    wordSetVar <- atomically $ newTVar Set.empty
    indexIdVar <- atomically $ newTVar initialSubIndices

    finishProcessing <- atomically $ Sem.new (1 - nWorkers)
    forM_ [1..nWorkers] $ \taskId ->
        forkIO $ processFile taskId fileBuffer maxFiles indexBuffer indexIdVar wordSetVar queryIndexBuffer logBuffer finishProcessing

    _ <- forkIO $ waiter finishProcessing indexBuffer queryIndexBuffer logBuffer
    return ()


search :: Query -> Buffer QueryIndex -> (Integer, QueryResult) -> SSem -> LogBuffer -> IO ()
search query indexBuffer result finishSearch logBuffer = do
    response <- atomically $ readBuffer indexBuffer
    case response of
        Nothing -> do
            Logger.searchPerformed logBuffer query result
            atomically $ Sem.signal finishSearch
        Just index -> do
            start <- getCPUTime
            r <- return $!! Query.perform query index
            end <- getCPUTime

            let diff = (end - start) `div` (10^9)
            let newResult = (fst result + diff, snd result ++ r)

            Logger.queryPerformed logBuffer query (id' index)
            search query indexBuffer newResult finishSearch logBuffer

searchMultiplexer :: Buffer QueryIndex -> [Buffer QueryIndex] -> IO ()
searchMultiplexer master slaves = do
    response <- atomically $ readBuffer master
    case response of
        Nothing -> return ()
        Just index -> do
            finished <- atomically $ readFlag master
            forM_ slaves $ \slave -> atomically $ do
                if finished then (enableFlag slave) else return ()
                writeBuffer slave index

            searchMultiplexer master slaves

processSearch :: [String] -> Buffer QueryIndex -> LogBuffer -> IO SSem
processSearch rawQueries queryIndexBuffer logBuffer = do
    finishSearch <- atomically $ Sem.new (1 - length rawQueries)

    slaveBuffers <- forM rawQueries $ \rawQuery -> do
        qBuffer <- atomically $ newEmptyBuffer
        _ <- forkIO $ Engine.search (Query.parse rawQuery) qBuffer (0, []) finishSearch logBuffer
        return qBuffer

    _ <- forkIO $ Engine.searchMultiplexer queryIndexBuffer slaveBuffers

    return finishSearch
