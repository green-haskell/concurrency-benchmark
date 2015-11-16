module Buffer ( Buffer
              , newEmptyBuffer
              , enableFlag
              , readFlag
              , readBuffer
              , readBuffer'
              , writeBuffer ) where

import Control.Concurrent.STM

data Buffer a = Buffer (TChan a) (TVar Int) (TVar Bool)

newEmptyBuffer :: STM (Buffer a)
newEmptyBuffer = do
    chan <- newTChan
    counter <- newTVar 0
    flag <- newTVar False
    return (Buffer chan counter flag)

enableFlag :: Buffer a -> STM ()
enableFlag (Buffer _ _ flagVar) = writeTVar flagVar True

readFlag :: Buffer a -> STM Bool
readFlag (Buffer _ _ flagVar) = readTVar flagVar

readBuffer' :: Buffer a -> STM a
readBuffer' (Buffer chan counterVar _) = do
    counter <- readTVar counterVar
    value <- readTChan chan
    writeTVar counterVar (counter - 1)
    return value

readBuffer :: Buffer a -> STM (Maybe a)
readBuffer buffer@(Buffer _ counterVar flagVar) = do
    flag <- readTVar flagVar
    if not flag
        then do
            value <- readBuffer' buffer
            return $ Just value
        else do
            counter <- readTVar counterVar
            if (counter == 0)
                then return Nothing
                else do
                    value <- readBuffer' buffer
                    return $ Just value

writeBuffer :: Buffer a -> a -> STM ()
writeBuffer (Buffer chan countVar _) value = do
    counter <- readTVar countVar
    writeTVar countVar (counter + 1)
    writeTChan chan value
