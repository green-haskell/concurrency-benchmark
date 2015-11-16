module Types where

import Prelude hiding ( Word )
import Data.Map
import Data.IntMap
import Data.Array

type Word = String

type Positions = [Int]

type Occurrences = Map Word Positions

type Vocabulary = Map Word [(FilePath, Positions)]

data Index = Index Int Int (IntMap Vocabulary)

data QueryIndex = QueryIndex Int Int (Array Int Vocabulary)

type QueryResult =  [(FilePath, Int)]


class TSearchIndex a where
    id' :: a -> Int
    numberOfFiles :: a -> Int

instance TSearchIndex Index where
    id' (Index i _ _) = i
    numberOfFiles (Index _ n _) = n

instance TSearchIndex QueryIndex where
    id' (QueryIndex i _ _) = i
    numberOfFiles (QueryIndex _ n _) = n
