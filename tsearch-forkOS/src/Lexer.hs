module Lexer ( processContent
             , tokenize ) where

import Prelude hiding ( Word )
import Data.Char ( isAscii, isAlphaNum, toLower )
import qualified Data.Map as Map
import Types

tokenize :: String -> [Word]
tokenize content = words $ map lowerAndReplace $ filter isValid content
    where
        isValid c = (isAscii c && isAlphaNum c) || c == ' ' || c == '\n'
        lowerAndReplace c = if (c == '\n') then ' ' else toLower c

processContent :: String -> (Int, Occurrences)
processContent content = process' words' (0, Map.empty)
    where
        words' = tokenize content

process' :: [Word] -> (Int, Occurrences) -> (Int, Occurrences)
process' [] pair = pair
process' (w:ws) (i, map') = process' ws (i + 1, Map.insertWith (++) w [i] map')
