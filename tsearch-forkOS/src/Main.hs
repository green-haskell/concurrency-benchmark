import Program ( exec' )

import System.Environment ( getArgs )

main :: IO ()
main = do
    (path:rawQueries) <- getArgs
    exec' path rawQueries
