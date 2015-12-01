import System.Environment
import System.IO

main = do
  args <- getArgs
  withFile (head args) ReadMode (\handle -> do
    contents <- hGetContents handle
    putStr contents)
