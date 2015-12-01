import System.Environment
import System.IO

main = do
  args <- getArgs
  contents <- readFile $ head args
  writeFile (args !! 1) contents
