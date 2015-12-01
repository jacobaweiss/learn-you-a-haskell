import System.Environment
import System.IO
import System.Directory
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("add", add)
           , ("view", view)
           , ("remove", remove)
           , ("bump", bump)
           ]

main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args

add :: [String] -> IO ()
add [fileName, item] = appendFile fileName (item ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
  putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
  handle <- openFile fileName ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let number = read numberString
      todoTasks = lines contents
      newTodoTasks = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle $ unlines newTodoTasks
  hClose handle
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName

bump :: [String] -> IO ()
bump [fileName, numberString] = do
  handle <- openFile fileName ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let number = read numberString
      todoTasks = lines contents
      movedTask = todoTasks !! number
      newTodoTasks = movedTask : (delete movedTask todoTasks)
  hPutStr tempHandle $ unlines newTodoTasks
  hClose handle
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName
