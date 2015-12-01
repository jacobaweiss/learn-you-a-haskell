import Data.Char

main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly input =
  let allLines = lines input
      shortLines = filter (\a -> length a < 10) allLines
      result = unlines shortLines
  in  result

main2 = interact $ unlines . filter ((<10) . length) . lines
