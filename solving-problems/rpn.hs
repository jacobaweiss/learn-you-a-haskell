import Data.List

solveRPN :: (Num a, Read a) => String -> Float
solveRPN = head . foldl evalExpression [] . words
  where evalExpression (x:y:ys) "*" = (x * y):ys
        evalExpression (x:y:ys) "+" = (x + y):ys
        evalExpression (x:y:ys) "-" = (y - x):ys
        evalExpression (x:y:ys) "/" = (y / x):ys
        evalExpression (x:y:ys) "^" = (y ** x):ys
        evalExpression (x:xs) "ln" = log x:xs
        evalExpression xs "sum" = [sum xs]
        evalExpression xs numberString = read numberString:xs
