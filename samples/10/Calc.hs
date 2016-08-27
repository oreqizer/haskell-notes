module RPN where

    solveRPN :: String -> Float
    solveRPN = head . foldl calculator [] . words
        where   calculator (x:y:ys) "*" = (x * y):ys
                calculator (x:y:ys) "+" = (x + y):ys
                calculator (x:y:ys) "/" = (y / x):ys
                calculator (x:y:ys) "-" = (y - x):ys
                calculator (x:y:ys) "^" = (y ** x):ys
                calculator (x:xs) "ln" = log x:xs
                calculator xs "sum" = [sum xs]
                calculator xs numString = read numString:xs
