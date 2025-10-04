import Data.Char (isUpper)

-- HC5T1 - Apply a Function Three Times
applyThrice :: (Int -> Int) -> Int -> Int
applyThrice f x = f (f (f x))

-- HC5T2 - Filtering Odd Numbers
oddNumbers :: [Int]
oddNumbers = filter odd [1..30]

-- HC5T3 - Checking for Uppercase Words
hasUppercaseWord :: [String] -> Bool
hasUppercaseWord = any (\w -> not (null w) && isUpper (head w))

-- HC5T4 - Lambda Function Example
biggerThan10 :: Int -> Bool
biggerThan10 = \x -> x > 10

-- HC5T5 - Partial Application
multiplyByFive :: Int -> Int
multiplyByFive = (*5)

-- HC5T6 - Function Composition
evenSquares :: [Int] -> [Int]
evenSquares = filter even . map (^2)

-- HC5T7 - The $ Operator
result :: Int
result = sum $ map (*2) $ filter (>3) [1..10]

-- HC5T8 - Point-Free Style
addFive :: Int -> Int
addFive = (+5)

-- HC5T9 - Transform List Using Higher-Order Function
transformList :: (a -> a) -> [a] -> [a]
transformList f = map (f . f)

-- HC5T10 - Combining Higher-Order Functions
hasLargeSquare :: [Int] -> Bool
hasLargeSquare = any (>50) . map (^2)

-- MAIN
main :: IO ()
main = do
    putStrLn "HC5T1 - Apply Thrice:"
    print (applyThrice (+1) 5)

    putStrLn "\nHC5T2 - Filtering Odd Numbers:"
    print oddNumbers

    putStrLn "\nHC5T3 - Checking for Uppercase Words:"
    print (hasUppercaseWord ["apple", "Banana", "cherry"])
    print (hasUppercaseWord ["apple", "banana", "cherry"])

    putStrLn "\nHC5T4 - Lambda Function Example:"
    print (biggerThan10 5)
    print (biggerThan10 15)

    putStrLn "\nHC5T5 - Partial Application:"
    print (multiplyByFive 4)
    print (multiplyByFive 10)

    putStrLn "\nHC5T6 - Function Composition (Even Squares):"
    print (evenSquares [1..10])

    putStrLn "\nHC5T7 - The $ Operator:"
    print result

    putStrLn "\nHC5T8 - Point-Free Style:"
    print (addFive 10)

    putStrLn "\nHC5T9 - Transform List (apply twice):"
    print (transformList (+1) [1,2,3])

    putStrLn "\nHC5T10 - Combining Higher-Order Functions:"
    print (hasLargeSquare [3,5,7])
    print (hasLargeSquare [8,9,10])
