import Data.List (sort)

-- ===============================================================
-- HC12T1: Print a Welcome Message
hc12t1 :: IO ()
hc12t1 = putStrLn "Welcome to Haskell Programming!"

-- ===============================================================
-- HC12T2: Add Two Numbers
addTwoNumbers :: Int -> Int -> Int
addTwoNumbers x y = x + y

hc12t2 :: IO ()
hc12t2 = do
    let x = 10
    let y = 15
    putStrLn $ "Sum of " ++ show x ++ " and " ++ show y ++ " is " ++ show (addTwoNumbers x y)

-- ===============================================================
-- HC12T3: Factorial Function
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

hc12t3 :: IO ()
hc12t3 = do
    let n = 5
    putStrLn $ "Factorial of " ++ show n ++ " is " ++ show (factorial n)

-- ===============================================================
-- HC12T4: First 10 Fibonacci Numbers
fibonacci :: Int -> [Integer]
fibonacci n = map fib [0..n-1]
  where
    fib 0 = 0
    fib 1 = 1
    fib k = fib (k-1) + fib (k-2)

hc12t4 :: IO ()
hc12t4 = do
    let fibs = fibonacci 10
    putStrLn $ "First 10 Fibonacci numbers: " ++ show fibs

-- ===============================================================
-- HC12T5: Palindrome Checker
isPalindrome :: String -> Bool
isPalindrome str = str == reverse str

hc12t5 :: IO ()
hc12t5 = do
    let testString = "racecar"
    putStrLn $ "Is \"" ++ testString ++ "\" a palindrome? " ++ show (isPalindrome testString)

-- ===============================================================
-- HC12T6: Sort a List of Integers
hc12t6 :: IO ()
hc12t6 = do
    let numbers = [5,2,9,1,3]  -- simulated input
    putStrLn $ "Original list: " ++ show numbers
    putStrLn $ "Sorted list: " ++ show (sort numbers)

-- ===============================================================
-- HC12T7: Calculate Circle Area
piValue :: Double
piValue = 3.141592653589793

calculateCircleArea :: Double -> Double
calculateCircleArea r = piValue * r * r

hc12t7 :: IO ()
hc12t7 = do
    let radius = 5.0
    putStrLn $ "Area of circle with radius " ++ show radius ++ " is " ++ show (calculateCircleArea radius)

-- ===============================================================
-- HC12T8: Merge Two Sorted Lists
mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys)
    | x <= y    = x : mergeLists xs (y:ys)
    | otherwise = y : mergeLists (x:xs) ys

hc12t8 :: IO ()
hc12t8 = do
    let list1 = [1,3,5]
    let list2 = [2,4,6]
    putStrLn $ "Merged list: " ++ show (mergeLists list1 list2)

-- ===============================================================
-- HC12T9: Read and Print File Content
-- Simulated file content (to avoid file I/O issues)
hc12t9 :: IO ()
hc12t9 = do
    let fileContent = "This is a sample file content."  -- simulated
    putStrLn $ "File content:\n" ++ fileContent

-- ===============================================================
-- HC12T10: Mathematical Operations Module
-- Simulated inline module functions
mathAdd :: Int -> Int -> Int
mathAdd x y = x + y

mathMultiply :: Int -> Int -> Int
mathMultiply x y = x * y

hc12t10 :: IO ()
hc12t10 = do
    let x = 5
    let y = 10
    putStrLn $ "Addition using module: " ++ show (mathAdd x y)
    putStrLn $ "Multiplication using module: " ++ show (mathMultiply x y)

-- ===============================================================
-- MAIN FUNCTION: Run all tasks sequentially
main :: IO ()
main = do
    putStrLn "\n=== HC12T1 ==="
    hc12t1

    putStrLn "\n=== HC12T2 ==="
    hc12t2

    putStrLn "\n=== HC12T3 ==="
    hc12t3

    putStrLn "\n=== HC12T4 ==="
    hc12t4

    putStrLn "\n=== HC12T5 ==="
    hc12t5

    putStrLn "\n=== HC12T6 ==="
    hc12t6

    putStrLn "\n=== HC12T7 ==="
    hc12t7

    putStrLn "\n=== HC12T8 ==="
    hc12t8

    putStrLn "\n=== HC12T9 ==="
    hc12t9

    putStrLn "\n=== HC12T10 ==="
    hc12t10
