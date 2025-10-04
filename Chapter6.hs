-- HC6T1: Factorial (Recursive)
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- HC6T2: Fibonacci (Recursive)
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- HC6T3: Sum of Elements Using foldr
sumFoldr :: [Int] -> Int
sumFoldr = foldr (+) 0

-- HC6T4: Product of Elements Using foldl
productFoldl :: [Int] -> Int
productFoldl = foldl (*) 1

-- HC6T5: Reverse a List (Recursive)
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- HC6T6: Element Exists in List
elementExists :: Eq a => a -> [a] -> Bool
elementExists _ [] = False
elementExists e (x:xs) = (e == x) || elementExists e xs

-- HC6T7: List Length
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

-- HC6T8: Filter Even Numbers
filterEvens :: [Int] -> [Int]
filterEvens = filter even

-- HC6T9: Map Implementation
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

-- HC6T10: Digits of a Number (Recursive)
digits :: Int -> [Int]
digits n
    | n < 10    = [n]
    | otherwise = digits (n `div` 10) ++ [n `mod` 10]

-- MAIN FUNCTION
main :: IO ()
main = do
    putStrLn "HC6T1 - Factorial (Recursive):"
    print (factorial 5)

    putStrLn "\nHC6T2 - Fibonacci (Recursive):"
    print (fibonacci 8)

    putStrLn "\nHC6T3 - Sum Using foldr:"
    print (sumFoldr [1,2,3,4,5])

    putStrLn "\nHC6T4 - Product Using foldl:"
    print (productFoldl [1,2,3,4,5])

    putStrLn "\nHC6T5 - Reverse List (Recursive):"
    print (reverseList [1,2,3,4,5])

    putStrLn "\nHC6T6 - Element Exists in List:"
    print (elementExists 3 [1,2,3,4,5])
    print (elementExists 9 [1,2,3,4,5])

    putStrLn "\nHC6T7 - List Length:"
    print (listLength [10,20,30,40])

    putStrLn "\nHC6T8 - Filter Even Numbers:"
    print (filterEvens [1..10])

    putStrLn "\nHC6T9 - Map Implementation:"
    print (myMap (*2) [1,2,3,4])

    putStrLn "\nHC6T10 - Digits of a Number:"
    print (digits 12345)
