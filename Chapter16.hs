import Data.Char (toUpper)
import Data.List (group, sort)

--------------------------------------
-- HC16T1: Reverse a String
--------------------------------------
reverseString :: String -> String
reverseString str = reverse str

main :: IO ()
main = do
putStrLn "HC16T1: Reverse a String"
print (reverseString "haskell")

--------------------------------------
-- HC16T2: Palindrome Checker
--------------------------------------
isPalindrome :: String -> Bool
isPalindrome str = str == reverse str

main :: IO ()
main = do
putStrLn "HC16T2: Palindrome Checker"
print (isPalindrome "level")
print (isPalindrome "hello")

--------------------------------------
-- HC16T3: Factorial
--------------------------------------
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

main :: IO ()
main = do
   putStrLn "HC16T3: Factorial"
   print (factorial 5)

--------------------------------------
-- HC16T4: Filter Even Numbers
--------------------------------------
filterEven :: [Int] -> [Int]
filterEven xs = filter even xs

main :: IO ()
main = do
   putStrLn "HC16T4: Filter Even Numbers"
   print (filterEven [1..10])

--------------------------------------
-- HC16T5: Uppercase String
--------------------------------------
toUppercase :: String -> String
toUppercase str = map toUpper str

main :: IO ()
main = do
   putStrLn "HC16T5: Uppercase String"
   print (toUppercase "haskell")

--------------------------------------
-- HC16T6: nth Fibonacci Number
--------------------------------------
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

main :: IO ()
main = do
   putStrLn "HC16T6: nth Fibonacci Number"
   print (fibonacci 10)

--------------------------------------
-- HC16T7: Element Existence in List
--------------------------------------
elementExists :: Eq a => a -> [a] -> Bool
elementExists x xs = x `elem` xs

main :: IO ()
main = do
putStrLn "HC16T7: Element Existence in List"
print (elementExists 3 [1,2,3,4,5])
print (elementExists 9 [1,2,3,4,5])

--------------------------------------
-- HC16T8: Insertion Sort
--------------------------------------
insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
  where
    insert y [] = [y]
    insert y (z:zs)
      | y <= z    = y:z:zs
      | otherwise = z : insert y zs

main :: IO ()
main = do
   putStrLn "HC16T8: Insertion Sort"
   print (insertionSort [5,2,8,1,3])

--------------------------------------
-- HC16T9: Remove Duplicates from List
--------------------------------------
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
  | x `elem` xs = removeDuplicates xs
  | otherwise   = x : removeDuplicates xs

main :: IO ()
main = do
putStrLn "HC16T9: Remove Duplicates from List"
print (removeDuplicates [1,2,2,3,3,4,5,5])

--------------------------------------
-- HC16T10: Character Frequency in String
--------------------------------------
charFrequency :: String -> [(Char, Int)]
charFrequency str = [(head g, length g) | g <- group (sort str)]

main :: IO ()
main = do
  putStrLn "HC16T10: Character Frequency in String"
  print (charFrequency "haskell")
