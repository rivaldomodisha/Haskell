{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

-- Haskell Chapter 14 Practical Tasks: Cabal Projects and Advanced Features
-- Combined version for HC14T1–HC14T10 in one file

module Main where

import System.Random (randomRIO)
import Data.Char (isLetter)
import Data.List (group, sort)
import qualified Data.List as L
import qualified Data.Map as M

-- ===============================
-- HC14T3: NumericUnderscores Extension
-- ===============================
printLargeNumbers :: IO ()
printLargeNumbers = do
    let population = 60_000_000
        distance   = 384_400_000
    putStrLn $ "Population: " ++ show population
    putStrLn $ "Distance to Moon (m): " ++ show distance

-- ===============================
-- HC14T4: TypeApplications Extension
-- ===============================
stringToInt :: String -> Int
stringToInt = read @Int

-- ===============================
-- HC14T5: Custom Data Type & Pattern Matching with @
-- ===============================
data Result a = Success a | Failure String deriving (Show)

describeResult :: Result Int -> String
describeResult r@(Success _) = "Success: " ++ show r
describeResult r@(Failure _) = "Failure: " ++ show r

-- ===============================
-- HC14T8: Character Frequency Function
-- ===============================
counts :: String -> [(Char, Int)]
counts str =
    map (\xs -> (head xs, length xs)) .
    group . sort .
    filter isLetter $ str

-- ===============================
-- HC14T9: Qualified & Renamed Imports
-- ===============================
demoQualifiedImports :: IO ()
demoQualifiedImports = do
    let lst = [5,1,2,9,4]
    putStrLn $ "Original list: " ++ show lst
    putStrLn $ "Sorted list (qualified): " ++ show (L.sort lst)
    putStrLn $ "List to Map: " ++ show (M.fromList (zip lst [1..]))

demoRenamedImports :: IO ()
demoRenamedImports = do
    let xs = [10, 20, 30]
    putStrLn $ "Sum via renamed List module: " ++ show (L.sum xs)
    putStrLn $ "Example Map: " ++ show (M.fromList [(1,"A"),(2,"B")])

-- ===============================
-- MAIN FUNCTION — Combines all Tasks
-- ===============================
main :: IO ()
main = do
    putStrLn "=== HC14T1: Hello Cabal ==="
    putStrLn "Hello, Cabal!"

    putStrLn "\n=== HC14T2: Random Number ==="
    num <- randomRIO (1, 100) :: IO Int
    putStrLn $ "Random number between 1 and 100: " ++ show num

    putStrLn "\n=== HC14T3: NumericUnderscores ==="
    printLargeNumbers

    putStrLn "\n=== HC14T4: TypeApplications ==="
    putStrLn $ "Converted string '456' -> Int: " ++ show (stringToInt "456")

    putStrLn "\n=== HC14T5: Custom Data Type and Pattern Matching ==="
    putStrLn $ describeResult (Success 42)
    putStrLn $ describeResult (Failure "Oops! Something went wrong")

    putStrLn "\n=== HC14T6: Project Structure Simulation ==="
    putStrLn "Simulating src and app structure in one file for simplicity."

    putStrLn "\n=== HC14T7: Library Component Demonstration ==="
    putStrLn "This section would be part of a library in a Cabal project."

    putStrLn "\n=== HC14T8: Character Frequency Function ==="
    print $ counts "hello"

    putStrLn "\n=== HC14T9: Qualified and Renamed Imports ==="
    demoQualifiedImports
    demoRenamedImports

    putStrLn "\n=== HC14T10: Cabal Test Suite (Simulated) ==="
    putStrLn "Test simulation: counts 'hello' == [('e',1),('h',1),('l',2),('o',1)]"
    print $ counts "hello" == [('e',1),('h',1),('l',2),('o',1)]

    putStrLn "\n✅ All Haskell Chapter 14 Tasks Completed Successfully!"
