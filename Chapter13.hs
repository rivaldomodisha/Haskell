-- HC13_Combined.hs
-- Combined Practical Tasks for Chapter 13: Modules and Directories

-- Task 1,2,3,10: System.Directory + Data.List
import System.Directory (listDirectory)
import Data.List (sort, isInfixOf)

-- Task 6: Data.Map
import qualified Data.Map as Map

-- Task 4,5,7: Custom module SumNonEmpty
-- Make sure SumNonEmpty.hs exists in the same folder
import SumNonEmpty (sumNonEmpty)

main :: IO ()
main = do
    putStrLn "=== HC13 Combined Tasks ===\n"

    -- HC13T1: List all files in current directory
    putStrLn "--- Task 1: List Files ---"
    files <- listDirectory "."
    mapM_ putStrLn files
    putStrLn ""

    -- HC13T2: Filter files by substring "hs"
    putStrLn "--- Task 2: Filter Files by substring 'hs' ---"
    let filtered = filter (isInfixOf "hs") files
    mapM_ putStrLn filtered
    putStrLn ""

    -- HC13T3: Sort and return filtered files
    putStrLn "--- Task 3: Sorted Filtered Files ---"
    let filteredSorted = sort filtered
    mapM_ putStrLn filteredSorted
    putStrLn ""

    -- HC13T4 & HC13T5: SumNonEmpty module
    putStrLn "--- Task 4 & 5: Using sumNonEmpty ---"
    putStrLn $ sumNonEmpty [5, 10, 15]  -- non-empty list
    putStrLn $ sumNonEmpty []           -- empty list
    putStrLn ""

    -- HC13T6: Convert filtered file names into a Map
    putStrLn "--- Task 6: Filtered Files as Map ---"
    let fileMap = Map.fromList (zip [1..] filteredSorted)
    print fileMap
    putStrLn ""

    -- HC13T7: Use custom module in main
    putStrLn "--- Task 7: sumNonEmpty in main ---"
    let numbers = [3,6,9]
    putStrLn $ sumNonEmpty numbers
    putStrLn ""

    -- HC13T8: Qualified imports for name conflicts
    putStrLn "--- Task 8: Qualified Imports ---"
    putStrLn "Sorting a list using Data.List.sort:"
    print (sort [4,1,3,2])
    putStrLn "Creating a map using Data.Map:"
    print (Map.fromList [(1,"A"), (2,"B")])
    putStrLn ""

    -- HC13T9: Renaming module namespace
    putStrLn "--- Task 9: Renaming Module Namespace ---"
    import qualified Data.Char as C
    import qualified Data.List as L
    putStrLn "Uppercase 'haskell' using Data.Char:"
    putStrLn (map C.toUpper "haskell")
    putStrLn "Sort numbers using Data.List:"
    print (L.sort [7,3,9,1])
    putStrLn ""

    -- HC13T10: Multi-module main function (search & display sorted files)
    putStrLn "--- Task 10: Multi-module Search ---"
    let results = sort (filter (isInfixOf "hs") files)
    putStrLn "Filtered & sorted files:"
    mapM_ putStrLn results

    putStrLn "\n=== End of HC13 Combined Tasks ==="
