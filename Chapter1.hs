import Data.List (sortBy)
import Data.Ord (comparing)

-- HC1T1 - Task 1: Function Composition
double :: Int -> Int
double x = x * 2

increment :: Int -> Int
increment x = x + 1

doubleThenIncrement :: Int -> Int
doubleThenIncrement = increment . double


-- HC1T2 - Task 2: Pure Function Example
circleArea :: Floating a => a -> a
circleArea r = pi * r * r


-- HC1T3 - Task 3: Checking if a Number is Greater than 18
greaterThan18 :: Int -> Bool
greaterThan18 x = x > 18


-- HC1T4 - Task 4: Composing a Function to Process Player Data
extractPlayers :: [(String, Int)] -> [String]
extractPlayers players = [name | (name, _) <- players]

sortByScore :: [(String, Int)] -> [(String, Int)]
sortByScore = sortBy (flip (comparing snd))

topThree :: [(String, Int)] -> [(String, Int)]
topThree players = take 3 players

getTopThreePlayers :: [(String, Int)] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore


-- HC1T5 - Task 5: Laziness in Haskell
infiniteNumbers :: [Int]
infiniteNumbers = [1..]

firstN :: Int -> [Int]
firstN n = take n infiniteNumbers


-- HC1T6 - Task 6: Using Type Signatures
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y


-- HC1T7 - Task 7: Converting Fahrenheit to Celsius
fToC :: Floating a => a -> a
fToC f = (f - 32) * 5 / 9


-- HC1T8 - Task 8: Higher-Order Functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)


-- MAIN FUNCTION FOR TESTING
main :: IO ()
main = do
    putStrLn "HC1T1 - doubleThenIncrement 3:"
    print (doubleThenIncrement 3)   -- 7

    putStrLn "\nHC1T2 - circleArea 5:"
    print (circleArea 5)            -- 78.53981633974483

    putStrLn "\nHC1T3 - greaterThan18 20:"
    print (greaterThan18 20)        -- True

    putStrLn "\nHC1T4 - getTopThreePlayers:"
    let players = [("Alice", 50), ("Bob", 70), ("Carol", 30), ("Dave", 90)]
    print (getTopThreePlayers players) -- ["Dave","Bob","Alice"]

    putStrLn "\nHC1T5 - firstN 5:"
    print (firstN 5)                -- [1,2,3,4,5]

    putStrLn "\nHC1T6 - addNumbers 7 8:"
    print (addNumbers 7 8)          -- 15

    putStrLn "\nHC1T7 - fToC 212:"
    print (fToC 212)                -- 100.0

    putStrLn "\nHC1T8 - applyTwice (*2) 3:"
    print (applyTwice (*2) 3)       -- 12
