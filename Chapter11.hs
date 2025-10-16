import Data.Char (toUpper)

-- ===============================================================
-- HC11T1: Greet the User
greetUser :: IO ()
greetUser = do
    let name = "Rivaldo"  -- simulated input
    putStrLn $ "Hello, " ++ name ++ "!"

-- ===============================================================
-- HC11T2: Count Characters in a Line
countChars :: IO ()
countChars = do
    let line = "Hello World!"  -- simulated input
    putStrLn $ "Number of characters: " ++ show (length line)

-- ===============================================================
-- HC11T3: Double a Number
doubleNumber :: IO ()
doubleNumber = do
    let n = 42  -- simulated input
    putStrLn $ "Double of the number: " ++ show (n * 2)

-- ===============================================================
-- HC11T4: Concatenate Two Lines
concatTwoLines :: IO ()
concatTwoLines = do
    let line1 = "Hello, "
    let line2 = "World!"
    putStrLn $ "Concatenated: " ++ line1 ++ line2

-- ===============================================================
-- HC11T5: Repeat Until "quit"
repeatUntilQuit :: IO ()
repeatUntilQuit = do
    let inputs = ["First", "Second", "quit"]  -- simulated input sequence
    putStrLn "Simulated repeat until 'quit':"
    mapM_ process inputs
  where
    process input =
        if input == "quit"
            then putStrLn "Exiting..."
            else putStrLn $ "You entered: " ++ input

-- ===============================================================
-- HC11T6: Uppercase Converter
uppercaseConverter :: IO ()
uppercaseConverter = do
    let line = "hello world"  -- simulated input
    putStrLn $ map toUpper line

-- ===============================================================
-- HC11T7: User Options
userOptions :: IO ()
userOptions = do
    let choice = "1"  -- simulated choice
    case choice of
        "1" -> putStrLn "Hello!"
        "2" -> putStrLn "Goodbye!"
        _   -> putStrLn "Invalid option"

-- ===============================================================
-- HC11T8: Even or Odd Checker
evenOrOdd :: IO ()
evenOrOdd = do
    let n = 7  -- simulated input
    if even n
        then putStrLn "The number is even."
        else putStrLn "The number is odd."

-- ===============================================================
-- HC11T9: Sum Two Numbers
sumTwoNumbers :: IO ()
sumTwoNumbers = do
    let n1 = 10  -- simulated input
    let n2 = 15  -- simulated input
    putStrLn $ "Sum: " ++ show (n1 + n2)

-- ===============================================================
-- HC11T10: Reverse User Input
reverseInput :: IO ()
reverseInput = do
    let str = "Hello World"  -- simulated input
    putStrLn $ "Reversed: " ++ reverse str

-- ===============================================================
-- MAIN FUNCTION: Call all tasks sequentially
main :: IO ()
main = do
    putStrLn "\n=== HC11T1 ==="
    greetUser

    putStrLn "\n=== HC11T2 ==="
    countChars

    putStrLn "\n=== HC11T3 ==="
    doubleNumber

    putStrLn "\n=== HC11T4 ==="
    concatTwoLines

    putStrLn "\n=== HC11T5 ==="
    repeatUntilQuit

    putStrLn "\n=== HC11T6 ==="
    uppercaseConverter

    putStrLn "\n=== HC11T7 ==="
    userOptions

    putStrLn "\n=== HC11T8 ==="
    evenOrOdd

    putStrLn "\n=== HC11T9 ==="
    sumTwoNumbers

    putStrLn "\n=== HC11T10 ==="
    reverseInput
