-- HC2T1 (type checking examples, values printed)
val1 = 42
val2 = 3.14
val3 = "Haskell"
val4 = 'Z'
val5 = True && False

-- HC2T2
add :: Int -> Int -> Int
add x y = x + y

isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

concatStrings :: String -> String -> String
concatStrings s1 s2 = s1 ++ s2

-- HC2T3
myAge :: Int
myAge = 22

piValue :: Double
piValue = 3.14159

greeting :: String
greeting = "Hello, Haskell!"

isHaskellFun :: Bool
isHaskellFun = True

-- HC2T4
prefix1 = (+) 5 3
prefix2 = (*) 10 4
prefix3 = (&&) True False

infix1 = 7 + 2
infix2 = 6 * 5
infix3 = True && False

-- HC2T5
circleArea :: Float -> Float
circleArea r = pi * r * r

maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c = max a (max b c)

-- HC2T6
smallNumber :: Int
smallNumber = 262

bigNumber :: Integer
bigNumber = 2127

-- HC2T7
expr1 = True && (5 > 2)
expr2 = False || False
expr3 = not False
expr4 = 10 < 5

-- Combined Main
main :: IO ()
main = do
    putStrLn "HC2T1 - Checking Types"
    print val1
    print val2
    print val3
    print val4
    print val5

    putStrLn "\nHC2T2 - Function Signatures"
    print (add 5 7)
    print (isEven 8)
    print (concatStrings "Hi " "there!")

    putStrLn "\nHC2T3 - Immutable Variables"
    print myAge
    print piValue
    print greeting
    print isHaskellFun

    putStrLn "\nHC2T4 - Infix and Prefix Conversion"
    print prefix1
    print prefix2
    print prefix3
    print infix1
    print infix2
    print infix3

    putStrLn "\nHC2T5 - Functions"
    print (circleArea 5)
    print (maxOfThree 3 7 5)
    print (maxOfThree 10 4 8)

    putStrLn "\nHC2T6 - Int vs Integer"
    print smallNumber
    print bigNumber

    putStrLn "\nHC2T7 - Boolean Expressions"
    print expr1
    print expr2
    print expr3
    print expr4
