import Numeric (showHex)

checkNumber :: Int -> String
checkNumber n = if n > 0 then "Positive"
                else if n < 0 then "Negative"
                else "Zero"

grade :: Int -> String
grade n
    | n >= 90   = "A"
    | n >= 80   = "B"
    | n >= 70   = "C"
    | n >= 60   = "D"
    | otherwise = "F"

rgbToHex :: (Int, Int, Int) -> String
rgbToHex (r,g,b) =
    let hr = toHex r
        hg = toHex g
        hb = toHex b
    in "#" ++ hr ++ hg ++ hb
  where
    toHex x =
        let hx = showHex x ""
        in if length hx == 1 then '0':hx else hx

triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c = let s = (a + b + c)/2
                     in sqrt (s * (s - a) * (s - b) * (s - c))

triangleType :: Float -> Float -> Float -> String
triangleType a b c
    | a == b && b == c = "Equilateral"
    | a == b || b == c || a == c = "Isosceles"
    | otherwise = "Scalene"

isLeapYear :: Int -> Bool
isLeapYear year = if year `mod` 400 == 0 then True
                  else if year `mod` 100 == 0 then False
                  else if year `mod` 4 == 0 then True
                  else False

season :: Int -> String
season m
    | m == 12 || m == 1 || m == 2 = "Winter"
    | m >= 3 && m <= 5 = "Spring"
    | m >= 6 && m <= 8 = "Summer"
    | m >= 9 && m <= 11 = "Autumn"
    | otherwise = "Invalid month"

bmiCategory :: Float -> Float -> String
bmiCategory weight height
    | bmi < 18.5 = "Underweight"
    | bmi < 25   = "Normal"
    | bmi < 30   = "Overweight"
    | otherwise  = "Obese"
    where bmi = weight / (height^2)

maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c = let m1 = max a b
                       m2 = max m1 c
                   in m2

isPalindrome :: String -> Bool
isPalindrome str
    | length str <= 1 = True
    | head str == last str = isPalindrome (init (tail str))
    | otherwise = False

main :: IO ()
main = do
    putStrLn "HC3T1:"
    print (checkNumber 5)
    print (checkNumber (-3))
    print (checkNumber 0)

    putStrLn "\nHC3T2:"
    print (grade 95)
    print (grade 72)
    print (grade 50)

    putStrLn "\nHC3T3:"
    print (rgbToHex (255, 0, 127))
    print (rgbToHex (0, 255, 64))

    putStrLn "\nHC3T4:"
    print (triangleArea 3 4 5)
    print (triangleArea 7 8 9)

    putStrLn "\nHC3T5:"
    print (triangleType 3 3 3)
    print (triangleType 5 5 8)
    print (triangleType 6 7 8)

    putStrLn "\nHC3T6:"
    print (isLeapYear 2000)
    print (isLeapYear 1900)
    print (isLeapYear 2024)

    putStrLn "\nHC3T7:"
    print (season 3)
    print (season 7)
    print (season 11)

    putStrLn "\nHC3T8:"
    print (bmiCategory 70 1.75)
    print (bmiCategory 90 1.8)

    putStrLn "\nHC3T9:"
    print (maxOfThree 10 20 15)
    print (maxOfThree 5 25 10)

    putStrLn "\nHC3T10:"
    print (isPalindrome "racecar")
    print (isPalindrome "haskell")
    print (isPalindrome "madam")
