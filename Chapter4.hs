-- HC4T1 - Task 1: Define a weatherReport Function
weatherReport :: String -> String
weatherReport "sunny"  = "It's a bright and beautiful day!"
weatherReport "rainy"  = "Don't forget your umbrella!"
weatherReport "cloudy" = "A bit gloomy, but no rain yet!"
weatherReport _        = "Weather unknown"

-- HC4T2 - Task 2: Define a dayType Function
dayType :: String -> String
dayType "Saturday" = "It's a weekend!"
dayType "Sunday"   = "It's a weekend!"
dayType "Monday"   = "It's a weekday."
dayType "Tuesday"  = "It's a weekday."
dayType "Wednesday"= "It's a weekday."
dayType "Thursday" = "It's a weekday."
dayType "Friday"   = "It's a weekday."
dayType _          = "Invalid day"

-- HC4T3 - Task 3: Define a gradeComment Function
gradeComment :: Int -> String
gradeComment n
    | n >= 90 && n <= 100 = "Excellent!"
    | n >= 70 && n <= 89  = "Good job!"
    | n >= 50 && n <= 69  = "You passed."
    | n >= 0  && n <= 49  = "Better luck next time."
    | otherwise           = "Invalid grade"

-- HC4T4 & HC4T5 - Task 4 & 5: Rewrite specialBirthday with pattern matching
specialBirthday :: Int -> String
specialBirthday 1  = "Happy 1st Birthday!"
specialBirthday 18 = "Congrats on turning 18!"
specialBirthday 21 = "21 – time to celebrate!"
specialBirthday age = "Happy " ++ show age ++ "th Birthday!"

-- HC4T6 - Task 6: Identify List Contents Using Pattern Matching
whatsInsideThisList :: [a] -> String
whatsInsideThisList []      = "The list is empty."
whatsInsideThisList [x]     = "The list has one element."
whatsInsideThisList [x,y]   = "The list has two elements."
whatsInsideThisList (x:y:_) = "The list has many elements."

-- HC4T7 - Task 7: Ignore Elements in a List
firstAndThird :: [a] -> (a, a)
firstAndThird (x:_:z:_) = (x, z)
firstAndThird _         = error "List must have at least 3 elements."

-- HC4T8 - Task 8: Extract Values from Tuples
describeTuple :: (String, Int, Bool) -> String
describeTuple (name, age, status) =
    name ++ " is " ++ show age ++ " years old. Status: " ++ show status

-- Main function to test everything
main :: IO ()
main = do
    putStrLn "HC4T1:"
    print (weatherReport "sunny")
    print (weatherReport "rainy")
    print (weatherReport "stormy")

    putStrLn "\nHC4T2:"
    print (dayType "Saturday")
    print (dayType "Monday")
    print (dayType "Funday")

    putStrLn "\nHC4T3:"
    print (gradeComment 95)
    print (gradeComment 75)
    print (gradeComment 30)
    print (gradeComment 120)

    putStrLn "\nHC4T4 & HC4T5:"
    print (specialBirthday 1)
    print (specialBirthday 18)
    print (specialBirthday 25)

    putStrLn "\nHC4T6:"
    print (whatsInsideThisList ([] :: [Int]))
    print (whatsInsideThisList [42])
    print (whatsInsideThisList [1,2])
    print (whatsInsideThisList [1,2,3,4])

    putStrLn "\nHC4T7:"
    print (firstAndThird [10,20,30,40])

    putStrLn "\nHC4T8:"
    print (describeTuple ("Alice", 30, True))
