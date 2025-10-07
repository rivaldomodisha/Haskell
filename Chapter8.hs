-- Haskell Chapter 8 Practical Tasks: Data Types, Synonyms, and Records

-- ======================
-- HC8T1: Type Synonyms and Basic Function
-- ======================

type Address = String
type Value = Int

generateTx :: Address -> Address -> Value -> String
generateTx fromAddr toAddr val =
  "From: " ++ fromAddr ++ ", To: " ++ toAddr ++ ", Value: " ++ show val

task1 :: IO ()
task1 = putStrLn (generateTx "AliceAddr" "BobAddr" 100)


-- ======================
-- HC8T2: New Types and Data Constructors
-- ======================

data PaymentMethod = Cash | Card | Cryptocurrency deriving Show

data Person = Person String (String, Int) PaymentMethod deriving Show

bob :: Person
bob = Person "Bob" ("123 Main Street", 101) Cash

task2 :: IO ()
task2 = print bob


-- ======================
-- HC8T3: Algebraic Data Types and Functions
-- ======================

data Shape = Circle Float | Rectangle Float Float deriving Show

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

circleAreaExample :: Float
circleAreaExample = area (Circle 5)

rectangleAreaExample :: Float
rectangleAreaExample = area (Rectangle 10 5)

task3 :: IO ()
task3 = do
  print circleAreaExample
  print rectangleAreaExample


-- ======================
-- HC8T4: Record Syntax for Employee
-- ======================

data Employee = Employee
  { name :: String
  , experienceInYears :: Float
  } deriving Show

richard :: Employee
richard = Employee { name = "Richard", experienceInYears = 7.5 }

task4 :: IO ()
task4 = print richard


-- ======================
-- HC8T5: Record Syntax for Person
-- ======================

data PersonRecord = PersonRecord
  { pname :: String
  , age :: Int
  , isEmployed :: Bool
  } deriving Show

person1 :: PersonRecord
person1 = PersonRecord "Alice" 25 True

person2 :: PersonRecord
person2 = PersonRecord "John" 30 False

task5 :: IO ()
task5 = do
  print person1
  print person2


-- ======================
-- HC8T6: Record Syntax for Shape Variants
-- ======================

data ShapeRecord
  = CircleRecord
      { center :: (Float, Float)
      , color :: String
      , radius :: Float
      }
  | RectangleRecord
      { center :: (Float, Float)
      , color :: String
      , width :: Float
      , height :: Float
      }
  deriving Show

circle1 :: ShapeRecord
circle1 = CircleRecord { center = (0, 0), color = "Red", radius = 5 }

rectangle1 :: ShapeRecord
rectangle1 = RectangleRecord { center = (5, 5), color = "Blue", width = 10, height = 4 }

task6 :: IO ()
task6 = do
  print circle1
  print rectangle1


-- ======================
-- HC8T7: Data Types and Describing Animals
-- ======================

data Animal = Dog String | Cat String deriving Show

describeAnimal :: Animal -> String
describeAnimal (Dog name) = "This is a dog named " ++ name
describeAnimal (Cat name) = "This is a cat named " ++ name

dog1 :: Animal
dog1 = Dog "Buddy"

cat1 :: Animal
cat1 = Cat "Misty"

task7 :: IO ()
task7 = do
  putStrLn (describeAnimal dog1)
  putStrLn (describeAnimal cat1)


-- ======================
-- HC8T8: Type Synonyms and Greeting Function
-- ======================

type Name = String
type Age = Int

greet :: Name -> Age -> String
greet n a = "Hello, my name is " ++ n ++ " and I am " ++ show a ++ " years old."

task8 :: IO ()
task8 = putStrLn (greet "Rivaldo" 22)


-- ======================
-- HC8T9: Record Type and Transaction Function
-- ======================

data Transaction = Transaction
  { from :: Address
  , to :: Address
  , amount :: Value
  , transactionId :: String
  } deriving Show

createTransaction :: Address -> Address -> Value -> String
createTransaction fromAddr toAddr val =
  let tx = Transaction { from = fromAddr, to = toAddr, amount = val, transactionId = "TX12345" }
  in transactionId tx

task9 :: IO ()
task9 = putStrLn (createTransaction "AddrA" "AddrB" 500)


-- ======================
-- HC8T10: Deriving Show for Book
-- ======================

data Book = Book
  { title :: String
  , author :: String
  , year :: Int
  } deriving Show

book1 :: Book
book1 = Book { title = "Learn You a Haskell", author = "Miran Lipovaca", year = 2011 }

task10 :: IO ()
task10 = print book1


-- ======================
-- MAIN FUNCTION
-- ======================

main :: IO ()
main = do
  putStrLn "=== HC8T1 ==="
  task1

  putStrLn "\n=== HC8T2 ==="
  task2

  putStrLn "\n=== HC8T3 ==="
  task3

  putStrLn "\n=== HC8T4 ==="
  task4

  putStrLn "\n=== HC8T5 ==="
  task5

  putStrLn "\n=== HC8T6 ==="
  task6

  putStrLn "\n=== HC8T7 ==="
  task7

  putStrLn "\n=== HC8T8 ==="
  task8

  putStrLn "\n=== HC8T9 ==="
  task9

  putStrLn "\n=== HC8T10 ==="
  task10
