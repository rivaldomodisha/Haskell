-- Haskell Chapter 10 Practical Tasks: Custom Type Classes and Instances
-- ===========================================================

-- HC10T1: ShowSimple Type Class
---------------------------------------------------------------
data PaymentMethod = Cash | CreditCard | Crypto deriving (Show)

class ShowSimple a where
    showSimple :: a -> String

instance ShowSimple PaymentMethod where
    showSimple Cash        = "Cash Payment"
    showSimple CreditCard  = "Credit Card Payment"
    showSimple Crypto      = "Crypto Payment"


-- HC10T2: Summable Type Class
---------------------------------------------------------------
class Summable a where
    sumUp :: [a] -> a

instance Summable Int where
    sumUp = sum


-- HC10T3: Comparable Type Class
---------------------------------------------------------------
-- Note: do NOT derive Eq here because we'll provide a custom Eq instance later.
data Blockchain = Bitcoin | Ethereum | Solana deriving (Show)

class Comparable a where
    compareWith :: a -> a -> Ordering

instance Comparable Blockchain where
    compareWith Bitcoin Bitcoin   = EQ
    compareWith Bitcoin _         = LT
    compareWith Ethereum Bitcoin  = GT
    compareWith Ethereum Ethereum = EQ
    compareWith Ethereum _        = LT
    compareWith Solana Solana     = EQ
    compareWith _ _               = GT


-- HC10T4: Eq Instance for Box
---------------------------------------------------------------
data Box a = Box a deriving (Show)

instance (Eq a) => Eq (Box a) where
    (Box x) == (Box y) = x == y


-- HC10T5: ShowDetailed Type Class
---------------------------------------------------------------
data User = User { userId :: Int, userName :: String } deriving (Show)

class ShowDetailed a where
    showDetailed :: a -> String

instance ShowDetailed User where
    showDetailed (User i name) =
        "User ID: " ++ show i ++ ", Name: " ++ name


-- HC10T6: Mutual Recursion in Eq for Blockchain
---------------------------------------------------------------
instance Eq Blockchain where
    (==) a b = not (a /= b)
    (/=) a b = case (a, b) of
        (Bitcoin, Bitcoin)   -> False
        (Ethereum, Ethereum) -> False
        (Solana, Solana)     -> False
        _                    -> True


-- HC10T7: Convertible Type Class
---------------------------------------------------------------
class Convertible a b where
    convert :: a -> b

instance Convertible PaymentMethod String where
    convert Cash        = "Cash"
    convert CreditCard  = "CreditCard"
    convert Crypto      = "Crypto"


-- HC10T8: AdvancedEq Subclass of Eq
---------------------------------------------------------------
class Eq a => AdvancedEq a where
    compareEquality :: a -> a -> Bool
    compareEquality x y = x == y

instance AdvancedEq Int


-- HC10T9: MinMax Type Class
---------------------------------------------------------------
class MinMax a where
    minValue :: a
    maxValue :: a

instance MinMax Int where
    minValue = minBound :: Int
    maxValue = maxBound :: Int


-- HC10T10: Concatenatable Type Class
---------------------------------------------------------------
class Concatenatable a where
    concatWith :: a -> a -> a

instance Concatenatable String where
    concatWith a b = a ++ b


-- ===========================================================
-- MAIN FUNCTION TO TEST ALL IMPLEMENTATIONS
---------------------------------------------------------------
main :: IO ()
main = do
    putStrLn "=== HC10T1: ShowSimple ==="
    print (showSimple Cash)
    print (showSimple CreditCard)
    print (showSimple Crypto)

    putStrLn "\n=== HC10T2: Summable ==="
    print (sumUp [10, 20, 30 :: Int])

    putStrLn "\n=== HC10T3: Comparable ==="
    print (compareWith Bitcoin Ethereum)
    print (compareWith Solana Ethereum)

    putStrLn "\n=== HC10T4: Eq Instance for Box ==="
    print (Box 5 == Box 5)
    print (Box "hi" == Box "bye")

    putStrLn "\n=== HC10T5: ShowDetailed ==="
    print (showDetailed (User 1 "Rivaldo"))

    putStrLn "\n=== HC10T6: Eq Mutual Recursion ==="
    print (Bitcoin == Ethereum)
    print (Solana == Solana)

    putStrLn "\n=== HC10T7: Convertible ==="
    print (convert Crypto :: String)

    putStrLn "\n=== HC10T8: AdvancedEq ==="
    print (compareEquality (5 :: Int) 5)
    print (compareEquality (5 :: Int) 10)

    putStrLn "\n=== HC10T9: MinMax ==="
    print (minValue :: Int)
    print (maxValue :: Int)

    putStrLn "\n=== HC10T10: Concatenatable ==="
    print (concatWith "Hello, " "World!")

