module Main where

-- HC17T1
data Severity = Low | Medium | High | Critical
  deriving (Show, Eq, Ord)

instance Semigroup Severity where
  (<>) = max

-- HC17T2
newtype Min a = Min { getMin :: a } deriving (Show)
newtype Max a = Max { getMax :: a } deriving (Show)

instance (Ord a) => Semigroup (Min a) where
  Min x <> Min y = Min (min x y)

instance (Ord a) => Semigroup (Max a) where
  Max x <> Max y = Max (max x y)

-- HC17T3
instance Monoid Severity where
  mempty = Low

-- HC17T4
newtype Sum a = Sum { getSum :: a } deriving (Show)

instance Num a => Semigroup (Sum a) where
  Sum x <> Sum y = Sum (x + y)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0

-- HC17T5
combineLists :: [Int] -> [Int] -> [Int]
combineLists = (<>)

-- HC17T6
maxSeverity :: [Severity] -> Severity
maxSeverity = mconcat

-- HC17T7
newtype Product a = Product { getProduct :: a } deriving (Show)

instance Num a => Semigroup (Product a) where
  Product x <> Product y = Product (x * y)

instance Num a => Monoid (Product a) where
  mempty = Product 1

multiplyProducts :: Num a => [Product a] -> Product a
multiplyProducts = mconcat

-- HC17T8
foldWithSemigroup :: Semigroup a => [a] -> a
foldWithSemigroup = foldr1 (<>)

-- HC17T9
data Config = Config
  { loggingLevel :: Int
  , timeout :: Int
  , retries :: Int
  } deriving (Show)

instance Semigroup Config where
  c1 <> c2 = Config
    { loggingLevel = max (loggingLevel c1) (loggingLevel c2)
    , timeout = min (timeout c1) (timeout c2)
    , retries = max (retries c1) (retries c2)
    }

-- HC17T10
instance Monoid Config where
  mempty = Config 0 maxBound 0

main :: IO ()
main = do
  print (Low <> Medium)
  print (Min 5 <> Min 3)
  print (Max 2 <> Max 8)
  print (mconcat [Low, Medium, High])
  print (mconcat [Sum 5, Sum 10, Sum 15])
  print (combineLists [1,2,3] [4,5,6])
  print (maxSeverity [Low, Medium, High, Critical, Medium])
  print (multiplyProducts [Product 2, Product 3, Product 4])
  print (foldWithSemigroup ["Hello ", "World", "!"])
  let c1 = Config 2 100 3
  let c2 = Config 4 80 5
  print (c1 <> c2)
  let defaultConfig = mempty
  let userConfig = Config 3 120 4
  print (defaultConfig <> userConfig)
