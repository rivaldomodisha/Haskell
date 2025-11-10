module Main where
import Data.Char (toLower)

-- HC18T1: mapToLower Function with fmap
mapToLower :: String -> String
mapToLower = fmap toLower

-- HC18T2: Functor Instance for Tree
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

-- HC18T3: incrementTreeValues Function
incrementTreeValues :: Num a => Tree a -> Tree a
incrementTreeValues = fmap (+1)

-- HC18T4: mapToBits Function
mapToBits :: [Bool] -> [Char]
mapToBits = fmap (\x -> if x then '1' else '0')

-- HC18T5: Functor Instance for Either (use built-in instance)
-- The Functor instance for Either is already defined in Prelude/Data.Either.

-- HC18T6: applyToMaybe Function
applyToMaybe :: (a -> b) -> Maybe a -> Maybe b
applyToMaybe = fmap

-- HC18T7: fmapTuple Function
fmapTuple :: (b -> c) -> (a, b) -> (a, c)
fmapTuple = fmap

-- HC18T8: identityLawCheck Function
identityLawCheck :: (Functor f, Eq (f a)) => f a -> Bool
identityLawCheck x = fmap id x == x

-- HC18T9: compositionLawCheck Function
compositionLawCheck :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
compositionLawCheck f g x = fmap (g . f) x == (fmap g . fmap f) x

-- HC18T10: nestedFmap Function
nestedFmap :: (a -> b) -> [[a]] -> [[b]]
nestedFmap = fmap . fmap

-- Main
main :: IO ()
main = do
  print (mapToLower "HELLO")
  let tree = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)
  print (incrementTreeValues tree)
  print (mapToBits [True, False, True])
  print (applyToMaybe (+2) (Just 5))
  print (fmapTuple (+10) ("Age", 20))
  print (identityLawCheck (Just 10))
  print (compositionLawCheck (+1) (*2) (Just 3))
  print (nestedFmap toLower ["HELLO", "WORLD"])
  print (fmap (+3) (Right 7 :: Either String Int))
  print (fmap (+3) (Left "Error" :: Either String Int))
