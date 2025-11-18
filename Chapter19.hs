module Main where

import Control.Applicative
import Control.Monad
import Control.Concurrent (threadDelay)

--------------------------------------------------------------------------------
-- HC19T1: Applicative Instance for Pair
--------------------------------------------------------------------------------

data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

demoPair :: Pair Int
demoPair = pure (+1) <*> Pair 1 2

--------------------------------------------------------------------------------
-- HC19T2: addThreeApplicative
--------------------------------------------------------------------------------

addThreeApplicative :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
addThreeApplicative a b c = (\x y z -> x + y + z) <$> a <*> b <*> c

--------------------------------------------------------------------------------
-- HC19T3: safeProduct
--------------------------------------------------------------------------------

safeProduct :: [Maybe Int] -> Maybe Int
safeProduct xs = fmap product (sequenceA xs)

--------------------------------------------------------------------------------
-- HC19T4: liftAndMultiply
--------------------------------------------------------------------------------

liftAndMultiply :: Applicative f => (Int -> Int -> Int) -> f Int -> f Int -> f Int
liftAndMultiply = liftA2

--------------------------------------------------------------------------------
-- HC19T5: applyEffects
--------------------------------------------------------------------------------

applyEffects :: (IO Int, IO Int) -> IO ()
applyEffects (ioa,iob) = do
  x <- ioa
  putStrLn ("First: " ++ show x)
  y <- iob
  putStrLn ("Second: " ++ show y)
  putStrLn ("Sum: " ++ show (x+y))

--------------------------------------------------------------------------------
-- HC19T6: repeatEffect
--------------------------------------------------------------------------------

repeatEffect :: IO () -> IO ()
repeatEffect action = forever action

--------------------------------------------------------------------------------
-- HC19T7: conditionalPrint
--------------------------------------------------------------------------------

conditionalPrint :: Bool -> String -> IO ()
conditionalPrint cond msg = when cond (putStrLn msg)

--------------------------------------------------------------------------------
-- HC19T8: discardSecond
--------------------------------------------------------------------------------

discardSecond :: Applicative f => f a -> f b -> f a
discardSecond = (<*)

--------------------------------------------------------------------------------
-- HC19T9: pureAndApply
--------------------------------------------------------------------------------

pureAndApply :: Maybe Int
pureAndApply = pure (+1) <*> Just 3

--------------------------------------------------------------------------------
-- HC19T10: combineResults
--------------------------------------------------------------------------------

combineResults :: Either String Int -> Either String Int -> Either String Int
combineResults = liftA2 (+)

--------------------------------------------------------------------------------
-- HC19T11: Applicative for Wrapper
--------------------------------------------------------------------------------

newtype Wrapper a = Wrapper a deriving (Show, Eq)

instance Functor Wrapper where
  fmap f (Wrapper x) = Wrapper (f x)

instance Applicative Wrapper where
  pure = Wrapper
  (Wrapper f) <*> (Wrapper x) = Wrapper (f x)

--------------------------------------------------------------------------------
-- HC19T12: sumThreeApplicative
--------------------------------------------------------------------------------

sumThreeApplicative
  :: Either String Int
  -> Either String Int
  -> Either String Int
  -> Either String Int
sumThreeApplicative a b c = (\x y z -> x + y + z) <$> a <*> b <*> c

--------------------------------------------------------------------------------
-- HC19T13: whenApplicative
--------------------------------------------------------------------------------

whenApplicative :: Applicative f => Bool -> f () -> f ()
whenApplicative cond action = if cond then action else pure ()

--------------------------------------------------------------------------------
-- HC19T14: replicateEffect
-----------------------------
