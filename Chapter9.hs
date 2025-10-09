-- HC9T1: Define a Parametric Type Synonym
type Entity a = (String, a)


-- HC9T2: Implement a Parametric Data Type
data Box a = Empty | Has a deriving (Show)


-- HC9T3: Function to Add Values in a Box
addN :: Num a => a -> Box a -> Box a
addN n Empty = Empty
addN n (Has x) = Has (x + n)


-- HC9T4: Extract a Value from a Box
extract :: a -> Box a -> a
extract def Empty = def
extract _ (Has x) = x


-- HC9T5: Parametric Data Type with Record Syntax
data Shape a
  = Circle { color :: a, radius :: Double }
  | Rectangle { color :: a, width :: Double, height :: Double }
  deriving (Show)


-- HC9T6: Recursive Data Type for Tweets
data Tweet = Tweet
  { content :: String
  , likes :: Int
  , comments :: [Tweet]
  } deriving (Show)


-- HC9T7: Engagement Function for Tweets
engagement :: Tweet -> Int
engagement (Tweet _ l cs) = l + sum (map engagement cs)


-- HC9T8: Recursive Sequence Data Type
data Sequence a = Nil | Node a (Sequence a) deriving (Show)


-- HC9T9: Check for Element in a Sequence
elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ Nil = False
elemSeq x (Node y ys)
  | x == y    = True
  | otherwise = elemSeq x ys


-- HC9T10: Binary Search Tree Data Type
data BST a = EmptyTree | NodeBST a (BST a) (BST a) deriving (Show)


-- MAIN FUNCTION: Run All Examples
main :: IO ()
main = do
  putStrLn "=== HC9T1: Entity Example ==="
  let user :: Entity Int
      user = ("Alice", 101)
  print user

  putStrLn "\n=== HC9T2–HC9T4: Box Examples ==="
  print (Has 42)
  print (addN 5 (Has 10))         -- Has 15
  print (addN 3 Empty)            -- Empty
  print (extract 0 (Has 7))       -- 7
  print (extract 100 Empty)       -- 100

  putStrLn "\n=== HC9T5: Shape Examples ==="
  print (Circle { color = "Red", radius = 5.0 })
  print (Rectangle { color = "Blue", width = 3.0, height = 4.0 })

  putStrLn "\n=== HC9T6–HC9T7: Tweet and Engagement ==="
  let reply1 = Tweet "Cool!" 2 []
  let reply2 = Tweet "Great job!" 4 []
  let mainTweet = Tweet "Check out my Haskell code" 10 [reply1, reply2]
  print mainTweet
  putStrLn ("Total Engagement: " ++ show (engagement mainTweet))

  putStrLn "\n=== HC9T8–HC9T9: Sequence Examples ==="
  let seq1 = Node 1 (Node 2 (Node 3 Nil))
  print seq1
  print (elemSeq 2 seq1)          -- True
  print (elemSeq 5 seq1)          -- False

  putStrLn "\n=== HC9T10: Binary Search Tree Example ==="
  let tree = NodeBST 10 (NodeBST 5 EmptyTree EmptyTree) (NodeBST 15 EmptyTree EmptyTree)
  print tree
