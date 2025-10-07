data Color = Red | Green | Blue
  deriving (Show, Enum, Bounded)

instance Eq Color where
  Red   == Red   = True
  Green == Green = True
  Blue  == Blue  = True
  _     == _     = False

instance Ord Color where
  compare Red Green  = LT
  compare Red Blue   = LT
  compare Green Blue = LT
  compare a b
    | a == b    = EQ
    | otherwise = GT

compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues x y
  | x >= y    = x
  | otherwise = y

data Shape = Circle Double | Rectangle Double Double
  deriving (Eq)

instance Show Shape where
  show (Circle r) = "Circle " ++ show r
  show (Rectangle w h) = "Rectangle " ++ show w ++ " " ++ show h

instance Read Shape where
  readsPrec _ input =
    case words input of
      ["Circle", r] ->
        [(Circle (read r), "")]
      ["Rectangle", w, h] ->
        [(Rectangle (read w) (read h), "")]
      _ -> []

squareArea :: Num a => a -> a
squareArea s = s * s

circleCircumference :: (Floating a, Integral b) => b -> a
circleCircumference r = 2 * pi * fromIntegral r

nextColor :: Color -> Color
nextColor c
  | c == maxBound = minBound
  | otherwise     = succ c

parseShape :: String -> Maybe Shape
parseShape s =
  case reads s of
    [(shape, "")] -> Just shape
    _             -> Nothing

class Describable a where
  describe :: a -> String

instance Describable Bool where
  describe True  = "This is True."
  describe False = "This is False."

instance Describable Shape where
  describe (Circle r) = "A circle with radius " ++ show r
  describe (Rectangle w h) = "A rectangle with width " ++ show w ++ " and height " ++ show h

instance Ord Shape where
  compare (Circle r1) (Circle r2) = compare r1 r2
  compare (Rectangle w1 h1) (Rectangle w2 h2) = compare (w1 * h1) (w2 * h2)
  compare (Circle _) (Rectangle _ _) = LT
  compare (Rectangle _ _) (Circle _) = GT

describeAndCompare :: (Describable a, Ord a) => a -> a -> String
describeAndCompare x y
  | x >= y    = describe x
  | otherwise = describe y

main :: IO ()
main = do
  putStrLn "=== HC7T1 & HC7T2: Eq and Ord for Color ==="
  print (Red == Blue)
  print (Red < Green)
  print (compareValues Blue Green)

  putStrLn "\n=== HC7T3: compareValues ==="
  print (compareValues 10 5)
  print (compareValues 3 8)

  putStrLn "\n=== HC7T4: Show & Read for Shape ==="
  let c1 = Circle 5.0
  let r1 = Rectangle 3.0 4.0
  print c1
  print r1
  print (read "Circle 2.5" :: Shape)

  putStrLn "\n=== HC7T5: squareArea ==="
  print (squareArea 4)

  putStrLn "\n=== HC7T6: circleCircumference ==="
  print (circleCircumference 7)

  putStrLn "\n=== HC7T7: nextColor ==="
  print (nextColor Red)
  print (nextColor Green)
  print (nextColor Blue)

  putStrLn "\n=== HC7T8: parseShape ==="
  print (parseShape "Rectangle 2.0 3.0")
  print (parseShape "Triangle 3.0 4.0")

  putStrLn "\n=== HC7T9: Describable Instances ==="
  putStrLn (describe True)
  putStrLn (describe c1)

  putStrLn "\n=== HC7T10: describeAndCompare ==="
  putStrLn (describeAndCompare (Circle 3) (Circle 5))
  putStrLn (describeAndCompare (Rectangle 2 4) (Rectangle 4 2))
