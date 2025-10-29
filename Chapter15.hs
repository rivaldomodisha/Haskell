{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.IO
import Control.Exception
import Text.Read (readMaybe)
import Data.Typeable

-----------------------------------------
-- HC15T1: Handle Exceptions for File Reading and Velocity Calculation
-----------------------------------------
handleFileAndVelocity :: IO ()
handleFileAndVelocity = do
    putStrLn "\n=== HC15T1: File Reading and Velocity Calculation ==="
    result <- try (readFile "data.txt") :: IO (Either IOError String)
    case result of
        Left e -> putStrLn $ "Error reading file: " ++ show e
        Right content -> do
            putStrLn "File read successfully."
            putStrLn "Enter time (in seconds):"
            timeInput <- getLine
            case readMaybe timeInput :: Maybe Float of
                Nothing -> putStrLn "Invalid time input."
                Just time ->
                    case readMaybe content :: Maybe Float of
                        Nothing -> putStrLn "Invalid file content."
                        Just distance -> do
                            let velocity = distance / time
                            putStrLn $ "Velocity: " ++ show velocity ++ " m/s"

-----------------------------------------
-- HC15T2: Self-Driving AI Car System
-----------------------------------------
selfDrivingCar :: String -> IO ()
selfDrivingCar color = do
    putStrLn "\n=== HC15T2: Self-Driving AI Car System ==="
    case color of
        "red"    -> putStrLn "Light: red -> Stop"
        "yellow" -> putStrLn "Light: yellow -> Slow down"
        "green"  -> putStrLn "Light: green -> Move forward"
        _        -> putStrLn "Unknown light color!"

-----------------------------------------
-- HC15T3: Custom Exception for Traffic Light Errors
-----------------------------------------
data TrafficLightError = InvalidLight String
    deriving (Show, Typeable)

instance Exception TrafficLightError

-----------------------------------------
-- HC15T4: Exception Handler for Traffic Light
-----------------------------------------
handleTrafficLight :: String -> IO ()
handleTrafficLight color = do
    putStrLn "\n=== HC15T4: Exception Handler for Traffic Light ==="
    handle (\(InvalidLight msg) -> putStrLn $ "Caught exception: " ++ msg) $ do
        if color `elem` ["red", "yellow", "green"]
            then selfDrivingCar color
            else throwIO (InvalidLight ("Invalid traffic light color: " ++ color))

-----------------------------------------
-- HC15T5: Safe Division Using Maybe
-----------------------------------------
safeDiv :: Float -> Float -> Maybe Float
safeDiv _ 0 = Nothing
safeDiv a b = Just (a / b)

-----------------------------------------
-- HC15T6: Safe Input Parsing with readMaybe
-----------------------------------------
safeParse :: String -> Maybe Int
safeParse = readMaybe

-----------------------------------------
-- HC15T7: Velocity Calculation with Optionals and Parsing Handling
-----------------------------------------
calcVelocity :: String -> String -> IO ()
calcVelocity distStr timeStr = do
    putStrLn "\n=== HC15T7: Velocity Calculation with Optionals ==="
    case (readMaybe distStr :: Maybe Float, readMaybe timeStr :: Maybe Float) of
        (Just d, Just t) ->
            case safeDiv d t of
                Just v  -> putStrLn $ "Velocity: " ++ show v ++ " m/s"
                Nothing -> putStrLn "Error: Division by zero!"
        _ -> putStrLn "Error: Invalid numeric input."

-----------------------------------------
-- HC15T8: Division with Either for Detailed Errors
-----------------------------------------
divEither :: Float -> Float -> Either String Float
divEither _ 0 = Left "Cannot divide by zero!"
divEither a b = Right (a / b)

-----------------------------------------
-- HC15T9: Try Function for File IO Exceptions
-----------------------------------------
tryFileIO :: IO ()
tryFileIO = do
    putStrLn "\n=== HC15T9: Try Function for File IO Exceptions ==="
    result <- try (readFile "example.txt") :: IO (Either IOError String)
    case result of
        Left e -> putStrLn $ "Caught file exception: " ++ show e
        Right content -> putStrLn $ "File content:\n" ++ content

-----------------------------------------
-- HC15T10: Hybrid Error Handling with Either and IO
-----------------------------------------
hybridVelocity :: IO ()
hybridVelocity = do
    putStrLn "\n=== HC15T10: Hybrid Error Handling with Either and IO ==="
    putStrLn "Enter distance:"
    distInput <- getLine
    putStrLn "Enter time:"
    timeInput <- getLine

    case (readMaybe distInput :: Maybe Float, readMaybe timeInput :: Maybe Float) of
        (Just d, Just t) ->
            case divEither d t of
                Left err -> putStrLn $ "Error: " ++ err
                Right v  -> putStrLn $ "Velocity: " ++ show v ++ " m/s"
        _ -> putStrLn "Invalid input provided."

-----------------------------------------
-- MAIN: Run all tasks sequentially
-----------------------------------------
main :: IO ()
main = do
    handleFileAndVelocity
    selfDrivingCar "green"
    handleTrafficLight "red"
    handleTrafficLight "purple"  -- Will trigger custom exception
    putStrLn "\n=== HC15T5: Safe Division Using Maybe ==="
    print (safeDiv 10 2)
    print (safeDiv 10 0)
    putStrLn "\n=== HC15T6: Safe Input Parsing ==="
    print (safeParse "123")
    print (safeParse "abc")
    calcVelocity "100" "20"
    calcVelocity "100" "0"
    putStrLn "\n=== HC15T8: Division with Either ==="
    print (divEither 10 2)
    print (divEither 10 0)
    tryFileIO
    hybridVelocity
