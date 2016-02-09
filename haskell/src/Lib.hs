{-# LANGUAGE PatternGuards #-}

module Lib where

import Data.Char
import Data.List

dim = 5

data Direction = North | East | South | West
  deriving (Eq, Show)

type State = (Int, Int, Direction)
data Command = Place State | Move | TurnLeft | TurnRight | Report
  deriving (Eq, Show)

-- parsing the input

parseDirection :: String -> Maybe Direction
parseDirection "NORTH" = Just North
parseDirection "EAST"  = Just East
parseDirection "SOUTH" = Just South
parseDirection "WEST"  = Just West
parseDirection _       = Nothing

parsePlaceCmd :: String -> Maybe Command
parsePlaceCmd (x:',':y:',':s) | isDigit x && isDigit y
              = case (parseDirection s) of
                  Just d -> Just $ Place (digitToInt x, digitToInt y, d)
                  _ -> Nothing
parsePlaceCmd _ = Nothing

parseLine' :: String -> Maybe Command
parseLine' "MOVE" = Just Move
parseLine' "LEFT" = Just TurnLeft
parseLine' "RIGHT" = Just TurnRight
parseLine' "REPORT" = Just Report
parseLine' cs | Just rest <- stripPrefix "PLACE " cs = parsePlaceCmd rest
parseLine' _ = Nothing

parseLine :: String -> Maybe Command
parseLine = parseLine' . (map toUpper)

parse :: [String] -> [Command]
parse [] = []
parse (s:ss) = case (parseLine s) of
                 Just cmd -> cmd : parse ss
                 Nothing  -> parse ss

-- robot simulation

report :: State -> String
report state = tail $ init $ (map toUpper) $ show state

isValid :: State -> Bool
isValid (x, y, d) = x >= 0 && x < dim && y >= 0 && y < dim

tryMove :: State -> State -> State
tryMove old_state new_state
  | isValid new_state = new_state
  | otherwise = old_state

-- execute a command
execute :: Maybe State -> Command -> Maybe State
execute _ (Place state)
          | isValid state = Just state
          | otherwise = Nothing
execute Nothing _ = Nothing
execute (Just (x, y, d)) Move
          | d == North = Just $ tryMove (x, y, d) (x, y+1, d)
          | d == East  = Just $ tryMove (x, y, d) (x+1, y, d)
          | d == South = Just $ tryMove (x, y, d) (x, y-1, d)
          | d == West  = Just $ tryMove (x, y, d) (x-1, y, d)

execute (Just (x, y, d)) TurnLeft
          | d == North = Just (x, y, West)
          | d == East  = Just (x, y, North)
          | d == South = Just (x, y, East)
          | d == West  = Just (x, y, South)

execute (Just (x, y, d)) TurnRight
          | d == North = Just (x, y, East)
          | d == East  = Just (x, y, South)
          | d == South = Just (x, y, West)
          | d == West  = Just (x, y, North)

-- run simulation on the commands from a initial state
simulate :: Maybe State -> [Command] -> IO ()
simulate _ [] = return ()
simulate Nothing (Report:cs) = simulate Nothing cs
simulate (Just state) (Report:cs) = do
                    putStrLn $ report state
                    simulate (Just state) cs
simulate mstate (c:cs) = simulate (execute mstate c) cs

