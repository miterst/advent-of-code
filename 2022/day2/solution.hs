#!/usr/bin/env stack

import Data.Tuple as Tuple
import Data.Bifunctor as Bifunctor


data Choice = Rock | Papper | Scissors
                deriving (Show, Eq)

choiceFromString :: String -> Choice
choiceFromString "A" = Rock
choiceFromString "X" = Rock
choiceFromString "B" = Papper
choiceFromString "Y" = Papper
choiceFromString "C" = Scissors
choiceFromString "Z" = Scissors

loosesFrom :: Choice -> Choice
loosesFrom Rock     = Papper
loosesFrom Scissors = Rock
loosesFrom Papper   = Scissors

mkChoice :: [String] -> (Choice, Choice)
mkChoice choices = list2Tuple $ choiceFromString <$> choices

mkChoice2 :: (Choice, String) -> (Choice, Choice)
mkChoice2 (choice, "Z") = (choice, loosesFrom choice) 
mkChoice2 (choice, "X") = (choice, (loosesFrom . loosesFrom) choice) -- Loss is found transitively
mkChoice2 (choice,   _) = (choice, choice) 

pointsForChoice :: Choice -> Int
pointsForChoice Rock     = 1
pointsForChoice Papper   = 2
pointsForChoice Scissors = 3


data Outcome = Win | Loss | Draw
                deriving Show

gameOutcome :: (Choice, Choice) -> Outcome
gameOutcome (opponent, me) 
            | opponent == me            = Draw
            | loosesFrom opponent == me = Win
            | otherwise                 = Loss

pointsForOutcome :: Outcome -> Int
pointsForOutcome Win  = 6
pointsForOutcome Draw = 3
pointsForOutcome Loss = 0


list2Tuple :: [a] -> (a, a)
list2Tuple [x, y] = (x, y)


part1 :: String -> IO ()
part1 gamesFile = do
    let games         = mkChoice <$> words <$> lines gamesFile
    let outcomePoints = sum $ pointsForOutcome <$> gameOutcome <$> games 
    let choicePoints  = sum $ pointsForChoice <$> Tuple.snd <$> games
    let score         = outcomePoints + choicePoints
    putStrLn $ "Part 1: " ++ show score

part2 :: String -> IO ()
part2 gamesFile = do
    let games         = mkChoice2 <$> Bifunctor.first choiceFromString <$> list2Tuple <$> words <$> lines gamesFile
    let outcomePoints = sum $ pointsForOutcome <$> gameOutcome <$> games
    let choicePoints  = sum $ pointsForChoice <$> Tuple.snd <$> games
    let score         = outcomePoints + choicePoints
    putStrLn $ "Part 2: " ++ show score


main :: IO ()
main = do
    gamesFile <- readFile "input"
    part1 gamesFile
    part2 gamesFile