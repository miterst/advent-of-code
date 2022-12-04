#!/usr/bin/env stack

import Data.Bifunctor ( bimap )

main :: IO ()
main = do 
    file <- readFile "input"
    let assignments    = bimap mkAssignment mkAssignment <$> split ',' <$> lines file
    let fullyContained = length $ filter id $ fullyOverlap <$> assignments
    let overlapAtAll   = length $ filter id $ overlapAny <$> assignments

    putStrLn $ "Part 1: " ++ show fullyContained
    putStrLn $ "Part 2: " ++ show overlapAtAll


type Assignment = (Int, Int)

mkAssignment :: String -> Assignment 
mkAssignment = splitMap readInt '-' 

fullyOverlap :: (Assignment, Assignment) -> Bool
fullyOverlap ((l1, r1), (l2, r2)) = (l1 <= l2 && r2 <= r1) || (l2 <= l1 && r1 <= r2)

overlapAny :: (Assignment, Assignment) -> Bool
overlapAny ((l1, r1), (l2, r2)) = (l1 <= l2 && l2 <= r1) || (l2 <= l1 && l1 <= r2)

splitMap :: (String -> a) -> Char -> String -> (a, a)
splitMap f chr = bimap f f . split chr

split :: Char -> String -> (String, String)
split chr str = (left, right)
                where
                    left  = takeWhile (/= chr) str
                    right = (drop 1 . dropWhile (/= chr)) str

readInt :: String -> Int    
readInt = read