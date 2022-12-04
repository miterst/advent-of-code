#!/usr/bin/env stack

import Data.List ( intersect, nub )
import Data.Char ( ord, isLower )

main :: IO ()
main = do
    file <- readFile "input"               
    let xs               = lines file   
    let commonTypesTotal = sum $ priority <$> commonTypes xs
    let groupTypesTotal  = sum $ priority <$> groupTypes xs

    putStrLn $ "Part 1: " ++ show commonTypesTotal
    putStrLn $ "Part 2: " ++ show groupTypesTotal


commonTypes :: [String] -> String
commonTypes xs = concat $ nub <$> intersectHalf <$> xs
                    where 
                        intersectHalf xs = let n = length xs `div` 2 
                                           in intersect (take n xs) (drop n xs)
                        

groupTypes :: [String] -> String
groupTypes xs = concat $ map intersectAll $ chunk 3 $ xs  
                where
                    intersectAll xs = foldr1 intersect $ map nub xs 

chunk :: Int -> [String] -> [[String]]
chunk n [] = []
chunk n xs = (take n xs) : chunk n (drop n xs)

priority :: Char -> Int    
priority c | isLower c = ord c - 96
           | otherwise = ord c - 38