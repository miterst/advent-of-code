import Data.Array
import Data.List

main :: IO ()
main = do 
    calories <- readFile "input"
    let sumPerGroup = reverse $ sort $ map sum $ mkGroups $ lines calories
    putStrLn $ "Part1: " ++ show (head $ take 1 sumPerGroup)
    putStrLn $ "Part2: " ++ show (sum $ take 3 sumPerGroup)


mkGroups :: [String] -> [[Int]]
mkGroups [] = []
mkGroups xs = group : remainingGroups
              where 
                group = map readInt $ takeWhile (/= "") xs
                remainingGroups = mkGroups rest
                rest = drop 1 $ dropWhile (/= "") xs                

readInt :: String -> Int
readInt = read
