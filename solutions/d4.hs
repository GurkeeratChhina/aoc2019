module Main where

import System.TimeIt ( timeItT )
import Text.Printf ( printf )
import Data.List.Split ( splitOn )

filename :: String
filename = "data/d4.txt"

--adjust depending on structure of input file
parse :: IO [Int]
parse = map read . splitOn "-" <$> readFile filename

part1 :: IO Int
part1 =  length . validInRange <$> parse

part2 :: IO Int
part2 = length . validInRange' <$> parse

digitsIncreasing :: Integral a => a -> Bool
digitsIncreasing x = (x < 10) || (digitsIncreasing (x `div` 10) && (x `mod` 10) >= ((x `div` 10) `mod` 10))

hasDouble :: Integral a => a -> Bool
hasDouble x = (x >= 10) && (hasDouble (x `div` 10) || (x `mod` 10) == ((x `div` 10) `mod` 10))

validInRange :: Integral a => [a] -> [a]
validInRange range = filter digitsIncreasing . filter hasDouble $ [(head range)..(last range)]

validInRange' :: Integral a => [a] -> [a]
validInRange' range = filter digitsIncreasing . filter hasDouble' $ [(head range)..(last range)]

hasDouble' :: Integral t => t -> Bool
hasDouble' x = 2 `elem` [countOf x n | n <- [0..9]]

countOf :: (Num a, Integral t) => t -> t -> a
countOf x n
  | x < 10 && x == n = 1
  | x < 10 && x /= n = 0
  | x `mod` 10 == n = 1 + countOf (x `div` 10) n
  | otherwise = countOf (x `div` 10) n

main :: IO ()
main = do
    (time1, result1) <- timeItT part1
    printf "The answer to Day 4 part 2 is: %-10d, calculated in: %.6f ms\n" result1 (time1*1000)
    (time2, result2) <- timeItT part2
    printf "The answer to Day 4 part 2 is: %-10d, calculated in: %.6f ms\n" result2 (time2*1000)