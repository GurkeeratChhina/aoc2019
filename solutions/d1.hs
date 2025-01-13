module Main where

import System.TimeIt ( timeItT )
import Text.Printf ( printf )

filename :: String
filename = "data/d1.txt"

--adjust depending on structure of input file
parse :: IO [Int]
parse = map read . lines <$> readFile filename

part1 :: IO Int
part1 = sum . map fuel <$> parse

part2 :: IO Int
part2 = sum . map recursiveFuel <$> parse

fuel :: Int -> Int
fuel mass = mass `div` 3 - 2

recursiveFuel :: Int -> Int
recursiveFuel mass = if fuel mass <= 0 then 0 else fuel mass + (recursiveFuel . fuel) mass

main :: IO ()
main = do
    (time1, result1) <- timeItT part1
    printf "The answer to Day 1 part 2 is: %-10d, calculated in: %.6f ms\n" result1 (time1*1000)
    (time2, result2) <- timeItT part2
    printf "The answer to Day 1 part 2 is: %-10d, calculated in: %.6f ms\n" result2 (time2*1000)