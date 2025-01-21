module Main where

import System.TimeIt ( timeItT )
import Text.Printf ( printf )

filename :: String
filename = "data/d12.txt"

--adjust depending on structure of input file
parse :: IO [Int]
parse = map read . lines <$> readFile filename

part1 :: IO Int
part1 = sum <$> parse

part2 :: IO Int
part2 = sum <$> parse


main :: IO ()
main = do 
    (time1, result1) <- timeItT part1
    printf "The answer to Day 12 part 1 is: %-10d, calculated in: %.6f ms\n" result1 (time1*1000)
    (time2, result2) <- timeItT part2
    printf "The answer to Day 12 part 2 is: %-10d, calculated in: %.6f ms\n" result2 (time2*1000)