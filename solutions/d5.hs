module Main where

import System.TimeIt ( timeItT )
import Text.Printf ( printf )
import Data.List.Split ( splitOn )

filename :: String
filename = "data/d5.txt"

--adjust depending on structure of input file
parse :: IO [Int]
parse = map read . splitOn "," <$> readFile filename

instr :: Int -> [Int] -> [Int]
instr index l
  | code == 99 = []
  | code == 1 = instr (index+4) (replace (l!!(index+3)) (paramMode mode1 (index+1) l + paramMode mode2 (index+2) l) l )
  | code == 2 = instr (index+4) (replace (l!!(index+3)) (paramMode mode1 (index+1) l * paramMode mode2 (index+2) l) l )
  | code == 3 = instr (index+2) (replace (l!!(index+1)) 1 l)
  | code == 4 = paramMode mode1 (index+1) l:instr (index+2) l
  | otherwise = [0]
  where (code, mode1, mode2) = ((l!!index) `mod` 100, ((l!!index) `div` 100) `mod` 10, ((l!!index) `div` 1000) `mod` 10)

replace :: Int -> a -> [a] -> [a]
replace index value l = x ++ value:tail y where (x, y) = splitAt index l

part1 :: IO [Int]
part1 = instr 0 <$> parse

part2 :: IO [Int]
part2 = part1


paramMode :: Int -> Int -> [Int] -> Int
paramMode 0 index l = l!!(l!!index)
paramMode 1 index l = l!!index
paramMode x index l = x

main :: IO ()
main = do
    print >== part1
    -- (time1, result1) <- timeItT part1
    -- printf "The answer to Day 5 part 2 is: %-10d, calculated in: %.6f ms\n" result1 (time1*1000)
    -- (time2, result2) <- timeItT part2
    -- printf "The answer to Day 5 part 2 is: %-10d, calculated in: %.6f ms\n" result2 (time2*1000)