module Main where

import Data.List.Split ( splitOn )
import System.TimeIt ( timeItT )
import Text.Printf ( printf )


filename :: String
filename = "data/d2.txt"

--adjust depending on structure of input file
parse :: IO [Int]
parse = map read . splitOn "," <$> readFile filename

part1 :: IO Int
part1 = run 12 2 <$> parse

instr :: Int -> [Int] -> [Int]
instr index l
  | l !! index == 99 = l
  | l !! index == 1 = instr (index+4) (replace (l!!(index+3)) ((l!!(l!!(index+1))) + (l!!(l!!(index+2)))) l )
  | l !! index == 2 = instr (index+4) (replace (l!!(index+3)) ((l!!(l!!(index+1))) * (l!!(l!!(index+2)))) l )
  | otherwise = [0]

replace :: Int -> a -> [a] -> [a]
replace index value l = x ++ value:tail y where (x, y) = splitAt index l

run :: Int -> Int -> [Int] -> Int
run noun verb = head . instr 0 . replace 1 noun . replace 2 verb

part2' :: Int -> [Int] -> Int
part2' x l = if 19690720 == run (x `div` 100) (x `mod` 100) l then x else part2' (x+1) l

part2 :: IO Int
part2 = part2' 0 <$> parse

main :: IO ()
main = do 
    (time1, result1) <- timeItT part1
    printf "The answer to Day 1 part 2 is: %-10d, calculated in: %.6f ms\n" result1 (time1*1000)
    (time2, result2) <- timeItT part2
    printf "The answer to Day 1 part 2 is: %-10d, calculated in: %.6f ms\n" result2 (time2*1000)