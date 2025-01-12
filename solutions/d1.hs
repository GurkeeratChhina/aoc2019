module Main where

filename :: String
filename = "data/d1.txt"

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
    putStr "The answer to Day 1 part 1 is: "
    print =<< part1
    putStr "The answer to Day 1 part 2 is: "
    print =<< part2