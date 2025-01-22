{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import System.TimeIt ( timeItT )
import Text.Printf ( printf )

filename :: String
filename = "data/d8.txt"

--adjust depending on structure of input file

parse :: IO [Int]
parse = map read . letterize . head . lines <$> readFile filename

part1 :: IO Int
part1 = part1' . listSections (25*6) <$> parse

part1' :: (Eq a, Num a) => [[a]] -> Int
part1' list = let fewestZeros = minBy (countOccurances 0) list in countOccurances 1 fewestZeros * countOccurances 2 fewestZeros

part2 :: IO [[Int]]
part2 = part2' <$> parse

listSections :: Int -> [a] -> [[a]]
listSections n l = if length l > n then take n l :listSections n (drop n l) else [l]

minBy :: Ord b => (a -> b) -> [a] -> a
minBy _ [x] = x
minBy measure (x:y:rest) = minBy measure (if measure x > measure y then y:rest else x:rest)

countOccurances :: Eq a => a -> [a] -> Int
countOccurances element list = length $ filter (==element) list

letterize :: String -> [String]
letterize = map return

zipAny :: [[a]] -> [[a]]
zipAny lists = if any null lists then [] else map head lists:zipAny (map tail lists)

part2' :: [Int] -> [[Int]]
part2' = listSections 25 . map (head . filter (/= 2)) . zipAny . listSections (25*6)

printBinaryArray :: [[Int]] -> String
printBinaryArray [] = ""
printBinaryArray (x:xs) = printBinaryRow x ++ printBinaryArray xs

printBinaryRow :: [Int] -> String
printBinaryRow [] = "\n"
printBinaryRow (0:xs) = "⬛"++printBinaryRow xs
printBinaryRow (1:xs) = "⬜"++printBinaryRow xs

main :: IO ()
main = do
    -- print =<< parse
    (time1, result1) <- timeItT part1
    printf "The answer to Day 8 part 1 is: %-7d, calculated in: %.6f ms\n" result1 (time1*1000)
    -- print =<< part2
    (time2, result2) <- timeItT part2
    printf "The answer to Day 8 part 2 is:  calculated in: %.6f ms\n" (time2*1000)
    putStr (printBinaryArray result2)