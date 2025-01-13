module Main where

import System.TimeIt ( timeItT )
import Text.Printf ( printf )
import Data.List.Split ( splitOn )

filename :: String
filename = "data/d3.txt"

--adjust depending on structure of input file
parse :: IO [[String]]
parse = map (splitOn ",") . lines <$> readFile filename

part1 :: IO Int
part1 = part1' <$> parse

part1' :: [[String]] -> Int
part1' ls = minAboveThreshhold 0 [distance (intersect a b c d) | 
    (a, b) <- zip xs (tail xs), (c,d) <- zip ys (tail ys)] 
    where (xs,ys) = (toPoints [(0,0)] (head ls), toPoints [(0,0)] (last ls))

part2 :: IO Int
part2 = part2' <$> parse

part2' :: [[String]] -> Int
part2' ls = minAboveThreshhold 0 [pathDistance a b c d t1 t2 |
    (a, b, t1) <- zip3 xs (tail xs) t1s, (c,d, t2) <- zip3 ys (tail ys) t2s]
    where (xs, ys, t1s, t2s) = (toPoints [(0,0)] (head ls), toPoints [(0,0)] (last ls), cumulativeDistance [0] (head ls), cumulativeDistance [0] (last ls))

cumulativeDistance :: [Int] -> [String] -> [Int]
cumulativeDistance xs [] = xs
cumulativeDistance xs ds = cumulativeDistance (xs ++ [last xs + read (tail (head ds))]) (tail ds)

pathDistance :: (Num a, Ord a) => (a, a) -> (a, a) -> (a, a) -> (a, a) -> a -> a -> a
pathDistance s1 e1 s2 e2 c1 c2 = if d > 0 then c1 + c2 + 2*d - distance s1 - distance s2 else 0 
  where d = distance (intersect s1 e1 s2 e2)

intersect :: (Ord a, Num a) => (a, a) -> (a, a) -> (a, a) -> (a, a) -> (a, a)
intersect (ai, bi) (af,bf) (ci,di) (cf, df)
  | ai == af && di == df && inbetween ci cf ai && inbetween bi bf di = (ai, di)
  | bi == bf && ci == cf && inbetween di df bi && inbetween ai af ci = (bi, ci)
  | otherwise = (0,0)

inbetween :: Ord a => a -> a -> a -> Bool
inbetween start end value = (value >= start && value <= end) || (value >= end && value <= start)

endPos :: (Num a, Num b, Read a, Read b) => (a, b) -> String -> (a, b)
endPos _ "" = (0,0)
endPos (x,y) (dir:dist)
  | dir == 'R' = (x+read dist, y)
  | dir == 'L' = (x-read dist, y)
  | dir == 'U' = (x, y+read dist)
  | dir == 'D' = (x, y-read dist)
  | otherwise = (0,0)

toPoints :: [(Int, Int)] -> [String] -> [(Int, Int)]
toPoints xs [] = xs
toPoints xs ds = toPoints (xs ++ [endPos (last xs) (head ds)]) (tail ds)


distance :: Num a => (a, a) -> a
distance (x,y) = abs x + abs y

minAboveThreshhold :: Ord a => a -> [a] -> a
minAboveThreshhold thresh xs = minimum (filter (> thresh) xs)


main :: IO ()
main = do
    -- ls <- parse
    -- let (xs,ys) = (toPoints [(0,0)] (head ls), toPoints [(0,0)] (last ls))
    -- print (xs, ys)
    (time1, result1) <- timeItT part1
    printf "The answer to Day 3 part 2 is: %-10d, calculated in: %.6f ms\n" result1 (time1*1000)
    (time2, result2) <- timeItT part2
    printf "The answer to Day 3 part 2 is: %-10d, calculated in: %.6f ms\n" result2 (time2*1000)