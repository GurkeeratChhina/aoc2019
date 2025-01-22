{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Main where

import System.TimeIt ( timeItT )
import Text.Printf ( printf )
import qualified Data.Set as Set
import Data.Foldable (maximumBy)
import Data.Ord (comparing)
import Data.List (sortBy)

data Point = Point {x::Int, y::Int} deriving (Eq, Ord, Show) 

filename :: String
filename = "data/d10.txt"

--adjust depending on structure of input file
parse :: IO [String]
parse = lines <$> readFile filename

toLowest :: Point -> Point
toLowest (Point a b)
  | a == 0 && b == 0 = Point a b
  | a == 0 = Point a (b `div` d)
  | b == 0 = Point (a `div` d) b
  | otherwise = Point (a `div` d) (b `div` d)
  where d = gcd a b

buildPoints :: [String] -> Int -> [Point]
buildPoints [] _ = []
buildPoints (s:rest) n = buildPoints' s 0 n ++ buildPoints rest (n+1)

buildPoints' :: String -> Int -> Int -> [Point]
buildPoints' "" _ _  = []
buildPoints' (s:rest) a b = if s == '#' then Point a b: buildPoints' rest (a+1) b else buildPoints' rest (a+1) b

differences :: Point -> [Point] -> Set.Set Point
differences _ [] = Set.empty
differences p (s:rest) = Set.insert (toLowest (diff p s)) (differences p rest)

diff :: Point -> Point -> Point
diff (Point a b) (Point c d) = Point (c-a) (d-b)

part1' :: [String] -> Int
part1' grid = maximum (map (Set.size . \s -> differences s points) points) - 1 where points = buildPoints grid 0

part1 :: IO Int
part1 = part1' <$> parse

toAngle :: Point -> Double
toAngle (Point a b) = atan2 (fromIntegral b) (fromIntegral a)

clockwiseAngle :: Point -> Double
clockwiseAngle (Point a b) = let angle = atan2 (fromIntegral (-b)) (fromIntegral  a) in
  if angle > pi/2 then (5 * pi/2) - angle
  else (pi/2) - angle

bestPoint :: [Point] -> Point
bestPoint points = maximumBy (comparing (Set.size . \s -> differences s points)) points

sortedAngle :: Set.Set Point -> [Point]
sortedAngle points = sortBy (comparing clockwiseAngle) (Set.toList points)

firstMatch :: [Point] -> Point -> Point -> Point
firstMatch points origin vector = if next `elem` points then next else firstMatch points next vector where next = Point (x origin + x vector) (y origin + y vector)

linearize :: Point -> Int
linearize (Point a b) = 100 * a + b

part2' :: Int ->  [Point] -> Point
part2' n points = firstMatch points p ((sortedAngle (differences p (Set.toList . Set.delete p . Set.fromList $ points)))!!(n-1))
 where p = bestPoint points

part2 :: IO Int
part2 = linearize . part2' 200 . (`buildPoints` 0) <$> parse


main :: IO ()
main = do
    (time1, result1) <- timeItT part1
    printf "The answer to Day 10 part 1 is: %-10d, calculated in: %.6f ms\n" result1 (time1*1000)
    -- print =<< part2' 200 . (`buildPoints` 0) <$> parse
    (time2, result2) <- timeItT part2
    printf "The answer to Day 10 part 2 is: %-10d, calculated in: %.6f ms\n" result2 (time2*1000)