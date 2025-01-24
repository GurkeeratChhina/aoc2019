{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import System.TimeIt ( timeItT )
import Text.Printf ( printf )
import Control.Lens
import Data.List
import Text.Regex.Posix

data Point = Point {_x::Integer, _y::Integer, _z::Integer} deriving (Show)
data Moon = Moon {_pos::Point, _vel::Point} deriving (Show)
makeLenses ''Point
makeLenses ''Moon

filename :: String
filename = "data/d12.txt"

--adjust depending on structure of input file
parse :: IO [[Integer]]
parse = map ( map read . (\s -> getAllTextMatches (s =~ "-?[0-9]+")) ) . lines <$> readFile filename

makeMoons :: [[Integer]] -> [Moon]
makeMoons = map (\a -> Moon (Point (a!!0) (a!!1) (a!!2)) (Point 0 0 0))
-- makeMoons [] = []
-- makeMoons (a:rest) = Moon (Point a!!0 a!!1 a!!2) (Point 0 0 0): makeMoons rest

pairs :: [a] -> [(a, a)]
pairs l = [(a,b) | (a:bs) <- tails l, b <- bs]

energy :: Point -> Integer
energy (Point a b c) = abs a + abs b + abs c

int3Sum :: Point -> Point -> Point
int3Sum a b = Point (_x a + _x b) (_y a + _y b) (_z a + _z b)

applyVelocity:: Moon -> Moon
applyVelocity (Moon p v) = Moon (p `int3Sum` v) v

applyGravity:: (Moon, Moon) -> (Moon, Moon)
applyGravity = applyGravity' x . applyGravity' y . applyGravity' z

applyGravities :: [Moon] -> [Moon]
applyGravities [] = []
applyGravities (m:rest) = let (m', rest') = foldl' (\ (a,l) b -> let (c,d) = applyGravity (a,b) in (c,l++[d])) (m, []) rest in m':applyGravities rest'

applyGravity' :: Lens' Point Integer -> (Moon, Moon) -> (Moon, Moon)
applyGravity' coord (m1, m2)
  | m1 ^. (pos . coord) < m2 ^. (pos . coord) = (m1 & (vel . coord) +~ 1, m2 & (vel . coord) +~ (-1))
  | m1 ^. (pos . coord) > m2 ^. (pos . coord) = (m1 & (vel . coord) +~ (-1), m2 & (vel . coord) +~ 1)
  | otherwise = (m1, m2)

applyAxialGravities :: [(Integer, Integer)] -> [(Integer, Integer)]
applyAxialGravities [] = []
applyAxialGravities (m:rest) = let (m', rest') = foldl' (\ (a,l) b -> let (c,d) = applyAxialGravity (a,b) in (c,l++[d])) (m, []) rest in m':applyAxialGravities rest'

applyAxialGravity :: ((Integer, Integer),(Integer, Integer)) -> ((Integer, Integer),(Integer, Integer))
applyAxialGravity (m1, m2)
  | fst m1 < fst m2 = (m1 & _2 +~ 1, m2 & _2 +~ (-1))
  | fst m1 > fst m2 = (m1 & _2 +~ (-1), m2 & _2 +~ 1)
  | otherwise = (m1, m2)

updateMoons :: [Moon] -> [Moon]
updateMoons moons = map applyVelocity $ applyGravities moons

updateCoords :: [(Integer, Integer)] -> [(Integer, Integer)]
updateCoords = map (\(a,b) -> (a+b, b)) . applyAxialGravities



finalEnergies :: Integer -> [Moon] -> Integer
finalEnergies 0 moons = sum . map (\m -> energy (_pos m) * energy (_vel m)) $ moons
finalEnergies n moons = finalEnergies (n-1) (updateMoons moons)

cycleLength :: Lens' Point Integer -> [Moon] -> Integer
cycleLength coord moons = cycleLength' 0 (map (\m -> m ^. (pos . coord)) moons) (map (\m -> (m ^. (pos . coord), m ^. (vel . coord))) moons)

-- current is [(pos, vel)]
cycleLength' :: (Num t, Eq t) => t -> [Integer] -> [(Integer, Integer)] -> t
cycleLength' 0 s c = cycleLength' 1 s (updateCoords c)
cycleLength' steps start current = 
  if all (== 0) velocities && start == positions then steps
  else cycleLength' (steps+1) start (updateCoords current)
  where (positions, velocities) = (map fst current, map snd current)


part1 :: IO Integer
part1 = finalEnergies 1000 . makeMoons <$> parse

part2' :: [Moon] -> Integer
part2' moons = lcm a (lcm b c) where (a, b, c) = (cycleLength x moons, cycleLength y moons, cycleLength z moons)

part2 :: IO Integer
part2 = part2' . makeMoons <$> parse


main :: IO ()
main = do
  -- print =<< part1
    (time1, result1) <- timeItT part1
    printf "The answer to Day 12 part 1 is: %-10d, calculated in: %.6f ms\n" result1 (time1*1000)
    (time2, result2) <- timeItT part2
    printf "The answer to Day 12 part 2 is: %-10d, calculated in: %.6f ms\n" result2 (time2*1000)