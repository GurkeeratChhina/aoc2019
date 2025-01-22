{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import System.TimeIt ( timeItT )
import Text.Printf ( printf )
import Data.List.Split ( splitOn )
import qualified Data.Map.Strict as Map

filename :: String
filename = "data/d11.txt"

data State = State {halted :: Bool, i :: Integer, code :: Map.Map Integer Integer, inputs :: [Integer], outputs :: [Integer], relbase:: Integer} deriving (Show)
data ComplexInt = ComplexInt {x::Int, yi:: Int} deriving (Eq, Ord, Show)

runInstr :: State -> State
runInstr (State h index l ins outs b)
  | h = State False 9998 Map.empty [] [] 0
  | opcode == 99 = State True index l ins outs b
  | opcode == 1 = runInstr (State h (index+4) (Map.insert (paramMode (mode3+10) (index+3) l b) (paramMode mode1 (index+1) l b + paramMode mode2 (index+2) l b) l ) ins outs b)
  | opcode == 2 = runInstr (State h (index+4) (Map.insert (paramMode (mode3+10) (index+3) l b) (paramMode mode1 (index+1) l b * paramMode mode2 (index+2) l b) l ) ins outs b)
  | opcode == 3 && null ins = State h index l ins outs b
  | opcode == 3 && not (null ins) = runInstr (State h (index+2) (Map.insert (paramMode (mode1+10) (index+1) l b) (head ins) l) (tail ins) outs b)
  | opcode == 4 = runInstr (State h (index+2) l ins (outs++[paramMode mode1 (index+1) l b]) b)
  | opcode == 5 = if paramMode mode1 (index+1) l b /= 0 then runInstr (State h (paramMode mode2 (index+2) l b) l ins outs b) else runInstr (State h (index+3) l ins outs b)
  | opcode == 6 = if paramMode mode1 (index+1) l b == 0 then runInstr (State h (paramMode mode2 (index+2) l b) l ins outs b) else runInstr (State h (index+3) l ins outs b)
  | opcode == 7 = if paramMode mode1 (index+1) l b < paramMode mode2 (index+2) l b
    then runInstr (State h (index+4) (Map.insert (paramMode (mode3+10) (index+3) l b) 1 l ) ins outs b)
    else runInstr (State h (index+4) (Map.insert (paramMode (mode3+10) (index+3) l b) 0 l ) ins outs b)
  | opcode == 8 = if paramMode mode1 (index+1) l b == paramMode mode2 (index+2) l b
    then runInstr (State h (index+4) (Map.insert (paramMode (mode3+10) (index+3) l b) 1 l ) ins outs b)
    else runInstr (State h (index+4) (Map.insert (paramMode (mode3+10) (index+3) l b) 0 l ) ins outs b)
  | opcode == 9 = runInstr (State h (index +2) l ins outs (b + paramMode mode1 (index+1) l b))
  | otherwise = State False (opcode+1000) Map.empty [] [] b
  where (opcode, mode1, mode2, mode3) = (Map.findWithDefault 0 index l `mod` 100, (Map.findWithDefault 0 index l `div` 100) `mod` 10, (Map.findWithDefault 0 index l `div` 1000) `mod` 10, (Map.findWithDefault 0 index l `div` 10000) `mod` 10)

paramMode :: Integer -> Integer -> Map.Map Integer Integer -> Integer -> Integer
paramMode 0 index l _ = Map.findWithDefault 0 (Map.findWithDefault 0 index l) l
paramMode 1 index l _ = Map.findWithDefault 0 index l
paramMode 2 index l base = Map.findWithDefault 0 (Map.findWithDefault 0 index l + base) l
paramMode 10 index l _ = Map.findWithDefault 0 index l
paramMode 12 index l base = Map.findWithDefault 0 index l + base

--adjust depending on structure of input file
parse :: IO [Integer]
parse = map read . splitOn "," <$> readFile filename

buildMap :: Integer -> [Integer] -> Map.Map Integer Integer
buildMap _ [] = Map.empty
buildMap n (x1:xs) = Map.insert n x1 (buildMap (n+1) xs)

robotLoop :: Map.Map ComplexInt Integer -> ComplexInt -> ComplexInt -> State -> Map.Map ComplexInt Integer
robotLoop visited _ _ (State True _ _ _ _ _)  = visited
robotLoop visited current direction state  = let newDir = (direction `complexMult` outputToDirection (last $ outputs nextState)) in
    robotLoop (Map.insert current (head $ outputs nextState) visited) (current `complexAdd` newDir) newDir nextState
    where nextState = runInstr state{inputs = [Map.findWithDefault 0 current visited], outputs = []}

outputToDirection :: Integer -> ComplexInt
outputToDirection 0 = ComplexInt 0 1
outputToDirection 1 = ComplexInt 0 (-1)
outputToDirection _ = ComplexInt 0 0

complexAdd :: ComplexInt -> ComplexInt -> ComplexInt
complexAdd (ComplexInt a b) (ComplexInt c d) = ComplexInt (a + c) (b + d)

complexMult :: ComplexInt -> ComplexInt -> ComplexInt
complexMult (ComplexInt a b) (ComplexInt c d) = ComplexInt (a*c - b*d) (a*d + b*c)

part1' :: Map.Map Integer Integer -> Int
part1' m = Map.size $ robotLoop Map.empty (ComplexInt 0 0) (ComplexInt 0 1) (State False 0 m [] [] 0)

part1 :: IO Int
part1 = part1' . buildMap 0 <$> parse

part2' :: Map.Map Integer Integer -> Map.Map ComplexInt Integer
part2' m = robotLoop (Map.singleton (ComplexInt 0 0) 1) (ComplexInt 0 0) (ComplexInt 0 1) (State False 0 m [] [] 0)

part2 :: IO String
part2 = drawGrid . part2' . buildMap 0 <$> parse

drawGrid :: Map.Map ComplexInt Integer -> String
drawGrid m = drawGrid' m (findMin m) (findMax m) (findMin m)
-- drawGrid m = show (findMin m) ++ show (findMax m)

drawGrid' :: Map.Map ComplexInt Integer -> ComplexInt -> ComplexInt -> ComplexInt -> String
drawGrid' m topLeft bottomRight pos
  | yi pos < yi bottomRight = ""
  | x pos > x bottomRight = show (yi pos) ++ "\n"++ drawGrid' m topLeft bottomRight (ComplexInt (x topLeft) (yi pos - 1))
  | Map.findWithDefault 0 pos m == 1 = "⬜" ++ drawGrid' m topLeft bottomRight pos{x = x pos + 1}
  | otherwise = "⬛" ++ drawGrid' m topLeft bottomRight pos{x = x pos + 1}
findMin :: Map.Map ComplexInt Integer -> ComplexInt
findMin m = ComplexInt (x $ fst (findWith (\a b -> x (fst a) < x (fst b)) m')) (yi $ fst (findWith (\a b -> yi (fst a) > yi (fst b)) m')) where m' = Map.toList m

findMax :: Map.Map ComplexInt Integer -> ComplexInt
findMax m = ComplexInt (x $ fst (findWith (\a b -> x (fst a) > x (fst b)) m')) (yi $ fst (findWith (\a b -> yi (fst a) < yi (fst b)) m')) where m' = Map.toList m

findWith :: (a -> a -> Bool) -> [a] -> a
findWith _ [a] = a
findWith f (x1:x2:rest) = if f x1 x2 then findWith f (x1:rest) else findWith f (x2:rest)

main :: IO ()
main = do
    (time1, result1) <- timeItT part1
    printf "The answer to Day 11 part 1 is: %-10d, calculated in: %.6f ms\n" result1 (time1*1000)
    (time2, result2) <- timeItT part2
    printf "The answer to Day 11 part 2 is: , calculated in: %.6f ms\n" (time2*1000)
    putStr result2