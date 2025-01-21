module Main where

import System.TimeIt ( timeItT )
import Text.Printf ( printf )
import qualified Data.Map.Strict as Map

filename :: String
filename = "data/d9.txt"

-- TODO: replace c (code) with a map from Data.Map
data State = State {halted :: Bool, i :: Integer, c :: Map.Map Integer Integer, inputs :: [Integer], outputs :: [Integer], relbase:: Integer} deriving (Show)

--adjust depending on structure of input file
parse :: IO [Int]
parse = map read . lines <$> readFile filename

part1 :: IO Int
part1 = sum <$> parse

part2 :: IO Int
part2 = sum <$> parse

-- TODO: update to use Data.Map Methods
runInstr :: State -> State
runInstr (State h index l ins outs b)
  | h = State False 0 Map.empty [] [] 0
  | code == 99 = State True index l ins outs b
  | code == 1 = runInstr (State h (index+4) (Map.insert (paramMode mode3 (index+3) l b) (paramMode mode1 (index+1) l b + paramMode mode2 (index+2) l b) l ) ins outs b)
  | code == 2 = runInstr (State h (index+4) (Map.insert (paramMode mode3 (index+3) l b) (paramMode mode1 (index+1) l b * paramMode mode2 (index+2) l b) l ) ins outs b)
  | code == 3 && null ins = State h index l ins outs b
  | code == 3 && not (null ins) = runInstr (State h (index+2) (Map.insert (paramMode mode1 (index+1) l b) (head ins) l) (tail ins) outs b)
  | code == 4 = runInstr (State h (index+2) l ins (paramMode mode1 (index+1) l b:outs) b)
  | code == 5 = if paramMode mode1 (index+1) l /= 0 then runInstr (State h (paramMode mode2 (index+2) l b) l ins outs b) else runInstr (State h (index+3) l ins outs b)
  | code == 6 = if paramMode mode1 (index+1) l == 0 then runInstr (State h (paramMode mode2 (index+2) l b) l ins outs b) else runInstr (State h (index+3) l ins outs b)
  | code == 7 = if paramMode mode1 (index+1) l < paramMode mode2 (index+2) l
    then runInstr (State h (index+4) (Map.insert (paramMode mode3 (index+3) l b) 1 l ) ins outs b)
    else runInstr (State h (index+4) (Map.insert (paramMode mode3 (index+3) l b) 0 l ) ins outs b)
  | code == 8 = if paramMode mode1 (index+1) l == paramMode mode2 (index+2) l
    then runInstr (State h (index+4) (Map.insert (paramMode mode3 (index+3) l b) 1 l ) ins outs b)
    else runInstr (State h (index+4) (Map.insert (paramMode mode3 (index+3) l b) 0 l ) ins outs b)
  | otherwise = State False 0 Map.empty [] [] b
  where (code, mode1, mode2, mode3) = (Map.findWithDefault 0 index l `mod` 100, (Map.findWithDefault 0 index l `div` 100) `mod` 10, (Map.findWithDefault 0 index l `div` 1000) `mod` 10, (Map.findWithDefault 0 index l `div` 10000) `mod` 10)

paramMode :: Integer -> Integer -> Map.Map Integer Integer -> Integer -> Integer
paramMode 0 index l _ = Map.findWithDefault 0 (Map.findWithDefault 0 index l) l
paramMode 1 index l _ = Map.findWithDefault 0 index l
paramMode 2 index l base = Map.findWithDefault 0 (Map.findWithDefault 0 index l + base) l
paramMode x _ _ _ = x


main :: IO ()
main = do
    (time1, result1) <- timeItT part1
    printf "The answer to Day 9 part 1 is: %-10d, calculated in: %.6f ms\n" result1 (time1*1000)
    (time2, result2) <- timeItT part2
    printf "The answer to Day 9 part 2 is: %-10d, calculated in: %.6f ms\n" result2 (time2*1000)