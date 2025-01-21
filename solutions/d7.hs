module Main where

import System.TimeIt ( timeItT )
import Text.Printf ( printf )
import Data.List.Split ( splitOn )
import Data.List ( permutations )

filename :: String
filename = "data/d7.txt"

parse :: IO [Int]
parse = map read . splitOn "," <$> readFile filename

instr :: [Int] -> Int-> [Int] -> [Int]
instr input index l
  | code == 99 = []
  | code == 1 = instr input (index+4) (replace (l!!(index+3)) (paramMode mode1 (index+1) l + paramMode mode2 (index+2) l) l )
  | code == 2 = instr input (index+4) (replace (l!!(index+3)) (paramMode mode1 (index+1) l * paramMode mode2 (index+2) l) l )
  | code == 3 = instr (tail input) (index+2) (replace (l!!(index+1)) (head input) l)
  | code == 4 = paramMode mode1 (index+1) l:instr input (index+2) l
  | code == 5 = if paramMode mode1 (index+1) l /= 0 then instr input (paramMode mode2 (index+2) l) l else instr input (index+3) l
  | code == 6 = if paramMode mode1 (index+1) l == 0 then instr input (paramMode mode2 (index+2) l) l else instr input (index+3) l
  | code == 7 = if paramMode mode1 (index+1) l < paramMode mode2 (index+2) l
    then instr input (index+4) (replace (l!!(index+3)) 1 l )
    else instr input (index+4) (replace (l!!(index+3)) 0 l )
  | code == 8 = if paramMode mode1 (index+1) l == paramMode mode2 (index+2) l
    then instr input (index+4) (replace (l!!(index+3)) 1 l )
    else instr input (index+4) (replace (l!!(index+3)) 0 l )
  | otherwise = [0]
  where (code, mode1, mode2) = ((l!!index) `mod` 100, ((l!!index) `div` 100) `mod` 10, ((l!!index) `div` 1000) `mod` 10)

runInstr :: State -> State
runInstr (State h index l ins outs)
  | h = State False 0 [] [] []
  | code == 99 = State True index l ins outs
  | code == 1 = runInstr (State h (index+4) (replace (l!!(index+3)) (paramMode mode1 (index+1) l + paramMode mode2 (index+2) l) l ) ins outs)
  | code == 2 = runInstr (State h (index+4) (replace (l!!(index+3)) (paramMode mode1 (index+1) l * paramMode mode2 (index+2) l) l ) ins outs)
  | code == 3 && null ins = State h index l ins outs
  | code == 3 && not (null ins) = runInstr (State h (index+2) (replace (l!!(index+1)) (head ins) l) (tail ins) outs)
  | code == 4 = runInstr (State h (index+2) l ins (paramMode mode1 (index+1) l:outs))
  | code == 5 = if paramMode mode1 (index+1) l /= 0 then runInstr (State h (paramMode mode2 (index+2) l) l ins outs) else runInstr (State h (index+3) l ins outs)
  | code == 6 = if paramMode mode1 (index+1) l == 0 then runInstr (State h (paramMode mode2 (index+2) l) l ins outs) else runInstr (State h (index+3) l ins outs)
  | code == 7 = if paramMode mode1 (index+1) l < paramMode mode2 (index+2) l
    then runInstr (State h (index+4) (replace (l!!(index+3)) 1 l ) ins outs)
    else runInstr (State h (index+4) (replace (l!!(index+3)) 0 l ) ins outs)
  | code == 8 = if paramMode mode1 (index+1) l == paramMode mode2 (index+2) l
    then runInstr (State h (index+4) (replace (l!!(index+3)) 1 l ) ins outs)
    else runInstr (State h (index+4) (replace (l!!(index+3)) 0 l ) ins outs)
  | otherwise = State False 0 [] [] []
  where (code, mode1, mode2) = ((l!!index) `mod` 100, ((l!!index) `div` 100) `mod` 10, ((l!!index) `div` 1000) `mod` 10)

replace :: Int -> a -> [a] -> [a]
replace index value l = x ++ value:tail y where (x, y) = splitAt index l

part1 :: IO Int
part1 = part1' <$> parse

part1' :: [Int] -> Int
part1' l = maximum $ map (repeatInstr 5 0 l) (permutations [0..4])

repeatInstr :: Int -> Int -> [Int] -> [Int] -> Int
repeatInstr _ input _ [] = input
repeatInstr n input instrs (x:xs)  = repeatInstr (n-1) (last . instr [x, input] 0 $ instrs) instrs xs

part2 :: IO Int
part2 = part2' <$> parse

part2' :: [Int] -> Int
part2' l = maximum $ map (loopingInstr 0 . buildStates True l) (permutations [5..9])

buildStates :: Bool -> [Int] -> [Int] -> [State]
buildStates _ _ [] = []
buildStates True ls (x:xs) = State False 0 ls [x, 0] []:buildStates False ls xs
buildStates False ls (x:xs) = State False 0 ls [x] []:buildStates False ls xs

paramMode :: Int -> Int -> [Int] -> Int
paramMode 0 index l = l!!(l!!index)
paramMode 1 index l = l!!index
paramMode x _ _ = x

data State = State {halted :: Bool, i :: Int, c :: [Int], inputs :: [Int], outputs :: [Int]} deriving (Show)

loopingInstr :: Int-> [State] -> Int
loopingInstr index states = if all halted states then last $ inputs (states !! index) else let (curr, next) = computeStates (states!!index) (states!!nextIndex) in loopingInstr nextIndex (replace index curr . replace nextIndex next $ states)
  where nextIndex = (index+1) `mod` length states

computeStates :: State -> State -> (State, State)
computeStates first (State h1 i1 code1 inputs1 outputs1) = (State h2 i2 code2 inputs2 [] , State h1 i1 code1 (inputs1++outputs2) outputs1)
  where (State h2 i2 code2 inputs2 outputs2) = runInstr first

main :: IO ()
main = do
    -- parsed <- parse 
    -- let temp = buildStates True parsed [9]
    -- print temp
    -- let processed = runInstr (temp!!0)
    -- print processed
    (time1, result1) <- timeItT part1
    printf "The answer to Day 7 part 1 is: %-10d, calculated in: %.6f ms\n" result1 (time1*1000)
    (time2, result2) <- timeItT part2
    printf "The answer to Day 7 part 2 is: %-10d, calculated in: %.6f ms\n" result2 (time2*1000)