module Main where

import System.TimeIt ( timeItT )
import Text.Printf ( printf )
import Data.Tree ( levels, Forest, Tree(Node) )
import Data.List ( delete, find )
import Data.List.Split ( splitOn )

filename :: String
filename = "data/d6.txt"

parse :: IO [(String, String)]
parse = map ((\x -> (head x, last x)) . splitOn ")") . lines <$> readFile filename

part1 :: IO Int
part1 = depths . makeTree "COM" <$> parse

part2 :: IO Int
part2 = stepsBetween "YOU" "SAN" . makeTree "COM" <$> parse

stepsBetween :: (Eq a) => a -> a -> Tree a -> Int
stepsBetween start end t = height start t + height end t - 2* height (common start end t) t - 2

height :: (Eq a) => a -> Tree a -> Int
height label t = height' label (levels t)

height' :: (Eq a) => a -> [[a]] -> Int
height' _ [] = 0
height' label (x:xs) = if label `elem` x then 0 else 1+height' label xs

common :: (Eq a) => a -> a -> Tree a -> a
common x y (Node root rest) = case find (\s -> hasNode x s && hasNode y s) rest of 
    Just a -> common x y a
    Nothing -> root

depths :: Tree a -> Int
depths = sum . zipWith (*) [0..] . map length . levels

makeTree :: (Eq a) => a -> [(a,a)] -> Tree a
makeTree root connections = head (addNodes [Node root []] connections )

addNodes :: (Eq a) => Forest a -> [(a,a)] -> Forest a
addNodes f [] = f
addNodes f ((parent,child):rest)
  | childIsRoot f child && hasParent f parent = addNodes (joinTrees f parent child) rest
  | not (childIsRoot f child) && hasParent f parent = addNodes (addLeaf f parent child) rest
  | childIsRoot f child && not (hasParent f parent) = addNodes (addAbove f parent child) rest
  | not (childIsRoot f child) && not (hasParent f parent) = addNodes (addNew f parent child) rest
  | otherwise = []

childIsRoot :: (Eq a) => Forest a -> a -> Bool
childIsRoot [] _ = False
childIsRoot (t:ts) child = root == child || childIsRoot ts child where (Node root _) = t

isRoot :: (Eq a) => a -> Tree a -> Bool
isRoot r (Node root _) = r == root

hasParent :: (Eq a) => Forest a -> a -> Bool
hasParent [] _ = False
hasParent (t:ts) parent = hasNode parent t || hasParent ts parent

hasNode :: (Eq a) => a -> Tree a -> Bool
hasNode label (Node root rest)  = root == label || any (hasNode label) rest

addNew :: Forest a -> a -> a -> Forest a
addNew f p c = Node p [Node c []] : f

addAbove :: (Eq a) => Forest a -> a -> a -> Forest a
addAbove [] _ _ = []
addAbove (t:ts) p c = if root == c then Node p [t]:ts else t:addAbove ts p c where (Node root _) = t

addLeaf :: (Eq a) => Forest a -> a -> a -> Forest a
addLeaf [] _ _ = []
addLeaf (t:ts) p c = if hasNode p t then addLeaf' p c t:ts else t:addLeaf ts p c

addLeaf' :: (Eq a) => a -> a -> Tree a -> Tree a
addLeaf' p c (Node root rest) = if root == p then Node root (Node c []:rest) else Node root (map (addLeaf' p c) rest)

joinTrees :: (Eq a) => Forest a -> a -> a -> Forest a
joinTrees f p c = findAndCombine (isRoot c) (hasNode p) (joinTrees' p) f

joinTrees' :: (Eq a) => a -> Tree a -> Tree a -> Tree a
joinTrees' label toAdd (Node root rest) = if root == label then Node root (toAdd:rest) else Node root (map (joinTrees' label toAdd) rest)

findAndCombine :: (Eq p) => (p -> Bool) -> (p -> Bool)  -> (p->p->p) -> [p] -> [p]
findAndCombine c1 c2 f xs = case (find c1 xs, find c2 xs) of
    (Just a, Just b) -> f a b:(delete a . delete b $ xs)
    (_,_) -> xs

main :: IO ()
main = do
    (time1, result1) <- timeItT part1
    printf "The answer to Day 6 part 2 is: %-10d, calculated in: %.6f ms\n" result1 (time1*1000)
    (time2, result2) <- timeItT part2
    printf "The answer to Day 6 part 2 is: %-10d, calculated in: %.6f ms\n" result2 (time2*1000)