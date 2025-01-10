filename = "data/d1.txt"

parse = (map read) . lines <$> readFile filename :: IO [Integer]

part1 = sum . (map fuel) <$> parse :: IO Integer 

part2 = sum . (map recursive_fuel) <$> parse :: IO Integer

fuel mass = mass `div` 3 - 2 :: Integer

recursive_fuel mass = if fuel mass <= 0 then 0 else fuel mass + (recursive_fuel . fuel) mass :: Integer

main = do 
    putStr "The answer to Day 1 part 1 is: "
    print =<< part1
    putStr "The answer to Day 1 part 2 is: "
    print =<< part2