main = do  
    contents <- readFile "input.txt" 
    putStrLn  . show $ getIncreases contents
  
getIncreases :: String -> Int  
getIncreases input = numberOfIncreases . map read $ lines input

numberOfIncreases :: [Int] -> Int
numberOfIncreases []  = 0
numberOfIncreases [x] = 0
numberOfIncreases (x:xs)
 | x < y = 1 + numberOfIncreases xs
 | otherwise = numberOfIncreases xs
 where y = head xs