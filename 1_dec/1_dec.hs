main = do
    contents <- readFile "input.txt"
    putStrLn  $ "Number of increases: " ++ show (getIncreases contents)
     ++ "\n" ++ "Number of three sum increases: " ++ show (getThreeSumIncreases contents)

getIncreases :: String -> Int
getIncreases input = numberOfIncreases . map read $ lines input

getThreeSumIncreases :: String -> Int
getThreeSumIncreases input = numberOfIncreases $ threeMeasurementSum $ map read $ lines input

numberOfIncreases :: [Int] -> Int
numberOfIncreases []  = 0
numberOfIncreases [x] = 0
numberOfIncreases (x:xs)
 | x < y     = 1 + numberOfIncreases xs
 | otherwise = numberOfIncreases xs
 where y = head xs

threeMeasurementSum :: [Int] -> [Int]
threeMeasurementSum []  = []
threeMeasurementSum [x] = []
threeMeasurementSum (x:y:xs)
 | null xs = []
 | otherwise = (x + y + head xs) : threeMeasurementSum (y:xs)