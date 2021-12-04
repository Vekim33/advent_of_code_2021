-- för att lista ut om det är flest 1 eller 0:
-- skapa en summa av varje position
-- totala antalet siffror i positionen - summan = antalet 0
-- om antal 0 < summan av 1:or -> 1, annars -> 0

import Data.Char

main = do
 contents <- readFile "input.txt"
 let input        = lines contents
     startingBits = replicate (length (head input)) 0
     gamma        = gammaRate (length input) $ bitSums startingBits input
     epsilon      = epsilonRate gamma
     bitLength    = length startingBits
     power        = binaryToDecimal gamma bitLength * binaryToDecimal epsilon bitLength
 putStrLn $ "The submarine's power consumption: " ++ show power

bitSums :: [Int] -> [String] -> [Int]
bitSums = foldl updateBitSum

updateBitSum :: [Int] -> String -> [Int]
updateBitSum _ []                = []
updateBitSum [] _                = []
updateBitSum (g:gs) (bit:bitmap) = (g + digitToInt bit) : updateBitSum gs bitmap

gammaRate :: Int -> [Int] -> [Bool]
gammaRate _ []     = []
gammaRate n (x:xs)
 | numOfZeroes <= x = True : gammaRate n xs
 | otherwise        = False : gammaRate n xs
 where numOfZeroes  = n - x

epsilonRate :: [Bool] -> [Bool]
epsilonRate bits = [not x | x <- bits]

binaryToDecimal :: [Bool] -> Int -> Int
binaryToDecimal [] _     = 0
binaryToDecimal (x:xs) n 
 | n == 0    = if x then 1 else 0
 | not x     = 0 * 2^(n - 1) + binaryToDecimal xs (n - 1)
 | otherwise = 1 * 2^(n - 1) + binaryToDecimal xs (n - 1)







