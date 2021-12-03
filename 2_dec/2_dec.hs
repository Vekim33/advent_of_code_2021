import Data.Char(digitToInt)

data Direction = Forward | Up | Down deriving (Show)
type Position  = Int
type Aim       = Int

main = do
 content <- readFile "input.txt"
 let commands = getCommand $ lines content
     (h, d)      = move (0, 0) commands
     (h', d', a) = move' (0, 0, 0) commands
 putStrLn $ 
  "Submarine's horizontal position: " ++ show h ++ "\n"
  ++ "Submarine's depth (Part 1): " ++ show d ++ "\n"
  ++ "Submarine's product: " ++ "\n"
  ++ "Part 1: " ++ show (h * d) ++ "\n"
  ++ "Part 2: " ++ show (h' * d')

getCommand :: [String] -> [(Direction, Int)]
getCommand = map stringToTuple

stringToTuple :: String -> (Direction, Int)
stringToTuple s   = (horizontal, depth)
 where horizontal = show' . head $ words s
       depth      = digitToInt $ last s

show' :: String -> Direction
show' "forward" = Forward
show' "up"      = Up
show' "down"    = Down
show' _         = error "Not a direction"

move :: (Position, Position) -> [(Direction, Int)] -> (Position, Position)
move (h, d) []     = (h, d)
move (h, d) (x:xs) =
 case dir of Forward -> move (h + n, d) xs
             Up      -> move (h, d - n) xs
             Down    -> move (h, d + n) xs
 where (dir, n) = x

move' :: (Position, Position, Aim) ->[(Direction, Int)] -> (Position, Position, Aim)
move' (h, d, a) []     = (h, d, a)
move' (h, d, a) (x:xs) =
 case dir of Forward -> move' (h + n, d + a * n, a) xs
             Up      -> move' (h, d, a - n) xs
             Down    -> move' (h, d, a + n) xs
 where (dir, n) = x 

