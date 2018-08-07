module Main where
 
import Data.List
import Data.Char

data Player = O | B | X deriving ( Show, Eq, Ord )

size = 3 :: Int

type Grid = [[Player]]

cls :: IO()
cls = putStr "\ESC[2J"

type Pos = ( Int, Int )
goto :: Pos -> IO()
goto ( x,y )  = putStr ( "\ESC[" ++ show y ++ ";" ++ show x ++ "H" )

showplayer :: Player -> [String]
showplayer O = ["O"]
showplayer X = ["X"]
showplayer B = [" "]

row :: [Player] -> [[ String ]]
row [] = []
row ps = showplayer <$> ps

showrow :: [Player] -> [ String ]
showrow  =  map ( interleave '|' ) <$>  combine

combine :: [Player] -> [String]
combine = foldr1 ( zipWith ( ++ ) ) . row

interleave :: a -> [a] -> [a]
interleave a [] = []
interleave a [y] = [y]
interleave x ( y:ys ) = y : x : interleave x ys

showgrid :: Grid -> IO()
showgrid g =  putStr . unlines .  concat $ interleave line $ showrow <$> g
   where
      line = [replicate ( size * 2 ) '-']

initial :: Int -> Grid
initial n = replicate n ( replicate n B )

test = [[X, O, O], [B,O,X],[O, X, B]]
diag ::  Grid -> [Player]
diag g = [ g !!i !!j | i <- [0..size-1],j <- [0..size-1], i == j]

full :: Grid -> Bool
full g = [] == ( filter (==B ) . concat $ g )

won :: Grid -> Player -> Bool
won _ B = False
won g p
     | or $ all (==p) <$> g = True
     | or $ all (==p) <$> transpose g = True
     | all (==p) $ diag g = True
     | all (==p) . diag $ reverse <$> g = True
     | otherwise = False

next :: Player -> Player
next X = O
next O = X

valid :: Grid -> Int -> Bool
valid g i 
  | concat g !! i == B = True
  | otherwise = False

split :: Int -> [a] -> [[a]]
split _ [] = []
split n xs = take n xs : split n (drop n xs)

move :: Grid -> Player -> Int -> Grid
move g p i 
   | valid g i = split size $ take i l ++ [p] ++ drop (i+1) l
      where l = concat g

display :: Grid -> Player  -> IO()
display g p = do
   cls
   goto(1,4)
   showgrid  g
   putStrLn "\n\n"
   if won g (next p) then
                     putStrLn $ "Player " ++ show (next p) ++ " wins!"
                     else if full g then
                     putStrLn "It's a draw!"
                     else
                     play g p

getnum :: IO Int
getnum = do
   val <- getLine
   if [] /= val && all isDigit val then
                                   return (( read val ) - 1)
                                   else
                                   do putStrLn "Invalid number, Try again:"
                                      getnum

play :: Grid -> Player -> IO()
play g p = do
   putStrLn $ "player " ++ show p ++ "'s turn 1-9):"
   i <- getnum
   display ( move g p i ) ( next p ) 


main :: IO ()
main = display (initial 3) O

