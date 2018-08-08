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

data Tree a = Tree a [Tree a]
   deriving (Show, Eq, Ord)
   
instance Functor Tree where
   fmap f (Tree a []) = Tree (f a) []
   fmap f (Tree a xs) = Tree (f a) [fmap f x | x <- xs]

gridtree :: Grid -> Player -> Tree Grid
gridtree g p 
   | full g = Tree g []
   | won g (next p) = Tree g []
   | otherwise = Tree g [ gridtree ( move g p i ) (next p) | i <- [0..size^2-1], valid g i]

prune :: Int -> Tree a -> Tree a
prune n ( Tree x xs )
  | n == 0 = Tree x []
  | null xs = Tree x []
  | otherwise = Tree x [prune (n-1) t | t <- xs]

apply :: Tree (Player,Grid) -> (Player,Grid)
apply ( Tree g _ ) = g

player :: Tree (Player,Grid) -> Player
player (Tree g _) = fst g

nextmoves :: Tree (Player,Grid) -> [ (Player, Grid) ]
nextmoves (Tree _ []) = []
nextmoves ( Tree _ xs ) = apply <$> xs

turn :: Grid -> Player
turn g 
  | (length $ filter (/= B) ( concat g )) `mod` 2 == 0 = O
  | otherwise = X

minmax :: Tree Grid -> Tree (Player,Grid)
minmax (Tree g []) 
  | won g O = Tree (O,g) []
  | won g X = Tree (X,g) []
  | otherwise = Tree (B,g) []

minmax (Tree g xs) 
  | turn g == X = Tree (maximum $ player <$> [ minmax x | x <- xs],g) [ minmax x | x<- xs]
  | otherwise = Tree (minimum $ player <$> [ minmax x | x <- xs],g) [ minmax x | x<- xs]

getnum :: IO Int
getnum = do
   val <- getLine
   if [] /= val && all isDigit val then
                                   do x <- return (read val - 1)
                                      if x > 0 && x < ( size^2 ) then
                                          return x
                                       else
                                          do putStrLn "Invalid number, Try again:"
                                             getnum
   else
      do putStrLn "Invalid number, Try again:"
         getnum

display :: Grid -> Player  -> IO()
display g p = do
   cls
   goto(1,4)
   showgrid  g
   if won g (next p) then
                     putStrLn $ "\n\nPlayer " ++ show (next p) ++ " wins!"
                     else if full g then
                     putStrLn "It's a draw!"
                     else
                     play g p


play :: Grid -> Player -> IO()
play g X = do
   putStrLn "Computer is thinking..."
   display (snd $ maximum x) O
      where x = nextmoves $  minmax $  gridtree g X

play g p = do
   putStrLn $ "player " ++ show p ++ "'s turn 1-9):"
   i <- getnum
   if valid g i then
      display ( move g p i ) ( next p ) 
   else
      do
         putStrLn "Invalid number, try again: "
         play g p


main :: IO ()
main =  display ( initial 3 ) O

