module Main where

data Player = O | B | X deriving ( Show, Eq, Ord )

size = 3 :: Int

type Grid = [[Player]]

cls :: IO()
cls = putStr "\ESC[2J"

type Pos = ( Int, Int )
goto :: Pos -> IO()
goto ( x,y )  = putStr ( "\ESC[" ++ show y ++ ";" ++ show x ++ "H" )

initial :: Int -> Grid
initial n = replicate n ( replicate n X )

showplayer :: Player -> [String]
showplayer O = [" ", "O", " "]
showplayer X = [" ", "X", " "]
showplayer B = [" ", " ", " "]

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


main :: IO ()
main = do
   cls
   goto(1,1)
   showgrid $ initial 3

