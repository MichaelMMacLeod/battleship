module Board 
    ( Tile (..)
    , Piece (..)
    , Board
    , locate
    , replace
    , shoot
    , put
    ) where

import Data.List (intersperse)

-- Datatypes

data Tile
    = Empty
    | Destroyed Int
    | Ship Int

data Piece = Piece
    { tile    :: Tile
    , visible :: Bool
    }

type Board = [[Piece]]

-- Management

locate :: (Int, Int) -> [[a]] -> Maybe a
locate (x,y) xs =
    if x >= 0 && y >= 0 && x < width && y < height then
        Just (xs !! x !! y)
    else
        Nothing
    where 
        width  = length xs
        height = length (head xs)

replace :: (Int, Int) -> a -> [[a]] -> [[a]]
replace (r,c) x' xs = take c xs ++ [replaceRow c x' (xs !! r)] ++ drop (r + 1) xs
    where replaceRow c x' xs = take c xs ++ [x'] ++ drop (c + 1) xs

shoot :: Piece -> Piece
shoot p = p { visible = True }

-- Display

token :: Piece -> Char
token (Piece _             False) = '#'
token (Piece (Ship n)      _)     = '!'
token (Piece (Destroyed n) _)     = head $ show n
token (Piece Empty         _)     = ' '

format :: Board -> String
format board = unlines $ xnums : border : middle ++ [border]
    where t      = map (map token) board
          width  = length (head t)
          xnums  = "    " ++ concat [show x ++ " " | x <- [0..width - 1]]
          border = "  +" ++ take (width * 2 + 1) (cycle ['-']) ++ "+"
          middle = [show n ++ " | " ++ intersperse ' ' s ++ " |" | (n,s) <- zip [0..] t]

put :: Board -> IO ()
put = putStr . format