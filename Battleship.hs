module Battleship where

import Data.List (intersperse)

data Piece 
    = Empty 
    | Destroyed 
    | Ship Int 
    deriving (Show)

data Action
    = Exit
    | Shoot (Int, Int)
    | Prompt String
    deriving (Read)

tboard = [[Empty | r <- [1..9]] | c <- [1..9]]

makeBoardStrings :: [[Piece]] -> [String]
makeBoardStrings xs = map (map toChar) xs
    where toChar :: Piece -> Char
          toChar Empty = '#'
          toChar Destroyed = ' '
          toChar (Ship n) = '#'

formatBoardStrings :: [String] -> String
formatBoardStrings b = unlines $ xnums : border : middle ++ [border]
    where width  = length (head b)
          xnums  = "    " ++ concat [show x ++ " " | x <- [0..width - 1]]
          border = "  +" ++ take (width * 2 + 1) (cycle ['-']) ++ "+"
          middle = [show n ++ " | " ++ intersperse ' ' s ++ " |" | (n,s) <- zip [0..] b]

putBoard :: [[Piece]] -> IO ()
putBoard = putStr . formatBoardStrings . makeBoardStrings

-- determines if (x,y) is a valid location in xs
isIn :: (Int, Int) -> [[a]] -> Bool
isIn (x,y) xs = x >= 0 && y >= 0 && x < length xs && y < length (head xs)

-- replace a value in a list
crow :: Int -> a -> [a] -> [a]
crow i p xs = take i xs ++ [p] ++ drop (i+1) xs

-- replace a value in a 2d list "
cmat :: (Int, Int) -> a -> [[a]] -> [[a]]
cmat (i,j) p xs = take i xs ++ [crow j p (xs !! i)] ++ drop (i+1) xs

-- shoot a piece, replacing it with Destroyed
shoot :: (Int, Int) -> [[Piece]] -> [[Piece]]
shoot (x,y) xs = cmat (x,y) Destroyed xs

sendInfo :: String -> IO ()
sendInfo s = putStrLn $ "info> " ++ s

sendWarning :: String -> IO ()
sendWarning s = putStrLn $ "Warning> " ++ s

gameLoop :: String -> [[Piece]] -> IO ()
gameLoop prompt board = do
    putBoard board
    sendInfo "Commands: Shoot (x,y) | Exit | Prompt s"
    putStr (prompt ++ "> ")
    input <- getLine
    case read input of
        Exit -> sendInfo "Thanks for playing!"
        Prompt s -> do
            sendInfo ("Prompt changed from \"" ++ prompt ++ "\" to \"" ++ s ++ "\"")
            gameLoop s board
        Shoot (x,y) -> do
            if (x,y) `isIn` board then do
                sendInfo ("Destroyed tile " ++ show (x,y))
                gameLoop prompt $ shoot (x,y) board
            else do
                sendWarning (show (x,y) ++ " is not on the game board!")
                gameLoop prompt board

main :: IO ()
main = do
    putStrLn ""
    putStrLn "+----------------------+"
    putStrLn "|      BATTLESHIP      |"
    putStrLn "+----------------------+"
    putStrLn ""
    gameLoop "Player" tboard