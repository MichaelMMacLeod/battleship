module Battleship where

import Data.List (intersperse, foldl')

data Piece 
    = Empty 
    | Destroyed 
    | Ship Int
    | Damaged Int
    deriving (Show, Eq)

data Action
    = Exit
    | Shoot (Int, Int)
    | Prompt String
    deriving (Read)

-- tboard = [[Empty | r <- [1..9]] | c <- [1..9]]

tboard =
    [   [   Ship 0, Ship 0, Ship 0, Ship 0, Ship 0, Empty,  Empty,  Empty,  Empty   ],
        [   Empty,  Empty,  Empty,  Empty,  Empty,  Empty,  Empty,  Empty,  Empty   ],
        [   Empty,  Empty,  Empty,  Empty,  Empty,  Empty,  Empty,  Empty,  Empty   ],
        [   Empty,  Empty,  Empty,  Empty,  Empty,  Empty,  Empty,  Empty,  Empty   ],
        [   Empty,  Empty,  Empty,  Empty,  Empty,  Empty,  Empty,  Empty,  Empty   ],
        [   Empty,  Empty,  Empty,  Empty,  Empty,  Empty,  Empty,  Empty,  Empty   ],
        [   Empty,  Empty,  Empty,  Empty,  Empty,  Empty,  Empty,  Empty,  Empty   ],
        [   Empty,  Empty,  Empty,  Empty,  Empty,  Empty,  Empty,  Empty,  Empty   ],
        [   Empty,  Empty,  Empty,  Empty,  Empty,  Empty,  Empty,  Empty,  Empty   ]   ]

makeBoardStrings :: [[Piece]] -> [String]
makeBoardStrings xs = map (map toChar) xs
    where toChar :: Piece -> Char
          toChar Empty = '#'
          toChar Destroyed = ' '
          toChar (Ship n) = '#'
          toChar (Damaged n) = 'O'

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

-- replace a value in a 2d list
cmat :: (Int, Int) -> a -> [[a]] -> [[a]]
cmat (i,j) p xs = take i xs ++ [crow j p (xs !! i)] ++ drop (i+1) xs

shoot :: Piece -> Piece
shoot Destroyed = Destroyed
shoot Empty = Destroyed
shoot (Ship n) = Damaged n

hitcheck :: Piece -> Piece -> Bool
hitcheck (Ship n) Destroyed = True
hitcheck _ _ = False

hitr :: [Piece] -> [Piece] -> Bool
hitr r r' = foldl' (||) False (zipWith hitcheck r r')

hitm :: [[Piece]] -> [[Piece]] -> Bool
hitm m m' = foldl' (||) False (zipWith hitr m m')

sendInfo :: String -> IO ()
sendInfo s = putStrLn $ "info> " ++ s

sendWarning :: String -> IO ()
sendWarning s = putStrLn $ "Warning> " ++ s

beginTurn :: [[Piece]] -> IO ()
beginTurn b = do
    putBoard b
    sendInfo "Commands: Shoot (x,y) | Exit | Prompt \"Your Name\""

getInput :: String -> IO String
getInput prompt = do
    putStr (prompt ++ "> ")
    getLine

gameLoop :: String -> [[Piece]] -> IO ()
gameLoop prompt board = do
    beginTurn board
    input <- getInput prompt
    case read input of
        Exit -> sendInfo "Thanks for playing!"
        Prompt s -> do
            sendInfo ("Prompt changed from \"" ++ prompt ++ "\" to \"" ++ s ++ "\"")
            gameLoop s board
        Shoot (x,y) -> do
            if (x,y) `isIn` board then do
                let piece = board !! x !! y
                case piece of
                    Ship n    -> sendInfo ("Hit a ship at " ++ (show (x,y)))
                    Empty     -> sendInfo ("Destroyed a tile at " ++ (show (x,y)))
                    Destroyed -> sendWarning ("You've already destroyed " ++ (show (x,y)))
                let board' = cmat (x,y) (shoot piece) board
                gameLoop prompt $ board'
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