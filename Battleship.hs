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
          toChar (Damaged n) = '!'

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
shoot (Damaged n) = Damaged n

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

locate :: (Int, Int) -> [[a]] -> Maybe a
locate (x,y) xs =
    if x >= 0 && y >= 0 && x < width && y < height then
        Just (xs !! x !! y)
    else
        Nothing
    where 
        width  = length xs
        height = length (head xs)

data GameState = GameState
    { board :: [[Piece]]
    , prompt :: String
    }

gameLoop :: GameState -> IO ()
gameLoop state = do
    beginTurn $ board state
    input <- getInput $ prompt state
    case read input of
        Exit -> do
            sendInfo "Thanks for playing!"
        Prompt s -> do
            sendInfo ("Prompt changed from \"" ++ prompt state ++ "\" to \"" ++ s ++ "\"")
            gameLoop $ state { prompt = s }
        Shoot (x,y) -> do
            let piece = locate (x,y) $ board state
            case piece of
                Nothing -> do
                    sendWarning (show (x,y) ++ " is not on the game board!")
                    gameLoop state
                Just p -> do
                    case p of
                        Ship n -> do
                            sendInfo ("Hit a ship at " ++ (show (x,y)))
                        Empty -> do
                            sendInfo ("Destroyed a tile at " ++ (show (x,y)))
                        Destroyed -> do
                            sendWarning ("You've already destroyed " ++ (show (x,y)))
                        Damaged n -> do
                            sendWarning ("You've already destroyed " ++ (show (x,y)))
                    gameLoop $ state { board = cmat (x,y) (shoot p) $ board state }          

main :: IO ()
main = do
    putStrLn ""
    putStrLn "+----------------------+"
    putStrLn "|      BATTLESHIP      |"
    putStrLn "+----------------------+"
    putStrLn ""
    gameLoop $ GameState { prompt = "Player", board = tboard }