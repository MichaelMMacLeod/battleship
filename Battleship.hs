module Battleship where

import Data.List (intersperse)

data Tile 
    = Empty 
    | Destroyed Int 
    | Ship Int

data Piece = Piece 
    { tile    :: Tile
    , visible :: Bool }

data Action
    = Exit 
    | Shoot (Int, Int) 
    | Prompt String 
    deriving (Read)

tboard =
    [   [   Piece (Ship 0) False,   Piece (Ship 0) False,   Piece (Ship 0) False,      Piece Empty False],
        [      Piece Empty False,      Piece Empty False,      Piece Empty False,   Piece (Ship 1) False],
        [      Piece Empty False,      Piece Empty False,      Piece Empty False,   Piece (Ship 1) False],
        [   Piece (Ship 2) False,   Piece (Ship 2) False,      Piece Empty False,   Piece (Ship 1) False]   ]

makeBoardStrings :: [[Piece]] -> [String]
makeBoardStrings xs = map (map toChar) xs
    where toChar :: Piece -> Char
          toChar (Piece _             False) = '#'
          toChar (Piece (Ship n)      _)     = '!'
          toChar (Piece (Destroyed n) _)     = head $ show n
          toChar (Piece Empty         _)     = ' '

formatBoardStrings :: [String] -> String
formatBoardStrings b = unlines $ xnums : border : middle ++ [border]
    where width  = length (head b)
          xnums  = "    " ++ concat [show x ++ " " | x <- [0..width - 1]]
          border = "  +" ++ take (width * 2 + 1) (cycle ['-']) ++ "+"
          middle = [show n ++ " | " ++ intersperse ' ' s ++ " |" | (n,s) <- zip [0..] b]

putBoard :: [[Piece]] -> IO ()
putBoard = putStr . formatBoardStrings . makeBoardStrings

replace :: (Int, Int) -> a -> [[a]] -> [[a]]
replace (r,c) x' xs = take c xs ++ [replaceRow c x' (xs !! r)] ++ drop (r + 1) xs
    where replaceRow c x' xs = take c xs ++ [x'] ++ drop (c + 1) xs

shoot :: Piece -> Piece
shoot p = p { visible = True }

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
                        Piece _ True -> do
                            sendWarning ("You've already destroyed " ++ (show (x,y)))
                        Piece (Ship n) _ -> do
                            sendInfo ("Hit a ship at " ++ (show (x,y)))
                        Piece Empty _ -> do
                            sendInfo ("Destroyed a tile at " ++ (show (x,y)))
                        Piece (Destroyed n) _ -> do
                            sendWarning ("You've already destroyed " ++ (show (x,y)))
                    gameLoop $ state { board = replace (x,y) (shoot p) $ board state }

main :: IO ()
main = do
    putStrLn ""
    putStrLn "+----------------------+"
    putStrLn "|      BATTLESHIP      |"
    putStrLn "+----------------------+"
    putStrLn ""
    gameLoop $ GameState { prompt = "Player", board = tboard }