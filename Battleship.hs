module Battleship where

import Board

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

sendInfo :: String -> IO ()
sendInfo s = putStrLn $ "info> " ++ s

sendWarning :: String -> IO ()
sendWarning s = putStrLn $ "Warning> " ++ s

beginTurn :: [[Piece]] -> IO ()
beginTurn b = do
    put b
    sendInfo "Commands: Shoot (x,y) | Exit | Prompt \"Your Name\""

getInput :: String -> IO String
getInput prompt = do
    putStr (prompt ++ "> ")
    getLine

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