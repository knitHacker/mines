module Main (main) where

import System.Random
import Text.Read (readMaybe)

import Types
import Mines
import AI

main :: IO ()
main = runGame

runGame :: IO ()
runGame = do
    let gen = mkStdGen 2025
--    gen <- initStdGen
    let width = 20
        height = 10
        mineNum = 40
        (gen', boardM) = generateBoard gen mineNum width height
    case boardM of
        Nothing -> putStrLn "Couldn't generate grid"
        Just b -> do
            putStrLn "Starting Mines"
            guessing $ Game gen' b mineNum RevealMode

guessing :: Game -> IO ()
guessing g@(Game _ b c m) = do
    putStrLn $ "Mode: " ++ show m
    let flagCnt = flagCount b
    putStrLn $ "Flags " ++ show flagCnt ++ "/" ++ show c
    print b
    let haveWon = checkWin b
    if haveWon
        then putStrLn "You WIN!!!!"
        else nextGuess g

nextGuess :: Game -> IO ()
nextGuess g = do
    line <- getLine
    let input = readMaybe line :: Maybe (Int, Int)
    case (input) of
        Nothing -> parseInput g line
        (Just (x, y)) -> executeSelect g (x, y)

parseInput :: Game -> String -> IO ()
parseInput g@(Game r b c _) inputs
    | "exit" == inputs = putStrLn "Goodbye"
    | "F" == inputs = guessing (Game r b c FlagMode)
    | "X" == inputs = guessing (Game r b c RevealMode)
    | "" == inputs = aiGuess g
    | otherwise = continue g "Input is either exit, F, X, or a coordinate in (x,y) format"

continue :: Game -> String -> IO ()
continue g str = do
    putStrLn str
    guessing g


executeSelect :: Game -> (Int, Int) -> IO ()
executeSelect game@(Game _ b@(Board (w, h) _ _ _) _ m) (x, y)
    | x < 0 || x >= w || y < 0 || y >= h = continue game "Coordinate was out of bounds"
    | otherwise = do
        let state = getCellState x y b
        case (state, m) of
            (Flag, RevealMode) -> continue game "Can't reveal flag in this mode"
            (Flag, FlagMode) -> doAction game $ RemoveFlag x y
            (Revealed, FlagMode) -> continue game "Already revealed"
            (Revealed, RevealMode) -> doAction game $ RevealNum x y
            (Hidden, RevealMode) -> doAction game $ RevealHidden x y
            (Hidden, FlagMode) -> doAction game $ PlaceFlag x y

doAction :: Game -> Move -> IO ()
doAction (Game r b c m) move = do
    putStrLn msg
    case bE of
        Left b' -> guessing $ Game r b' c m
        Right b' -> do
            print b'
            putStrLn "MINE!\nSorry you lose"
    where
        (msg, bE) = makeMove move b

aiGuess :: Game -> IO ()
aiGuess (Game r b c m) = do
    putStrLn "AI Making a move"
    let (msg, r', mv) = getAIMove r b
    putStrLn msg
    doAction (Game r' b c m) mv
