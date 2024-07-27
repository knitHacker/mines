module Main (main) where

import System.Random
import Text.Read (readMaybe)

import Types
import Mines

main :: IO ()
main = runGame

runGame :: IO ()
runGame = do
--    let gen = mkStdGen 2025
    gen <- initStdGen
    let width = 20
        height = 10
        mineNum = 20
        (_, boardM) = generateBoard gen mineNum width height
    case boardM of
        Nothing -> putStrLn "Couldn't generate grid"
        Just b -> do
            putStrLn "Starting Mines"
            guessing $ Game b mineNum RevealMode

guessing :: Game -> IO ()
guessing g@(Game b c m) = do
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
parseInput g@(Game b c _) inputs
    | "exit" == inputs = putStrLn "Goodbye"
    | "F" == inputs = guessing (Game b c FlagMode)
    | "X" == inputs = guessing (Game b c RevealMode)
    | otherwise = continue g "Input is either exit, F, X, or a coordinate in (x,y) format"

continue :: Game -> String -> IO ()
continue g str = do
    putStrLn str
    guessing g


executeSelect :: Game -> (Int, Int) -> IO ()
executeSelect game@(Game b@(Board (w, h) _ _ _) _ m) (x, y)
    | x < 0 || x >= w || y < 0 || y >= h = continue game "Coordinate was out of bounds"
    | otherwise = do
        let state = getCellState x y b
        case (state, m) of
            (Flag, RevealMode) -> continue game "Can't reveal flag in this mode"
            (Flag, FlagMode) -> removeFlag game (x, y)
            (Revealed, FlagMode) -> continue game "Already revealed"
            (Revealed, RevealMode) -> tryExpand game (x, y)
            (Hidden, RevealMode) -> revealHidden game (x, y)
            (Hidden, FlagMode) -> addFlag game (x, y)

addFlag :: Game -> (Int, Int) -> IO ()
addFlag (Game b c m) (x, y) = do
    let b' = flagCell x y b
    guessing $ Game b' c m

removeFlag :: Game -> (Int, Int) -> IO ()
removeFlag (Game b c m) (x, y) = do
    let b' = hiddenCell x y b
    guessing $ Game b' c m

revealHidden :: Game -> (Int, Int) -> IO ()
revealHidden (Game b@(Board _ _ g _) c m) (x, y) =
        case getCell x y g of
            Mine -> putStrLn "MINE!\nSorry you lose"
            Neighbors Zero -> do
                let b' = revealAllNearZeros (x, y) b
                putStrLn "Reveal all 0 neighbors"
                guessing $ Game b' c m
            Neighbors cnt -> do
                let b' = revealCell x y b
                guessing $ Game b' c m


tryExpand :: Game -> (Int, Int) -> IO ()
tryExpand (Game b@(Board _ _ _ _) c m) pos =
    case bM of
        (Left b') -> guessing $ Game b' c m
        (Right b') -> do
            print b'
            putStrLn "MINE! \nSorry you lose"
    where
        bM = tryExpandComplete b pos
