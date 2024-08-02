module Types
    ( MineNum(..)
    , Cell(..)
    , CellState(..)
    , ShowCell(..)
    , Row
    , Grid
    , Board(..)
    , GameMode(..)
    , Game(..)
    , Move(..)
    , isZero
    , isFlagged
    , isHidden
    , isMine
    ) where

import System.Random

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import qualified Data.Array as A
-- import Data.Array ((!))
-- import qualified Data.List as L


showNum :: Int -> String
showNum n
    | length s == 1 = " " ++ s
    | otherwise = s
    where
        s = show n

data MineNum = Zero
             | One
             | Two
             | Three
             | Four
             | Five
             | Six
             | Seven
             | Eight
             deriving (Eq, Enum)

instance Show MineNum where
    show mn = case mn of
                Zero -> "  "
                _ -> showNum $ fromEnum mn

data Cell = Mine
          | Neighbors MineNum
          deriving (Eq)

instance Show Cell where
    show Mine = " X"
    show (Neighbors n) = show n

data CellState = Flag | Hidden | Revealed
               deriving (Show, Eq)

data ShowCell = ShowCell
    { cellInfo :: Cell
    , cellState :: CellState
    } deriving (Eq)

instance Show ShowCell where
    show (ShowCell c s) = case s of
                            Flag -> " F"
                            Hidden -> " H"
                            Revealed -> show c

type Row = A.Array Int Cell
type Grid = A.Array Int Row


data Move = PlaceFlag Int Int
          | RemoveFlag Int Int
          | RevealHidden Int Int
          | RevealNum Int Int
          deriving (Show, Eq)

data Board = Board
    { boardSize :: (Int, Int)
    , mines :: S.Set (Int, Int)
    , mineGrid :: Grid
    , showGrid :: M.Map Int (M.Map Int ShowCell)
    } deriving (Eq)

showListSpace :: [Int] -> String
showListSpace [] = "[]"
showListSpace ls = "[" ++ showList' ls ++ "]"

showList' :: [Int] -> String
showList' [] = ""
showList' (h:[]) = showNum h
showList' (h:tl) = showNum h ++ "," ++ showList' tl

instance Show Board where
    show (Board (w, _) _ _ g) = " C " ++ (showListSpace [0..w-1]) ++ "\n" ++ (concat $ showRow <$> M.assocs g)
        where
            showRow (rn, row) = (showNum rn) ++ " " ++ (show ( M.elems row)) ++ "\n"

data GameMode = FlagMode | RevealMode
              deriving (Show, Eq)

data Game = Game
    { rand :: StdGen
    , board :: Board
    , mineCount :: Int
    , mode :: GameMode
    } deriving (Show, Eq)

isZero :: Cell -> Bool
isZero (Neighbors Zero) = True
isZero _ = False

isFlagged :: ShowCell -> Bool
isFlagged (ShowCell _ Flag) = True
isFlagged _ = False

isHidden :: ShowCell -> Bool
isHidden (ShowCell _ Hidden) = True
isHidden _ = False

isMine :: ShowCell -> Bool
isMine (ShowCell Mine _) = True
isMine _ = False

