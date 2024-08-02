module AI
    ( getAIMove
    ) where

import System.Random

-- import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.Maybe (catMaybes)


import Types
import Mines

import Debug.Trace


data InfoCell = InfoCell
    { xPos :: Int
    , yPos :: Int
    , cnt :: Int
    , hiddenNbs :: S.Set (Int, Int)
    , flaggedNbs :: S.Set (Int, Int)
    } deriving (Show, Eq)

getAIMove :: RandomGen g => g -> Board -> (String, g, Move)
getAIMove r b
    | length hidden == 0 = error "No hidden cells to reveal. todo try to remove wrong flag?"
    | length cellInfos == 0 = ("Making a random guess: " ++ show mv, r', mv)
    | otherwise =
        case (checkCanReveal b cellInfos, checkCanFlag b cellInfos, lastCheck) of
            (Just move, _, _) -> ("Making safe reveal move: " ++ show move, r, move)
            (Nothing, Just move, _) -> ("Making safe flag move: " ++ show move, r, move)
            (Nothing, Nothing, Just move) -> ("Making hopefully safe reveal move: " ++ show move, r, move)
            _ -> ("Making a random guess" ++ show mv, r', mv) -- educatedGuess b cellInfos
    where
        (r', mv) = randomGuess r b
        cellInfos = getAllMoveInfo b
        hidden = getAllHidden b
        sets = getApproxCellInfo cellInfos
        lastCheck = checkCanUseNeighbor b cellInfos sets

getAllMoveInfo :: Board -> [InfoCell]
getAllMoveInfo b = catMaybes $ getMoveInfo b <$> revealed
    where
        revealed = getAllRevealed b

getMoveInfo :: Board -> (Int, Int) -> Maybe InfoCell
getMoveInfo b@(Board (w, h) _ g _) (x, y) =
    case getCell x y g of
        Mine -> error $ "Revealed min can't occur: " ++ show (x, y)
        Neighbors Zero -> Nothing
        Neighbors c ->
            let c' = fromEnum c
            in if length hds == 0 then Nothing else Just $ InfoCell x y c' (S.fromList hds) (S.fromList fgs)
    where
        nbs = getNeighbors x y w h
        (hds, fgs) = foldl filterHidden ([], []) nbs

        filterHidden (hd, fg) p@(nx, ny) =
            case cellState $ getShowCell nx ny b of
                Flag -> (hd, p:fg)
                Hidden -> (p:hd, fg)
                _ -> (hd, fg)

getApproxCellInfo :: [InfoCell] -> [(Int, S.Set (Int, Int))]
getApproxCellInfo [] = []
getApproxCellInfo ((InfoCell x y c hidden flags):tl)
    | lHid > 3 || lHid < 2 = getApproxCellInfo tl
    | otherwise = trace (show ((x, y), n, hidden)) $ (n, hidden) : getApproxCellInfo tl
    where
        lHid = length hidden
        n = c - length flags


checkCanUseNeighbor :: Board -> [InfoCell] -> [(Int, S.Set (Int, Int))] -> Maybe Move
checkCanUseNeighbor _ [] _ = Nothing
checkCanUseNeighbor b (i@(InfoCell _ _ c hidden flags):tl) nbInfo =
    if length hidden == 0
        then checkCanUseNeighbor b tl nbInfo
        else case foldr getSets Nothing nbInfo of
                    Nothing -> checkCanUseNeighbor b tl nbInfo
                    mv -> mv
    where
        getSets _ j@(Just _) = j
        getSets (n, set) _
            | not (set `S.isSubsetOf` hidden) = Nothing
            | ll == 0 = Nothing
            | c - n - f == 0 = rpt "Reveal cell" $ Just $ RevealHidden hx hy
            | c - n - f == ll = rpt "Placing flag" $ Just $ PlaceFlag hx hy
            | otherwise = trace (show (i, n, set, leftover))  Nothing
            where
                rpt msg = trace (show (i, n, set, leftover, msg))
                leftover = S.difference hidden set
                (hx, hy) = S.findMin leftover
                f = length flags
                ll = length leftover


randomGuess :: RandomGen g => g -> Board -> (g, Move)
randomGuess r b@(Board (w, h) _ _ _) =
    case getCellState x y b of
        Hidden -> (r', RevealHidden x y)
        _ -> randomGuess r' b
    where
        ((x, y), r') = guess r w h

guess :: RandomGen g => g -> Int -> Int -> ((Int, Int), g)
guess g w h = ((x, y), g'')
    where
        (x, g') = randomR (0, w - 1) g
        (y, g'') = randomR (0, h - 1) g'


checkCanReveal :: Board -> [InfoCell] -> Maybe Move
checkCanReveal _ [] = Nothing
checkCanReveal b ((InfoCell ox oy c hidden flags):tl)
    | lHid == 0 = checkCanReveal b tl
    | c == lFlag = Just $ RevealNum ox oy
    | otherwise = checkCanReveal b tl
    where
        lFlag = length flags
        lHid = length hidden

checkCanFlag :: Board -> [InfoCell] -> Maybe Move
-- no flags found
checkCanFlag _ [] = Nothing
checkCanFlag b ((InfoCell _ _ c hidden flags):tl)
    | lHid == 0 = checkCanFlag b tl
    | c == lFlag + lHid = Just $ PlaceFlag hx hy
    | otherwise = checkCanFlag b tl
    where
        (hx, hy) = S.findMin hidden
        lHid = length hidden
        lFlag = length flags

educatedGuess :: Board -> [InfoCell] -> (String, Move)
educatedGuess b cells = undefined

{-
makeChances :: Board -> InfoCell -> [(Double, (Int, Int))]
makeChances _ (InfoCell _ _ 
makeChances b (InfoCell x y cnt hidden@((hx, hy):htl) flags) =
-}
