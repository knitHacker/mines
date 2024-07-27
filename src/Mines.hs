module Mines
    ( revealCell
    , revealAllNearZeros
    , getCell
    , hiddenCell
    , flagCell
    , getCellState
    , flagCount
    , generateBoard
    , checkWin
    , tryExpandComplete
    ) where

import System.Random
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import qualified Data.Array as A
import Data.Array ((!))
import qualified Data.List as L

import Debug.Trace

import Types

generateMinPos :: RandomGen g => g -> Int -> Int -> Int -> (g, S.Set (Int, Int))
generateMinPos g m w h
    | m <= 0 = (g, S.empty)
    | otherwise = generateMinPos' g m w h S.empty

generateMinPos' :: RandomGen g => g -> Int -> Int -> Int -> S.Set (Int, Int) -> (g, S.Set (Int, Int))
generateMinPos' g m w h s
    | S.size s >= m = (g, s)
    | otherwise =
        let (x, g') = randomR (0, w - 1) g
            (y, g'') = randomR (0, h - 1) g'
        in generateMinPos' g'' m w h $ S.insert (x, y) s

getNeighbors :: Int -> Int -> Int -> Int -> [(Int, Int)]
getNeighbors x y maxX maxY = filter (\(a, b) -> a >= 0 && b >= 0 && a < maxX && b < maxY) neighbors
    where neighbors = [ (x - 1, y - 1)
                      , (x, y - 1)
                      , (x + 1, y - 1)
                      , (x - 1, y)
                      , (x + 1, y)
                      , (x - 1, y + 1)
                      , (x, y + 1)
                      , (x + 1, y + 1)
                      ]

genArray :: Int -> Int -> S.Set (Int, Int) -> Grid
genArray w h minePos = A.array (0, h-1) rows
    where
        rows = map (\r -> (r, genRow r w h minePos)) [0..h-1]

genRow :: Int -> Int -> Int -> S.Set (Int, Int) -> Row
genRow r width height minePos = A.array (0, width-1) cells
    where
        pos = [(x, r) | x <- [0..width-1]]
        cells = map (\(x, y) -> (x, genCell x y width height minePos)) pos

genCell :: Int -> Int -> Int -> Int -> S.Set (Int, Int) -> Cell
genCell x y maxX maxY minePos
    | S.member (x, y) minePos = Mine
    | otherwise =
        let n = getNeighbors x y maxX maxY
            cnt = foldl (\cn (nx, ny) -> if S.member (nx, ny) minePos then succ cn else cn) Zero n
        in Neighbors cnt

generateBoardMap :: Grid -> M.Map Int (M.Map Int ShowCell)
generateBoardMap grid = bm
    where
        innerFold row = foldl (\bd (cn, cell) -> M.insert cn (ShowCell cell Hidden) bd) M.empty $ A.assocs row
        bm = foldl (\b (rn, row) -> M.insert rn (innerFold row) b) M.empty $ A.assocs grid

generateBoard :: RandomGen g => g -> Int -> Int -> Int -> (g, Maybe Board)
generateBoard rGen mineCnt width height
    | mineCnt < 0 = (rGen, Nothing)
    | mineCnt >= width * height = (rGen, Nothing)
    | otherwise =
        let (rGen', minePos) = generateMinPos rGen mineCnt width height
            grid = genArray width height minePos
            bm = generateBoardMap grid
        in (rGen', Just (Board (width, height) minePos grid bm))

updateCell :: CellState -> Int -> Int -> Board -> Board
updateCell cs x y (Board s m g b) = (Board s m g (M.update updateRow y b))
    where
        updateRow row = Just $ M.update updateCell' x row
        updateCell' (ShowCell c _) = Just (ShowCell c cs)


revealCell :: Int -> Int -> Board -> Board
revealCell = updateCell Revealed

flagCell :: Int -> Int -> Board -> Board
flagCell = updateCell Flag

hiddenCell :: Int -> Int -> Board -> Board
hiddenCell = updateCell Hidden

getCell :: Int -> Int -> Grid -> Cell
getCell x y g = (g ! y) ! x

getShowCell :: Int -> Int -> Board -> ShowCell
getShowCell x y (Board _ _ _ m) = row M.! x
    where
        row = m M.! y

getCellState :: Int -> Int -> Board -> CellState
getCellState x y b = cellState $ getShowCell x y b

flagCount :: Board -> Int
flagCount (Board _ _ _ m) = M.foldl (\cnt row -> M.foldl (\cnt' cell  -> if isFlagged cell then cnt'+1 else cnt') cnt row) 0 m

revealAllNearZeros :: (Int, Int) -> Board -> Board
revealAllNearZeros z b@(Board (w, h) _ g _) = S.foldl (\bd (x, y) -> revealCell x y bd) b reveals
    where
        reveals = findAllNearZeros S.empty (S.singleton z) w h g

findAllNearZeros :: S.Set (Int, Int) -> S.Set (Int, Int) -> Int -> Int -> Grid -> S.Set (Int, Int)
findAllNearZeros searched new maxX maxY g =
    case S.lookupMin new of
        Nothing -> searched
        Just (x, y) ->
            let ns = getNeighbors x y maxX maxY
                ns' = filter (\z -> S.notMember z searched') ns
                (zs, os) = L.partition (\(r, c) -> isZero $ getCell r c g) ns'
                searched' = S.insert (x, y) searched
                searched'' = foldr S.insert searched' os
                new' = foldr S.insert new zs
                new'' = S.delete (x, y) new'
            in findAllNearZeros searched'' new'' maxX maxY g


checkWin :: Board -> Bool
checkWin (Board _ _ _ m) = M.foldl (\b row -> M.foldl (\b' cell -> isComplete cell && b') b row) True m
    where
        isComplete (ShowCell _ Revealed) = True
        isComplete (ShowCell Mine _ ) = True
        isComplete _ = False

-- TODO: need to expand zeros more
tryExpandComplete :: Board -> (Int, Int) -> Either Board Board
tryExpandComplete b@(Board (w, h) _ _ _) (x, y) =
    case cellInfo showCell of
        Mine -> Left b
        Neighbors Zero -> Left b
        Neighbors cnt ->
            let cnt' = fromEnum cnt
                nextFlags = foldl (\cnt'' cell -> if isFlagged cell then cnt'' + 1 else cnt'') 0 ns'
                isBad = foldl (\mB cell -> mB || (isMine cell && not (isFlagged cell))) False ns'
            in if cnt' /= nextFlags then Left b else if isBad then Right expanded else Left expanded
    where
        showCell = getShowCell x y b
        ns = getNeighbors x y w h
        ns' = map (\(x',y') -> getShowCell x' y' b) ns
        expanded = foldl expand b ns
        expand b' (x', y') =
            let cell = getShowCell x' y' b'
                shouldReveal = not $ isFlagged cell
            in if shouldReveal then revealCell x' y' b' else b'
