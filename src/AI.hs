module AI
    ( guess
    ) where

import System.Random

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import qualified Data.Array as A
import Data.Array ((!))
import qualified Data.List as L


import Types
import Mines

guess :: RandomGen g => g -> Board -> (String, Board)
guess rand b@(Board (w, h) minPos grid display) = undefined
--    | length revealed == 0 = randomGuess
--    where
--        revealed = getAllRevealed b


-- randomGuess :: Board -> 

--checkCanFlag :: Board -> Maybe (Int, Int)
--checkCanFlag (Board (w, h) minPos grid display) = undefined
