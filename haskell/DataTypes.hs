module DataTypes where

import qualified Data.Map as M
import ShrdliteGrammar

type Id = String

data Goal = MoveObj Id Relation Id | TakeObj Id deriving (Eq, Show)
type Utterance = [String]
type World = [[Id]]
type Objects = M.Map Id Object
type Plan = [String]

getPositions :: [[Id]] -> M.Map Id (Int,Int)
getPositions = snd .
    foldl (\(cx,m) stack ->
          (cx+1,snd $ foldl (\(cy,m') elem ->
                      (cy+1,M.insert elem (cx,cy) m)) (length stack,m) stack))
    (0,M.empty)
