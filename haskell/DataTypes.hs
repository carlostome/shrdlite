module DataTypes where

import qualified Data.Map as M
import ShrdliteGrammar

type Id = String

data Goal = MoveObj Id Relation Id | TakeObj Id deriving (Eq, Show)
type Utterance = [String]
type World = [[Id]]
type Objects = M.Map Id Object
type Plan = [String]
type WorldState = (Maybe Id, World)
