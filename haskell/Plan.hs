module Plan where

import DataTypes
import ShrdliteGrammar
import qualified Data.Set as Set
import qualified Data.Hashable as Hash
import qualified Data.Map as M
import Data.Maybe

data Action = Drop Int | Take Int
data WorldState = WState { holding :: Maybe Id,
                           positions :: M.Map Id (Int, Int),
                           world :: World,
                           objectsInfo :: M.Map Id Object
			 }

-- | Finds all the numbers of the stacks with elements.
stacksWithElements :: WorldState -> [Integer]
stacksWithElements (_, world) = map snd $ filter (\(s, n) -> length s > 0) $ zip world [1..]

-- | Calculates all the possible actions to take in the current world.
actions :: WorldState -> [Action]
actions (WState Nothing _ world info)            = map Plan.Take stacksWithElements
actions (WState (Just currentObj) _ world info)  = map Plan.Drop validStacksToDropOn
  where
    validStacksToDropOn = map snd $ filter (\(item, n) -> currentObj `canBeOn` n) $ zip (map headOrFloor world) [1..]
    canBeOn _ "Floor" = True
    canBeOn id id2 | id `isLargerThan` id2 = False
                   | isBall id  = isBox id2 -- Or is floor, but that's checked beforehand
                   | isBall id2 = False
                   | isBox id2  = not isPyramid id || not isPlank id || id2 `isLargerThan` id
                   | isBox id   = id `sameSize` id2 && (isTable id2 || isPlank id2 || (isLarge id && isBrick id2))
                   | otherwise = True
    headOrFloor [] = "Floor"
    headOrFllor l  = head l
    isBall id = getForm id == Ball  
    isBrick id = getForm id == Brick
    isPyramid id = getForm id == Pyramid
    isPlank id = getForm id == Plank
    isBox id = getForm id == Box
    isTable id = getForm id == Table
    isLargerThan id id2 = go (getSize id) (getSize id2)
      where
        go Small _   = False
        go Large sz2 = sz2 == Small
    sameSize id id2 = getSize id == getSize id2
    isLarge id = getSize id == Large
    getObject id = fromJust $ M.lookup id info
    getForm id = let (Object _ _ form) = getObject id in form
    getSize id = let (Object sz _ _ )  = getObject id in sz


-- Checks if a given world satisfies a world
isSolution :: Goal -> WorldState -> Bool
isSolution goal worldState =
	case goal of
		MoveObj id rel id2 ->
			if isJust (holding worldState) then False
			else
				case rel of
					Beside -> abs (pos1 - pos2) == 1
					Leftof -> x1 < x2
					Rightof -> x1 > x2
					Above -> y1 > y2
					Ontop -> y1 - y2 == 1
					Inside -> y1 - y2 == 1
					Under -> y1 < y2
			          where Just (x1, y1) = lookup id (positions worldState)
					Just (x2, y2) = lookup id2 (positions worldState)
		TakeObj id -> 
			case holding worldState of
				Just id2 -> id1 == id2
				Nothing -> False

-- Apply an action to a world and get a new world
transition :: WorldState -> Action -> WorldState
transition = undefined
          
-- Bfs on the tree of worlds
plan :: World -> Goal -> Maybe [Action]
plan world goal = go [(world,[])] Set.empty
  where
    go []                              = Nothing
    go [(world,actions):rest] visited
       | isSolution goal world         = Just actions
       | otherwise = go $ rest ++ (filterVisited . mapActions . actions) world
       where
         filterVisited = filter (\(w,a) -> Hash.hash w `Set.notMember` visited)
         mapActions    = map (\act -> (transition world act,actions ++ [act]))
