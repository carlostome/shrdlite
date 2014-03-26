module Plan where

import DataTypes
import qualified Data.Set as Set
import qualified Data.Hashable as Hash

data Action = Drop Int | Take Int
data WorldState = WState { holding :: Maybe Id,
                           positions :: Map Id (Int, Int),
			   world :: World
			 }

-- | Finds all the numbers of the stacks with elements.
stacksWithElements :: WorldState -> [Integer]
stacksWithElements (_, world) = map snd $ filter (\(s, n) -> length s > 0) $ zip world [1..]

-- | Calculates all the possible actions to take in the current world.
actions :: WorldState -> [Action]
actions (Nothing, world) = map Take stacksWithElements
actions (Just currentObj, world)  = map Drop validStacksToDropOn
  where
    numberOfColumns = length world
    validStacksToDropOn = map snd $ filter (\(item, n) -> currentObj `canBeOn` n) $ zip (map head world) [1..]
    canBeOn _ "Floor" = True
    canBeOn id id2 | isBall id  = isBox id2 -- Or is floor, but that's checked beforehand
                   | isBall id2 = False
                   | id `isLargerThan` id2 = False
                   | isBox id2  = not isPiramid id || not isPlank id || id2 `isLargerThan` id
                   | isBox id   = (isBox id2 && id `sameSize` id2) || (isLarge id && isBrick id2 && isLarge id2)
                   -- Last physical law
                   | otherwise = True
    isBall id = undefined
    isBrick id = undefined
    isPiramid id = undefined
    isPlank id = undefined
    isBox id = undefined
    isLargerThan = undefined
    sameSize = undefined
    isLarge = undefined


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
