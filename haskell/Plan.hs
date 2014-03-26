module Plan where

import DataTypes
import qualified Data.Set as Set
import qualified Data.Hashable as Hash

data WorldState = WState { holding :: Maybe Id,
						   positions :: Map Id (Int, Int),
						   world :: World
						 }

-- | Finds all the numbers of the stacks with elements.
stacksWithElements :: WorldState -> [Integer]
stacksWithElements (_, world) = map (\(s,n) -> n) $ filter (\(s, n) -> length s > 0) $ zip world [1..]

actions :: WorldState -> [Action]
actions (Nothing, world) = map Take stacksWithElements
actions (Just, world)    = map Drop [1..numberOfColumns] ++ map Take stacksWithElements
  where
    numberOfColumns = length world

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
