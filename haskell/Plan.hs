module Plan (plan) where

import DataTypes
import ShrdliteGrammar
import qualified Data.Set as Set
import qualified Data.Hashable as Hash
import qualified Data.Map as M
import Data.Maybe


data Action = DropA Int | TakeA Int
            
data WorldState = WState { holding :: Maybe Id,
                           positions :: M.Map Id (Int, Int),
                           world :: World,
                           objectsInfo :: M.Map Id Object
			 }

-- | Finds all the numbers of the stacks with elements.
stacksWithElements :: WorldState -> [Int]
stacksWithElements worldState = map snd $ filter (\(s, n) -> length s > 0)
                                $ zip (world worldState) [1..]


-- | Calculates all the possible actions to take in the current world.
actions :: WorldState -> [Action]
actions ws@(WState Nothing _ world info)         = map TakeA (stacksWithElements ws)
actions (WState (Just currentObj) _ world info)  = map DropA validStacksToDropOn
  where
    validStacksToDropOn = map snd $ filter (\(item, n) -> currentObj `canBeOn` n) $ zip (map headOrFloor world) [1..]
    canBeOn _ "Floor" = True
    canBeOn id id2 | id `isLargerThan` id2 = False
                   | isBall id  = isBox id2 -- Or is floor, but that's checked beforehand
                   | isBall id2 = False
                   | isBox id2  = not (isPyramid id) || not (isPlank id) || id2 `isLargerThan` id
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
					Beside -> abs (x1 - x2) == 1
					Leftof -> x1 < x2
					Rightof -> x1 > x2
					Above -> y1 > y2
					Ontop -> y1 - y2 == 1
					Inside -> y1 - y2 == 1
					Under -> y1 < y2
			          where Just (x1, y1) = M.lookup id (positions worldState)
					Just (x2, y2) = M.lookup id2 (positions worldState)
		TakeObj id -> 
			case holding worldState of
				Just id2 -> id == id2
				Nothing -> False

-- Apply an action to a world and get a new world
transition :: WorldState -> Action -> WorldState
transition worldState action =
  case action of
    TakeA n ->
      let id = head (world worldState !! n)
      in
        WState
        (Just id)
        (positions worldState)
        (take n (world worldState) ++ [tail (world worldState !! n)]
	 ++ drop (n + 1) (world worldState))
        (objectsInfo worldState)
    DropA n -> 
      let Just id = holding worldState
      in
        WState
	Nothing
	(M.insert id (n, length $ world worldState !! n) (positions worldState))
	(take n (world worldState) ++ [id : head (drop n $ world worldState)]
	        ++ drop (n + 1) (world worldState))
        (objectsInfo worldState)
                

-- Bfs on the tree of worlds
plan :: World -> Maybe Id -> Objects -> Goal -> Maybe Plan
plan world holding objects goal = go [(initialWorld,[])] Set.empty
  where
  	initialWorld = WState holding undefined world objects
        go []  _                          = Nothing
        go ((world,oldActions):rest) visited
          | isSolution goal world         = Just (map show oldActions)
          | otherwise = go (rest ++ filterVisited (mapActions newActions)) visited
          where
            filterVisited = filter (\(w,a) -> Hash.hash w `Set.notMember` visited)
            mapActions    = map (\act -> (transition world act,oldActions ++ [act]))
            newActions     = actions world
