{-# LANGUAGE DeriveGeneric #-}
module Plan  where

import           DataTypes
import           ShrdliteGrammar

import qualified Data.Heap       as PQ
import qualified Data.Map        as M
import qualified Data.Set        as S

import           Data.Hashable
import           GHC.Generics    (Generic)

import           Data.List       (foldl')
import           Data.Maybe      (isJust, isNothing, mapMaybe)

-- | Action that can be performed.
data Action = DropA Int | TakeA Int


instance Show Action where
  show (DropA n) = "drop " ++ show n
  show (TakeA n) = "pick " ++ show n

-- | WorldState for planning algorithm.
data WorldState = WState { holding     :: Maybe Id,
                           positions   :: M.Map Id (Int, Int),
                           world       :: World,
                           objectsInfo :: M.Map Id Object
			 } deriving (Generic)

instance Hashable WorldState where
  hashWithSalt s (WState holding _ world _) = s `hashWithSalt` holding
                                              `hashWithSalt` world


-- | Calculates all the possible actions to take in the current world.
actions :: WorldState -> [Action]
actions (WState holding _ world info) =
  case holding of
    Nothing     -> map (TakeA . snd) $ filter ((>0) . length . fst) $ zip world [0..]

    Just obj    -> map (DropA . snd) $ filter (canBeOn obj . fst) $
                   zip (map (\l -> if null l then "Floor" else head l) world)   [0..]
    where
    canBeOn _ "Floor" = True
    canBeOn id1 id2
      | size1 > size2 = False
      | form1 == Ball = form2 == Box -- Or is floor, but that's checked beforehand
      | form2 == Ball = False
      | form2 == Box  = not ((form1 == Box) || (form1 == Pyramid) || (form1 == Plank)) || size2 > size1
      | form1 == Box  = size1 == size2
                        &&    ( form2 == Table
                             || form2 == Plank
                             || (size1 == Large && form2 == Brick))
      | otherwise = True
      where
        Just (Object size1 _ form1) =  M.lookup id1 info
        Just (Object size2 _ form2) =  M.lookup id2 info



-- Checks if a given world satisfies a world
isSolution :: WorldState -> Goal -> Bool
isSolution worldState goal =
  case goal of
    MoveObj id rel id2 ->
      if isJust (holding worldState) then False
      else
        let Just (x1,y1) = M.lookup id (positions worldState)
            Just (x2,y2) =
              case id2 of
                "Floor" -> return (x1,0)
                _       -> M.lookup id2 (positions worldState)
	in
          relOK (x1, y1) (x2,y2)
      where
       relOK (x1, y1) (x2, y2) =
          case rel of
            Beside  -> abs (x1 - x2) == 1
            Leftof  -> x1 < x2
            Rightof -> x1 > x2
            Above   -> x1 == x2 && y1 > y2
            Ontop   -> x1 == x2 && y1 - y2 == 1
            Inside  -> x1 == x2 && y1 - y2 == 1
            Under   -> x1 == x2 && y1 < y2
    TakeObj id ->
      case holding worldState of
	Just id2 -> id == id2
	Nothing -> False

    And goals -> and $ map (isSolution worldState) goals
    Or goals  -> or  $ map (isSolution worldState) goals
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
           (removeFromStackN (world worldState) n)
           (objectsInfo worldState)
    DropA n ->
      let Just id = holding worldState
      in
        WState
	   Nothing
	   (M.insert id (n, length (world worldState !! n) + 1) (positions worldState))
           (addToStackN (world worldState) id n)
           (objectsInfo worldState)

    where
      removeFromStackN :: [[a]] -> Int -> [[a]]
      removeFromStackN stacks n = concat [ take n stacks
                                         , [tail $ stacks !! n]
                                         , drop (n+1) stacks ]

      addToStackN :: [[a]] -> a -> Int -> [[a]]
      addToStackN stacks elem n = concat [ take n stacks
                                         , [elem : stacks !! n]
                                         , drop (n + 1) stacks ]


heuristic :: WorldState -> Goal -> Int
heuristic worldState (And goals) =
  maximum $ map (heuristic worldState) goals
heuristic worldState (Or goals) =
  minimum $ map (heuristic worldState) goals
heuristic worldState (TakeObj _) = 0
heuristic worldState (MoveObj id1 rel id2) =
  case rel of
    Ontop -> hid1 + hid2
      where
        hid1 = let Just (x,y) = M.lookup id1 (positions worldState)
               in case holding worldState of
                 Nothing  ->  2 * (length (world worldState !! x) - y)

                 Just obj -> if obj == id1 then 1
                             else 2 * (length (world worldState !! x) - y)
                               
                 
        hid2 = if (id2 == "Floor") then
                 2 * (minimum $ map length (world worldState))
               else
                 let Just (x,y) = M.lookup id2 (positions worldState)
                 in 2 * (length (world worldState !! x) - y)
    Above -> hid1 + hid2
      where
        hid1 = let Just (x,y) = M.lookup id1 (positions worldState)
               in case holding worldState of
                    Nothing  ->  2 * (length (world worldState !! x) - y)
                                 
                    Just obj -> if obj == id1 then 1
                                else  2 * (length (world worldState !! x) - y)
        hid2 = if (id2 == "Floor") then 0
               else
                 let Just (x,y) = M.lookup id2 (positions worldState)
                 in 2 * (length $
                                takeWhile (\id ->
                                             id /= id2
                                           && not (validRelationship
                                                   (world worldState)
                                                   (objectsInfo worldState)
                                                   id Ontop id2))
                         (world worldState !! x))
         
    _ -> 2
         
cost :: WorldState -> Action -> Int
cost _ _ = 1

-- | Priority holding the heuristic and the cost
newtype Prio = Prio (Int,Int) deriving (Eq)

instance Ord Prio where
    compare (Prio (h1, c1)) (Prio (h2, c2)) = compare (h1+c1) (h2+c2)


-- | Bfs on the tree of worlds
plan :: World -> Maybe Id -> Objects -> Goal -> Maybe Plan
plan world holding objects goal = go initialQueue S.empty
  where
  	initialWorld     = WState holding (getPositions world) world objects
        initialQueue     = PQ.singleton (PQ.Entry (Prio (0,0))
                                                  (initialWorld,[]))

        go queue visited =
          case PQ.viewMin queue of
            Nothing  -> Nothing
            Just (PQ.Entry (Prio (_,oldCost)) (world,oldActions),rest) ->
                 if isSolution world goal then
                   Just (map show . reverse $ oldActions)
                 else
                   go (PQ.union rest (PQ.fromList newWorlds)) newVisited
                     where
                       newWorlds =
                         map (\(w,a) -> PQ.Entry
                                        (Prio ( heuristic w goal
                                              , cost w (head a) + oldCost))
                                        (w,a))
                          $ filter (\(w,_) -> hash w `S.notMember` visited)
                          $ zip (map (transition world) newActions)
                                (map (:oldActions) newActions)

                       newVisited    = foldl' (\v (PQ.Entry _ (w,_))
                                                 -> S.insert (hash w) v)
                                       visited newWorlds
                       newActions    = actions world
