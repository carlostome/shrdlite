module Plan where

import DataTypes
import qualified Data.Set as Set
import qualified Data.Hashable as Hash

isSolution :: Goal -> World -> Bool
isSolution = undefined


-- | Finds all the numbers of the stacks with elements.
stacksWithElements :: WorldState -> [Integer]
stacksWithElements (_, world) = map (\(s,n) -> n) $ filter (\(s, n) -> length s > 0) $ zip world [1..]

actions :: WorldState -> [Action]
actions (Nothing, world) = map Take stacksWithElements
actions (Just, world)    = map Drop [1..numberOfColumns] ++ map Take stacksWithElements
  where
    numberOfColumns = length world


transition :: World -> Action -> World
transition = undefined
          
plan :: World -> Goal -> Maybe [Action]
plan world goal = go [(world,[])] Set.empty
  where
    go []                              = Nothing
    go [(world,actions):rest] visited
       | isSolution goal world         = Just actions
       | otherwise = go $ rest ++ (filterVisited . mapActions . actions) world
       where
         filterVisited = filter (\(w,a) -> Hash.hash w `Set.notMember` visited)
         mapActions    = map (\a -> (transition world act,actions ++ [act]))
