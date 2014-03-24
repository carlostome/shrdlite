module Plan where

import Data.Set as Set
import Data.Hashable as Hash

isSolution :: Goal -> World -> Bool
isSolution = undefined

actions :: World -> [Action]
actions = undefined

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
