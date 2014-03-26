module Plan where

import DataTypes
import qualified Data.Set as Set
import qualified Data.Hashable as Hash

data Action = Drop Int | Take Int

isSolution :: Goal -> World -> Bool
isSolution = undefined


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
