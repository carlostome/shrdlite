module Interpreter where 

import DataTypes
import ShrdliteGrammar

import Data.Maybe (fromJust)
import qualified Data.Map as M

import Data.List
-- | Finds all the objects matching a given description.
findObjects :: Object -> World -> Objects -> [Id]
findObjects _ [] _              = []
findObjects objQ (x:xs) objInfo = searchInStack objQ x ++ findObjects objQ xs objInfo
  where
    searchInStack _              [] = []
    searchInStack queryObj (objId:xs) = 
      if queryObj ~== (fromJust $ M.lookup objId objInfo)
        then objId : searchInStack queryObj xs
        else searchInStack queryObj xs 

-- | Finds all the ids of the objects matching the given criteria.
findEntities :: Entity -> World -> Objects -> [Id]
findEntities (BasicEntity quantifier qObj) wrld objcts =
  findObjects qObj wrld objcts

findEntities (RelativeEntity quantifier qObj loc) wrld objcts = 
  [ id1  | (rel,id2) <- matchingLocations, id1 <- matchingObjects
  , validRelationship wrld objcts id1 rel id2]
  where
    matchingObjects     = findEntities (BasicEntity quantifier qObj) wrld objcts
    matchingLocations   = findLocations loc wrld objcts
findEntities Floor _ _  = ["Floor"]


-- | Generates a list of pairs (Relation, Id) given a relation 
-- and an entity description. It finds all the entities which 
-- match the criteria and just pair them with the given relation.
findLocations :: Location -> World -> Objects -> [(Relation, Id)] 
findLocations (Relative rel entity) wrld objcts = zip (repeat rel) entities
  where
   entities = findEntities entity wrld objcts 

interpret :: World -> Maybe Id -> Objects -> Command -> [Goal]
interpret world holding objects tree = 
  case tree of
    Take entity ->
      case holding of
        Nothing -> map TakeObj $ findEntities entity world objects
        Just _  -> []
    Put (Relative relation entity) ->
      case holding of
        Nothing -> []
        Just id -> map (MoveObj id relation) $ 
                   findEntities entity world objects
    Move entity loc ->
      case getQuantifier entity of
        All -> smartMatching matchingObjects (map snd matchingLocations)

        _   -> [MoveObj id1 rel id2 | id1 <- matchingObjects
               , (rel, id2) <- matchingLocations]
      where
        matchingObjects = findEntities entity world objects 
        matchingLocations = findLocations loc world objects
        relation = fst $ head matchingLocations

        smartMatching :: [Id] -> [Id] -> [Goal]
        smartMatching ids1 ids2 =
          let goal = maximumBy (\l1 l2 -> compare (length l1) (length l2)) .
                     filter (\_ -> True) $
                     [ zipWith (\id1 id2 -> MoveObj id1 relation id2) ids1'  ids2'
                       | ids1'  <- permutations ids1, ids2' <- permutations ids2]
          in if null goal then []
             else [Composed goal]
          where
            validGoal (MoveObj id1 rel id2) =
              validRelationship world objects id1 rel id2 


getQuantifier :: Entity -> Quantifier
getQuantifier (BasicEntity q _)      = q
getQuantifier (RelativeEntity q _ _) = q
