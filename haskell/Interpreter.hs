module Interpreter where 

import DataTypes
import ShrdliteGrammar

import Data.Maybe (fromJust)
import qualified Data.Map as M

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
findEntities (BasicEntity _ qObj) wrld objcts        = findObjects qObj wrld objcts
findEntities (RelativeEntity _ qObj loc) wrld objcts = 
  [ id1  | (rel,id2) <- matchingLocations, id1 <- matchingObjects
  , validRelationship wrld objcts id1 rel id2]
  where
    matchingObjects   = findObjects qObj wrld objcts
    matchingLocations = findLocations loc wrld objcts
findEntities Floor _ _                               = ["Floor"]



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
    Take entity                    ->
      case holding of
        Nothing -> map TakeObj $ findEntities entity world objects
        Just _  -> []
    Put (Relative relation entity) ->
      case holding of
        Nothing -> []
        Just id -> map (MoveObj id relation) $ findEntities entity world objects
    Move entity loc -> [MoveObj id1 rel id2 | id1 <- matchingObjects
                                         , (rel, id2) <- matchingLocations]
      where
        matchingObjects = findEntities entity world objects 
        matchingLocations = findLocations loc world objects
