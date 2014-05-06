module Interpreter where

import           DataTypes
import           ShrdliteGrammar

import qualified Data.Map        as M
import           Data.Maybe      (fromJust)

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
  case quantifier of
      The | length matchingObjects /=1 -> []
          | otherwise -> matchingObjects
      _ -> matchingObjects
  where
    matchingObjects = findObjects qObj wrld objcts
findEntities (RelativeEntity quantifier qObj loc) wrld objcts =
  [ id1  | (rel,id2) <- matchingLocations, id1 <- matchingObjects
  , validRelationship wrld id1 rel id2]
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

-- TODO do some kind of checking so we remove not possible at all goal?
interpret :: World -> Maybe Id -> Objects -> Command -> [Goal]
interpret world holding objects tree =
  case tree of
    Take entity 
      | entity == Floor -> []
      | otherwise -> 
        case holding of
          Nothing -> map TakeObj $ findEntities entity world objects
          Just _  -> []
    Put (Relative relation entity) ->
      case holding of
        Nothing -> []
        Just id -> map (MoveObj id relation) $
                   findEntities entity world objects
    Move entity loc ->
      [fstOperator $ 
        [sndOperator $ [MoveObj id1 rel id2 | (rel, id2) <- matchingLocations, validMovement objects id1 id2 rel] 
                                            | id1 <- matchingObjects]
      ]
      where
        matchingObjects = findEntities entity world objects
        matchingLocations = findLocations loc world objects
        fstOperator = selectOperator $ getQuantifier entity 
        sndOperator = selectOperator $ getQuantifierLoc loc
        selectOperator All = And
        selectOperator _   = Or


getQuantifier :: Entity -> Quantifier
getQuantifier (BasicEntity q _)      = q
getQuantifier (RelativeEntity q _ _) = q

getQuantifierLoc :: Location -> Quantifier
getQuantifierLoc (Relative _ entity) = getQuantifier entity
