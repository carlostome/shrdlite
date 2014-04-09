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
        All -> smartMatching matchingObjects (map snd matchingLocationsAll)

        _   -> [MoveObj id1 rel id2 | id1 <- matchingObjects
               , (rel, id2) <- matchingLocations]
      where
        matchingObjects = findEntities entity world objects 
        matchingLocations = findLocations loc world objects
        matchingLocationsAll = findLocations (allToAny loc) world objects
        relation = fst $ head matchingLocations
        allToAny (Relative r (BasicEntity All o)) = Relative r (BasicEntity Any o)
        allToAny (Relative r (RelativeEntity All o loc)) = Relative r (RelativeEntity Any o loc)
        allToAny loc = loc

        smartMatching :: [Id] -> [Id] -> [Goal]
        smartMatching ids1 ids2 =
          if null goal then [] else [Composed goal]
          where
            goal = maximumBy bestOption . 
                   filter (any (not . validGoal)) $
                   [ zipWith (\id1 id2 -> MoveObj id1 relation id2) ids1  ids2
                   | ids1  <- source, ids2 <- target]
            source = permutations ids1
            target =
              let len1 = length ids1
              in if ids2 == ["Floor"] then
                   replicate len1 (replicate len1 "Floor")
                 else
                   permutations ids2
            validGoal (MoveObj id1 rel id2) =
              validRelationship world objects id1 rel id2 
            bestOption l1 l2 = compare (fitRate l1) (fitRate l2)
            fitRate = foldl (\acc (MoveObj id1 _ id2) ->
                            if id2 == "Floor" then
                              acc + 100
                            else
                              let (Object s1 _ _) = getObject id1 objects
                                  (Object s2 _ _) = getObject id2 objects
                              in (if s1 == Small && s1 == s2 then 1 else 0) + acc + 100) 0

getQuantifier :: Entity -> Quantifier
getQuantifier (BasicEntity q _)      = q
getQuantifier (RelativeEntity q _ _) = q
