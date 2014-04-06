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
findEntities (BasicEntity quantifier qObj) wrld objcts =
  case quantifier of
   Any -> if null objects then []
          else [head objects]
   -- Later, objects has to be checked with a length == 1
   The -> objects
   All -> objects
  where
    objects = findObjects qObj wrld objcts
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
    Take entity -> case holding of
                     Nothing -> map TakeObj $ findEntities entity world objects
                     Just _  -> []
    Put (Relative relation entity) -> case holding of
                                        Nothing -> []
                                        Just id -> map (MoveObj id relation) $ 
                                                     findEntities entity world objects
    Move entity loc -> case getQuantifier entity of
                         All -> case smartMatching 
                                       [] matchingObjects 
                                          (map snd matchingLocations) 
                                of
                                  [] -> []
                                  list -> [Composed list]
                         _   -> [MoveObj id1 rel id2 | id1 <- matchingObjects
                                                     , (rel, id2) <- matchingLocations]
      where
        matchingObjects = findEntities entity world objects 
        matchingLocations = findLocations loc world objects
        relation = fst $ head matchingLocations
        smartMatching :: [Goal] -> [Id] -> [Id] -> [Goal]
        smartMatching accum [] _          = accum
        smartMatching accum _ []          = accum
        smartMatching accum (o:os) locs   = let (bestLoc, rest) = findBestLoc o locs 
                                            in case bestLoc of
                                                Nothing  -> smartMatching accum os locs
                                                Just smt -> smartMatching (MoveObj o relation o:accum) os rest
        findBestLoc o locs                = 
          case filter (validRelationship world objects o relation) locs of
            [] -> (Nothing, locs)
            list -> findMostSuitableLoc objects o locs

-- | Finds the smaller location allowed to be under the given object. 
-- This is useful in order to distribute the objects in an optimal way.
findMostSuitableLoc :: Objects -> Id -> [Id] -> (Maybe Id, [Id])
findMostSuitableLoc objects id1 locs = findSmaller id1 locs []
  where
    findSmaller id1 []     accum = (Nothing, accum)
    findSmaller id1 [x]    accum = (Just x, accum)
    findSmaller id1 (x:xs) accum = 
      case x of 
        "Floor" -> (Just "Floor", accum)
        object -> let (Object size _ _) = getObject object objects
                  in if (size == Small) then 
                       (Just object, accum)
                     else 
                       findSmaller id1 xs (x:accum)

getQuantifier :: Entity -> Quantifier
getQuantifier (BasicEntity q _)      = q
getQuantifier (RelativeEntity q _ _) = q
