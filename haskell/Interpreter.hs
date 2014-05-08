module Interpreter where

import           DataTypes
import           ShrdliteGrammar

import qualified Data.Map        as M

-- | Finds all the objects matching a given description.
findObjects :: Object -> World -> Objects -> [Id]
findObjects _ [] _              = []
findObjects objQ (x:xs) objInfo = searchInStack objQ x ++ findObjects objQ xs objInfo
  where
    searchInStack _              [] = []
    searchInStack queryObj (objId:xs) =
      if queryObj ~== maybe (error "searchInStack") id (M.lookup objId objInfo)
        then objId : searchInStack queryObj xs
        else searchInStack queryObj xs

-- | Finds all the ids of the objects matching the given criteria.
-- Returns (Right list) if the quantifier was a "The" and more 
-- than one item was found.
findEntities :: Entity -> WorldState -> Either [Id] [Id]
findEntities (BasicEntity quantifier qObj) worldState  =
  case quantifier of
      The | length matchingObjects > 1  -> Right matchingObjects
          | length matchingObjects == 0 -> Left []
          | otherwise -> Left matchingObjects
      _ -> Left matchingObjects
  where
    matchingObjects = findObjects qObj (_world worldState)
                                       (_objectsInfo worldState)
findEntities (RelativeEntity quantifier qObj loc) worldState =
  if not $ null ambiguity then
    Right ambiguity
  else
    let Left objectList   = matchingObjects
        Left locationList = matchingLocations
    in
      Left $ [ id1 
             | (rel,id2) <- locationList
             , id1 <- objectList
             , relationHolds worldState id1 rel id2]
  where
    matchingObjects   = findEntities (BasicEntity quantifier qObj) worldState
    matchingLocations = findLocations loc worldState
    leftAmbiguity     = getAmbiguity matchingObjects
    rightAmbiguity    = getAmbiguityLoc matchingLocations
    ambiguity         = leftAmbiguity ++ rightAmbiguity
findEntities Floor _   = Left ["Floor"]


-- TODO clean this up
getAmbiguity :: Either [Id] [Id] -> [Id]
getAmbiguity result =
  case result of
    Right amb -> amb
    _        -> []

getAmbiguityLoc :: Either [(Relation, Id)] [Id] -> [Id]
getAmbiguityLoc result =
  case result of
    Right amb -> amb
    _        -> []

-- | Generates a list of pairs (Relation, Id) given a relation
-- and an entity description. It finds all the entities which
-- match the criteria and just pair them with the given relation.
findLocations :: Location -> WorldState -> Either ([(Relation, Id)]) [Id]
findLocations (Relative rel entity) worldState = 
  case entities of
    Left entityList -> Left $ zip (repeat rel) entityList
    Right ambiguity -> Right ambiguity 
  where
   entities = findEntities entity worldState

-- TODO do some kind of checking so we remove not possible at all goal?
interpret :: WorldState -> Command -> Either [Goal] [Id]
interpret worldState tree =
  case tree of
    Take entity 
      | entity == Floor -> Left []
      | otherwise -> 
          case _holding worldState of
            Just _ -> Left [] 
            Nothing -> case findEntities entity worldState of
                        Left list        -> Left $ map TakeObj list
                        Right ambiguity  -> Right ambiguity
    Put (Relative relation entity) ->
      case _holding worldState of
        Nothing -> Left []
        Just id -> 
          case  findEntities entity worldState of
            Left list         -> Left $ map (MoveObj id relation) list
            Right ambiguity   -> Right ambiguity
    Move entity loc ->
      if not $ null ambiguity then
        Right ambiguity
      else 
        let Left locList = matchingLocations
            Left objList = matchingObjects
        in
          Left $ [fstOperator $ 
                     [sndOperator $ [MoveObj id1 rel id2 
                     | (rel, id2) <- locList
                     , relationValid (_objectsInfo worldState) id1 id2 rel] 
                 | id1 <- objList]
                 ]
      where
        matchingObjects = findEntities entity worldState
        matchingLocations = findLocations loc worldState
        fstOperator = selectOperator $ getQuantifier entity 
        sndOperator = selectOperator $ getQuantifierLoc loc
        selectOperator All = And
        selectOperator _   = Or
        leftAmbiguity  = getAmbiguity matchingObjects
        rightAmbiguity = getAmbiguityLoc matchingLocations
        ambiguity = leftAmbiguity ++ rightAmbiguity


getQuantifier :: Entity -> Quantifier
getQuantifier (BasicEntity q _)      = q
getQuantifier (RelativeEntity q _ _) = q
getQuantifier Floor = Any 

getQuantifierLoc :: Location -> Quantifier
getQuantifierLoc (Relative _ entity) = getQuantifier entity
