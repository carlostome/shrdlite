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
findEntities :: Entity -> World -> Objects -> Either [Id] [Id]
findEntities (BasicEntity quantifier qObj) wrld objcts =
  case quantifier of
      The | length matchingObjects > 1  -> Right matchingObjects
          | length matchingObjects == 0 -> Left []
          | otherwise -> Left matchingObjects
      _ -> Left matchingObjects
  where
    matchingObjects = findObjects qObj wrld objcts
findEntities (RelativeEntity quantifier qObj loc) wrld objcts =
  if not $ null ambiguity then
    Right ambiguity
  else
    let Left objectList   = matchingObjects
        Left locationList = matchingLocations
    in
      Left $ [ id1 
             | (rel,id2) <- locationList
             , id1 <- objectList
             , validRelationship wrld id1 rel id2]
  where
    matchingObjects   = findEntities (BasicEntity quantifier qObj) wrld objcts
    matchingLocations = findLocations loc wrld objcts
    leftAmbiguity     = getAmbiguity matchingObjects
    rightAmbiguity    = getAmbiguityLoc matchingLocations
    ambiguity         = leftAmbiguity ++ rightAmbiguity
findEntities Floor _ _  = Left ["Floor"]


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
findLocations :: Location -> World -> Objects -> Either ([(Relation, Id)]) [Id]
findLocations (Relative rel entity) wrld objcts = 
  case entities of
    Left entityList -> Left $ zip (repeat rel) entityList
    Right ambiguity -> Right ambiguity 
  where
   entities = findEntities entity wrld objcts

-- TODO do some kind of checking so we remove not possible at all goal?
interpret :: World -> Maybe Id -> Objects -> Command -> Either [Goal] [Id]
interpret world holding objects tree =
  case tree of
    Take entity 
      | entity == Floor -> Left []
      | otherwise -> 
          case holding of
            Just _ -> Left [] 
            Nothing -> case findEntities entity world objects of
                        Left list        -> Left $ map TakeObj list
                        Right ambiguity  -> Right ambiguity
    Put (Relative relation entity) ->
      case holding of
        Nothing -> Left []
        Just id -> 
          case  findEntities entity world objects of
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
                     , validMovement objects id1 id2 rel] 
                 | id1 <- objList]
                 ]
      where
        matchingObjects = findEntities entity world objects
        matchingLocations = findLocations loc world objects
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
