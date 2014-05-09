module Suggestions where

import           Data.Char       (toLower)
import           Data.List       (intercalate, sortBy, minimumBy)
import qualified Data.Map        as M
import           DataTypes
import           ShrdliteGrammar
import Plan
import Interpreter

suggest :: WorldState -> [String]
suggest worldState =
   map (intercalate " " . goalToUtterance worldState . snd) $
       take 5 $
              [(heuristicAStar worldState goal,goal)
                 | goal <- generateAllSugestions worldState]
       
generateAllSugestions :: WorldState -> [Goal]
generateAllSugestions worldState =  
  case _holding worldState of
   Just id -> [ MoveObj  id rel id2 | rel <- relations
                                    , id2 <- "Floor" : (map fst $ M.toList (_objectsInfo worldState))
                                    , relationValid (_objectsInfo worldState) id id2 rel
                                    , id /= id2]

   Nothing -> [ MoveObj  id1 rel id2 | rel <- relations
                                     , id1 <- map fst $ M.toList (_objectsInfo worldState)
                                     , id2 <- "Floor" : (map fst $ M.toList (_objectsInfo worldState))
                                     , relationValid (_objectsInfo worldState) id1 id2 rel
                                     , id1 /= id2]
              ++
              [ TakeObj  id1 | id1 <- map fst $ M.toList (_objectsInfo worldState)]

relations :: [Relation]
relations = [Beside , Leftof , Rightof , Above , Ontop , Under , Inside]
  
-- | Translate a goal to an utterance for displaying in the suggestions list
goalToUtterance ::  WorldState -> Goal -> Utterance
goalToUtterance worldState goal =
  case goal of
    TakeObj id1 -> ["take", "the"] ++ (getObjectDescription worldState id1)
    MoveObj id1 rel id2 ->
      ["put", "the"]
      ++ getObjectDescription worldState id1
      ++ getRelationDescription rel 
      ++ getObjectDescription worldState id2

getObjectDescription :: WorldState -> Id -> [String]
getObjectDescription worldState id
   | id == "Floor" = ["the", "floor"]
   | otherwise =
     let Just obj = M.lookup id objects
         (Object size color form) = fewestAttributesToIdentifyObject objects obj world

     in  map (map toLower) [show size,show color, show form]

-- | For a given object returns an object with the fewest number of attributes
-- for identifying it uniquely in the world
-- The entities are created with the "The" quantifier, to make sure only unique
-- matches are returned through the "Left" constructor. This criterion is used
-- to filter the list.
fewestAttributesToIdentifyObject :: WorldState -> Object -> Object
fewestAttributesToIdentifyObject worldState (Object size color form) = 
    let allCombEnts = map (BasicEntity The) [Object s c f | s <- [AnySize, size]
                                                          , c <- [AnyColor, color]
                                                          , f <- [AnyForm, form]]
        uniqueEntities = filter (isLeft . (\ent -> findEntities ent worldState)) allCombEnts
        uniqueObjects = [obj | (BasicEntity The obj) <- uniqueEntities]
    in minimumBy orderObjsByDescrLength uniqueObjects 

-- | Order of Objects with respect to their description length
orderObjsByDescrLength :: Object -> Object -> Ordering
orderObjsByDescrLength obj1 obj2
  | descriptionLength obj1 < descriptionLength obj2 = LT
  | descriptionLength obj1 > descriptionLength obj2 = GT
  | otherwise = EQ
   
-- | Counts how many attributes of the object are actually set,
-- i.e. are not Any*
descriptionLength :: Object -> Int
descriptionLength (Object size color form) = sc + cc + fc
  where
    sc = case size of
      AnySize -> 0
      _ -> 1
    cc = case color of
      AnyColor -> 0
      _ -> 1
    fc = case form of
      AnyForm -> 0
      _ -> 1
 
-- | Checks if an Either object was constructed with Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft x        = False

getRelationDescription :: Relation -> [String]
getRelationDescription relation = return $ 
  case relation of
    Ontop   -> "on the" 
    Inside  -> "in the"
    Above   -> "above the"
    Under   -> "under the"
    Rightof -> "right of the"
    Leftof  -> "left of the"
    Beside  -> "beside the"



