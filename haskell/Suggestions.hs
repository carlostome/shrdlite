module Suggestions where

import           Data.Char       (toLower)
import           Data.List       (intercalate, sortBy, minimumBy)
import qualified Data.Map        as M
import           Data.Maybe      (isJust, fromJust)
import           DataTypes
import           ShrdliteGrammar
import Plan
import Interpreter

suggest :: WorldState -> [String]
suggest worldState =
   map (intercalate " " . goalToUtterance worldState . snd) $
       take 5 $ sortBy (\(h1,_) (h2,_) -> compare h2 h1)
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
      (if isJust $ _holding worldState then ["put", "it"]
      else
        ["put", "the"] ++ getObjectDescription worldState id1)
      ++ getRelationDescription rel 
      ++ getObjectDescription worldState id2

getObjectDescription :: WorldState -> Id -> [String]
getObjectDescription worldState id
  | id == "Floor" = ["the", "floor"]
  | otherwise =
   let Just obj = M.lookup id (_objectsInfo worldState)
       ent = fewestAttributesToIdentifyObject worldState obj id
   in
     case ent of
       BasicEntity _ obj -> objAttrsToString obj
       RelativeEntity q1 thisObj (Relative rel (BasicEntity q2 obj2)) ->
         objAttrsToString thisObj ++ ((map toLower $ show rel) : "the" : objAttrsToString obj2)

-- | Returns a list of strings of the attributes of an object
objAttrsToString :: Object -> [String]
objAttrsToString (Object size color form) =
  let
    sizeList =
      if size == AnySize then []
      else                    [show size]
    colorList =
      if color == AnyColor then []
      else                      [show color]
    formList = 
      if form == AnyForm then ["object"]
      else                    [show form]
  in map (map toLower) $ concat [sizeList, colorList, formList]

-- | For a given object returns an object with the fewest number of attributes
-- for identifying it uniquely in the world
-- The entities are created with the "The" quantifier, to make sure only unique
-- matches are returned through the "Left" constructor. This criterion is used
-- to filter the list.
fewestAttributesToIdentifyObject :: WorldState -> Object -> Id -> Entity
fewestAttributesToIdentifyObject worldState obj@(Object size color form) id = 
    let allCombEnts = [BasicEntity The (Object s c form) | s <- [AnySize, size]
                                                         , c <- [AnyColor, color]]
        uniqueEntities = filter (isLeft . (\ent -> findEntities ent worldState)) allCombEnts
    in
      if null uniqueEntities then
        let allObjs = map snd $ M.toList $ _objectsInfo worldState
            allIds = concat $ _world worldState
            allRels = [Beside, Leftof, Rightof, Above, Ontop, Under, Inside]
            validRelPairs = filter (\(lokId, rel) -> relationHolds worldState id rel lokId)
                          [(localId, rel) | localId <- allIds, rel <- allRels]

            allRelEnts =
              [(RelativeEntity Any (Object s1 c1 form)
                (Relative rel (BasicEntity The (Object s c frm)))) |
                (lokId, rel) <- validRelPairs
                , (Object sz clr frm) <- [fromJust $ M.lookup lokId (_objectsInfo worldState)]
                , s <- [AnySize, sz]
                , c <- [AnyColor, clr]
                , s1 <- [AnySize, size]
                , c1 <- [AnyColor, color]
                ]
            validRels = filter (isLeft . \ent -> findEntities ent worldState) allRelEnts
        in
            minimumBy orderRelEntByBothObjects validRels
      else minimumBy orderEntsByDescrLength uniqueEntities

orderRelEntByBothObjects :: Entity -> Entity -> Ordering
orderRelEntByBothObjects (RelativeEntity _ obj11 (Relative _ (BasicEntity _ obj12)))
                         (RelativeEntity _ obj21 (Relative _ (BasicEntity _ obj22)))
  | descriptionLength obj11 + descriptionLength obj12 <
    descriptionLength obj21 + descriptionLength obj22 = LT
  | descriptionLength obj11 + descriptionLength obj12 >
    descriptionLength obj21 + descriptionLength obj22 = GT
  | otherwise                                         = EQ 

-- | Order of Entities with respect to their description length
orderEntsByDescrLength :: Entity -> Entity -> Ordering
orderEntsByDescrLength (BasicEntity _ obj1) (BasicEntity _ obj2) =
  orderObjByDescr obj1 obj2
orderEntsByDescrLength (RelativeEntity _ _ (Relative _ bEnt1))
                       (RelativeEntity _ _ (Relative _ bEnt2)) =
  orderEntsByDescrLength bEnt1 bEnt2

-- | Order of Objects with respect to their description length
orderObjByDescr :: Object -> Object -> Ordering
orderObjByDescr obj1 obj2
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



