module Suggestions where

import           Data.Char       (toLower)
import           Data.List       (intercalate, sortBy)
import qualified Data.Map        as M
import           DataTypes
import           ShrdliteGrammar
import Plan

suggest :: World -> Maybe Id -> Objects -> [String]
suggest world holding objects = ["Hey"]
{-
   map (intercalate " " . goalToUtterance objects . snd) $
       take 5 $
       sortBy (\(h1,_) (h2,_) -> compare h2 h1)
            [(heuristicAStar worldState goal,goal)
               | goal <- generateAllSugestions world holding objects]
     where
       worldState = WState holding (getPositions world) world objects
       
generateAllSugestions :: World -> Maybe Id -> Objects -> [Goal]
generateAllSugestions world holding objects =  
  case holding of
   Just id -> [ MoveObj  id rel id2 | rel <- relations
                                    , id2 <- "Floor" : (map fst $ M.toList objects)
                                    , validMovement objects id id2 rel
                                    , id /= id2]

   Nothing -> [ MoveObj  id1 rel id2 | rel <- relations
                                     , id1 <- map fst $ M.toList objects
                                     , id2 <- "Floor" : (map fst $ M.toList objects)
                                     , validMovement objects id1 id2 rel
                                     , id1 /= id2]
              ++
              [ TakeObj  id1 | id1 <- map fst $ M.toList objects]

relations :: [Relation]
relations = [Beside , Leftof , Rightof , Above , Ontop , Under , Inside]
  
goalToUtterance :: Objects -> Goal -> Utterance
goalToUtterance objs goal =
  case goal of
    TakeObj id1 -> ["take", "the"] ++ (getObjectDescription objs id1)
    MoveObj id1 rel id2 ->
      ["put", "the"]
      ++ getObjectDescription objs id1
      ++ getRelationDescription rel 
      ++ getObjectDescription objs id2

getObjectDescription :: Objects -> Id -> [String]
getObjectDescription objects id
   | id == "Floor" = ["the", "floor"]
   | otherwise =
     let Just (Object size color form) = M.lookup id objects
     in  map (map toLower) [show size,show color, show form]

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




-}
