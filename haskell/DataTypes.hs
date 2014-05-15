module DataTypes where

import           Data.Hashable
import qualified Data.Map        as M
import           ShrdliteGrammar

type Id = String

data Goal = And [Goal]
          | Or [Goal]
          | MoveObj Id Relation Id
          | TakeObj Id deriving (Eq, Show)

type Utterance = [String]
type World = [[Id]]
type Objects = M.Map Id Object
type Plan = [String]

data Strategy = AStar | BFS | LowerCost | PartialOrderPlanner

data WorldState = WState { _holding     :: Maybe Id,
                           _positions   :: M.Map Id (Int, Int),
                           _world       :: World,
                           _objectsInfo :: M.Map Id Object } deriving Show

instance Hashable WorldState where
  hashWithSalt s (WState holding positions world _) = 
    s `hashWithSalt` holding `hashWithSalt` world 

-- | Get the coordinates of all objects in the world
getPositions :: World -> M.Map Id (Int,Int)
getPositions = snd .
    foldl (\(cx,m) stack ->
          (cx+1,snd $ foldl (\(cy,m') elem ->
                      (cy-1,M.insert elem (cx,cy) m')) (length stack,m) stack))
    (0,M.empty)

-- | Equality of two objects.
(~==) :: Object -> Object -> Bool
(Object s1 c1 f1) ~== (Object s2 c2 f2) =
  and [ s1 == AnySize  || s2 == AnySize  || s1 == s2
      , c1 == AnyColor || c2 == AnyColor || c1 == c2
      , f1 == AnyForm  || f2 == AnyForm  || f1 == f2]


-- | Makes sure that the given object fulfills the relation with the
-- second one.
relationHolds :: WorldState -> Id -> Relation -> Id -> Bool
relationHolds worldState id1 rel id2 =
  case rel of
    Beside  -> abs (x1 - x2) == 1
    Leftof  -> x1 < x2
    Rightof -> x1 > x2
    Above   -> x1 == x2 && y1 > y2
    Ontop   -> x1 == x2 && y1 - y2 == 1
    Inside  -> x1 == x2 && y1 - y2 == 1
    Under   -> x1 == x2 && y1 < y2
    where (x1,y1) = maybe (error $ "relationHolds: " ++ show id1 ++ "\n" ++ show worldState) 
                          id
                          (M.lookup id1 (_positions worldState))
          Just (x2,y2) = case id2 of
                           "Floor" -> return (x1,0)
                           _       -> M.lookup id2 (_positions worldState)


-- | Checks if it is possible to put id1 in relation to id2.
--  The only case where can be a problem is when it attempts to
--  put an object over another.
relationValid :: Objects -> Id -> Id -> Relation -> Bool
relationValid info id1 id2 Ontop
  | id2 == "Floor"         = True
  | size1 > size2          = False
  | form1 == Ball          = form2 == Box 
  | form2 == Ball          = False
  | form2 == Box           = not (form1 == Pyramid || form1 == Plank || form1 == Box)
                             || size2 > size1
  | form1 == Box           = and [ size1 == size2
                                 , or [ form2 == Table
                                      , form2 == Plank
                                      , size1 == Large && form2 == Brick]]
  | otherwise = True
  where 
    (Object size1 _ form1) =
      maybe (error $ "getObject" ++ show id1) id (M.lookup id1 info)
    (Object size2 _ form2) =
      maybe (error $ "getObject" ++ show id2) id (M.lookup id2 info)

relationValid info id1 id2 Inside
  | id1 == "Floor" || id2 == "Floor"  || form2 /= Box = False
  | otherwise = relationValid info id1 id2 Ontop
  where
    (Object size2 _ form2) =
      maybe (error $ "getObject" ++ show id2) id (M.lookup id2 info)
    
relationValid info id1 id2 Above 
  | id2 /= "Floor" && form2 == Ball = False
  | otherwise     = True
  where
    (Object size2 _ form2) =
      maybe (error $ "getObject" ++ show id2) id (M.lookup id2 info)

relationValid info id1 id2 Under 
  | id1 == "Floor" = True
  | id2 == "Floor" = False
  | form1 == Ball  = False
  | otherwise     = True
  where
    (Object size1 _ form1) =
      maybe (error $ "getObject" ++ show id1) id (M.lookup id1 info)

relationValid _ _ "Floor" Leftof  = False
relationValid _ _ "Floor" Rightof = False
relationValid _ _ "Floor" Beside  = False
relationValid _ "Floor" _ _       = False
relationValid _ _ _ _             = True
