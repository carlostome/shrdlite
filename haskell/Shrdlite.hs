#!/usr/bin/env runhaskell

-- You need the 'json' package: cabal install json

-- Test from the command line:
-- runhaskell Shrdlite.hs < ../examples/medium.json

module Main where 

import ShrdliteGrammar
import CombinatorParser
import Text.JSON
import Data.List (findIndex)
import qualified Data.Map  as M
import Data.Maybe (fromJust, isNothing, isJust)
import Control.Monad (foldM, liftM)
import Plan
  
import DataTypes

main :: IO ()
main = getContents >>= putStrLn . encode . jsonMain . ok . decode


jsonMain :: JSObject JSValue -> JSValue
jsonMain jsinput = makeObj result
    where 
      utterance = ok (valFromObj "utterance" jsinput)   :: Utterance
      world     = ok (valFromObj "world"     jsinput)   :: World
      holding   = ok (valFromObj "holding"   jsinput >>= parseId )      :: Maybe Id
      objects   = ok (valFromObj "objects"   jsinput >>= parseObjects ) :: Objects

      trees     = parse command utterance :: [Command]

      goals     = [goal | tree <- trees, goal <- interpret world holding objects tree] :: [Goal]

      plan      = Nothing {-solve world holding objects (head goals)-} :: Maybe Plan

      force     = error (show goals ++ show trees)
      output    = if {-null trees-} force then "Parse error!"
                  else if null goals then "Interpretation error!"
                       else if length goals >= 2 then "Ambiguity error!"
                            else if isNothing plan then "Planning error!"
                                 else "Success!"

      result    = [("utterance", showJSON utterance),
                   ("trees",     showJSON (map show trees)),
                   ("goals",     if length trees >= 1 then showJSON (show goals) else JSNull),
                   ("plan",      if isJust plan && length goals == 1 then showJSON (fromJust plan)
				 else JSNull),
                   ("world",     showJSON (show objects)),
                   ("output",    showJSON output)
                  ]

-- | Parse a JSValue to a Maybe Id
parseId :: JSValue -> Result (Maybe Id)
parseId JSNull = return Nothing
parseId (JSString str) = return . return . fromJSString $ str
          
-- | Parse JSON Object to real Object representation.
parseObjects :: JSObject JSValue -> Result Objects
parseObjects = foldM (\m (id,JSObject o) -> readObj (fromJSObject o)
                                            >>= \obj -> return $ M.insert id obj m) M.empty
               . fromJSObject 
  where
    readObj :: [(String,JSValue)]-> Result Object
    readObj object = do
       form  <- look "form"   object >>= toForm   . fromJSString
       color <- look "color"  object >>= toColor  . fromJSString
       size  <- look "size"   object >>= toSize   . fromJSString
       return $ Object  size color form

    toForm form = case form of
                    "anyform" -> return AnyForm
                    "brick"   -> return Brick
                    "plank"   -> return Plank
                    "ball"    -> return Ball
                    "pyramid" -> return Pyramid
                    "box"     -> return Box
                    "table"   -> return Table
                    str       -> fail $ "Not a form: " ++ str
    toColor col = case col of
                    "anycolor" -> return AnyColor
                    "black"    -> return Black
                    "white"    -> return White
                    "blue"     -> return Blue
                    "green"    -> return Green
                    "yellow"   -> return Yellow
                    "red"      -> return Red
                    str        -> fail $ "Not a color: " ++ str
    toSize size = case size of
                    "anysize" -> return AnySize
                    "small"   -> return Small
                    "large"   -> return Large
                    str       -> fail $ "Not a size: " ++ str
                                     
    look str list = maybe (fail "Not in the list")
                    (\(JSString s) -> return s) $ lookup str list 



(~==) :: Object -> Object -> Bool
(Object sz1 c1 f1) ~== (Object sz2 c2 f2) = (cmpSz sz1 sz2) && (cmpCol c1 c2) && (cmpForm f1 f2)
  where
    cmpSz s1 s2   = s1 == AnySize || s2 == AnySize || s1 == s2
    cmpCol c1 c2  = c1 == AnyColor || c2 == AnyColor || c1 == c2
    cmpForm f1 f2 = f1 == AnyForm || f2 == AnyForm || f1 == f2
     
-- | Finds all the objects matching a given description.
findObjects :: Object -> World -> Objects -> [Id]
findObjects _ []       objInfo  = []
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
findEntities (RelativeEntity _ qObj loc) wrld objcts = map fst3 correctOutputs 
  where
    matchingObjects   = findObjects qObj wrld objcts
    matchingLocations = findLocations loc wrld objcts
    joinTuples        = zipWith (\id1 (rel, id2) -> (id1, rel, id2)) 
                          matchingObjects matchingLocations
    correctOutputs    = filter filterFunction joinTuples
    filterFunction (id1, rel, id2) =  filterByLocation wrld objcts id1 rel id2
    fst3 (a,b,c) = a
findEntities Floor _ _                               = ["Floor"]

-- | Makes sure that the given object fulfills the relation with the 
-- second one.
filterByLocation :: World -> Objects -> Id -> Relation -> Id -> Bool
filterByLocation w objs id1 rel id2 = 
  case rel of
    Ontop   -> checkOnTop
    Inside  -> checkOnTop
    Above   -> checkAbove
    Under   -> checkUnder
    Rightof -> checkRight
    Leftof  -> checkLeft 
    Beside  -> checkBeside
  where
    getObject "Floor" = error "Can't retrieve the object Floor"
    getObject id = fromJust $ M.lookup id objs
    getStack "Floor" = error "Can't retrieve the stack number of a Floor"
    getStack id = fst $ fromJust $ M.lookup id positions
    getPositionInStack "Floor" = error "Can't retrieve the position in the stack of the Floor"
    getPositionInStack id = snd $ fromJust $ M.lookup id positions
    checkOnTop = (id2 == "Floor" && getPositionInStack id1 == 1)
                 || 
                 (getStack id1 == getStack id2 
                 && getPositionInStack id1 == getPositionInStack id2 + 1)
    checkAbove = (id2 == "Floor" && getPositionInStack id1 >= 1)
                 ||
                 getStack id1 == getStack id2
                 && getPositionInStack id1 > getPositionInStack id2 
    checkUnder = getStack id1 == getStack id2
                 && getPositionInStack id1 < getPositionInStack id2
    checkLeft  = getStack id1 < getStack id2
    checkRight = getStack id1 > getStack id2
    checkBeside = abs (getStack id1 - getStack id2) == 1
    positions  = DataTypes.getPositions w 


-- | Generates a list of pairs (Relation, Id) given a relation 
-- and an entity description. It finds all the entities which 
-- match the criteria and just pair them with the given relation.
findLocations :: Location -> World -> Objects -> [(Relation, Id)] 
findLocations (Relative rel entity) wrld objcts = zip (repeat rel) entities
  where
   entities = findEntities entity wrld objcts 

{- 
data Command  = Take Entity | Put Location | Move Entity Location

data Location = Relative Relation Entity

data Entity   = Floor 
              | BasicEntity Quantifier Object 
              | RelativeEntity Quantifier Object Location
-}

-- TODO
physicalLawHolds _ _ _= True

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
                                         , (rel, id2) <- matchingLocations
                                         , physicalLawHolds id1 rel id2]
      where
        matchingObjects = findEntities entity world objects 
        matchingLocations = findLocations loc world objects

solve :: World -> Maybe Id -> Objects -> Goal -> Maybe Plan
solve  = plan 

ok :: Result a -> a
ok (Ok res) = res
ok (Error err) = error err

