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
import Control.Monad (foldM, liftM)
import Data.Maybe
  
type Utterance = [String]
type Id = String
type World = [[Id]]
type Objects = M.Map Id Object
data Goal = MoveObj Id Id | TakeObj Id | PutObj Id 
type Plan = [String]


main :: IO ()
main = getContents >>= putStrLn . encode . jsonMain . ok . decode


jsonMain :: JSObject JSValue -> JSValue
jsonMain jsinput = makeObj result
    where 
      utterance = ok (valFromObj "utterance" jsinput)   :: Utterance
      world     = ok (valFromObj "world"     jsinput)   :: World
      holding   = ok (valFromObj "holding"   jsinput)   :: Id
      objects   = ok (valFromObj "objects"   jsinput >>= parseObjects ) :: Objects

      trees     = parse command utterance :: [Command]

      goals     = [goal | tree <- trees, goal <- interpret world holding objects tree] :: [Goal]

      plan      = solve world holding objects (head goals) :: Plan

      output    = if null trees then "Parse error!"
                  else if null goals then "Interpretation error!"
                       else if length goals >= 2 then "Ambiguity error!"
                            else if null plan then "Planning error!"
                                 else "Success!"

      result    = [("utterance", showJSON utterance),
                   ("trees",     showJSON (map show trees)),
                   ("goals",     if length trees >= 1 then showJSON goals else JSNull),
                   ("plan",      if length goals == 1 then showJSON plan  else JSNull),
                   ("world",     showJSON (show objects)),
                   ("output",    showJSON output)
                  ]

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
      where
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
(~==) = undefined
    
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

{- 
data Command  = Take Entity | Put Location | Move Entity Location

data Location = Relative Relation Entity

data Entity   = Floor 
              | BasicEntity Quantifier Object 
              | RelativeEntity Quantifier Object Location
-}

interpret :: World -> Id -> Objects -> Command -> [Goal]
interpret world holding objects tree = 
  case tree of
    Take entity          -> map (TakeObj) $ findEntities entity
    Put location         -> map (PutObj) $ findLocations location
    Move entity location -> 
      (findEntities entity) ** (findLocations location)
      
  where
    findEntities = undefined
    findLocations = undefined
    [] ** _          = []
    _ ** []          = []
    (x:xs) ** ys     = map (createGoal x) ys ++ (xs ** ys)
      where
        createGoal x y = MoveObj x y


solve :: World -> Id -> Objects -> Goal -> Plan
solve world holding objects goal = ["I picked it up . . .", "pick " ++ show col, ". . . and I dropped it down", "drop " ++ show col]
    where
      Just col = findIndex (not . null) world


ok :: Result a -> a
ok (Ok res) = res
ok (Error err) = error err

