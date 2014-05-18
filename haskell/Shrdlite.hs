#!/usr/bin/env runhaskell

-- You need the 'json' package: cabal install json

-- Test from the command line:
-- runhaskell Shrdlite.hs < ../examples/medium.json

module Main where

import           CombinatorParser
import           Control.Monad    (foldM, liftM)
import           Data.List        (findIndex, intersperse, nub)
import qualified Data.Map         as M
import           Data.Maybe       (fromJust, isJust, isNothing)
import           ShrdliteGrammar
import           Text.JSON

import           DataTypes
import           Interpreter
import           Plan
import           Suggestions

main :: IO ()
main = getContents >>= putStrLn . encode . jsonMain . ok . decode


jsonMain :: JSObject JSValue -> JSValue
jsonMain jsinput = makeObj result
    where
      utterance = ok (valFromObj "utterance" jsinput)   :: Utterance
      world     = ok (fmap (map reverse) $ valFromObj "world" jsinput)  :: World
      holding   = ok (valFromObj "holding"   jsinput >>= parseId )      :: Maybe Id
      objects   = ok (valFromObj "objects"   jsinput >>= parseObjects ) :: Objects
      algorithm = ok (valFromObj "strategy"  jsinput >>= parseStrategy) :: Strategy

      trees     = parse command utterance :: [Command]

      (nonFilteredGoals, ambs) = let (g, a) = unzip $ map findG trees in (concat g, concat a)
      goals = filter validGoal $ map clearGoal nonFilteredGoals

      validGoal (Or [])    = False
      validGoal (And [])   = False
      validGoal _          = True
      clearGoal (And list) = And $ filter validGoal list
      clearGoal (Or list)  = Or $ filter validGoal list
      clearGoal other      = other

      findG tree = 
        case interpret currentWorld tree of
            Left list -> (list, [])
            Right amb -> ([], amb)
                         
      disambiguity = if null ambs 
                       then [] 
                       else map (unwords . getObjectDescription currentWorld) ambs

      currentWorld = WState holding (getPositions world) world objects

      (solution,stats) = plan algorithm currentWorld (head goals) :: (Maybe (Plan,WorldState), Int)
                  
      output    = if null trees then "Parse error!"
                  else 
                    if null goals then "Interpretation error!"
                    else 
                      if (length goals >= 2 || (not $ null disambiguity))
                      then "Ambiguous sentence, please provide more information."
                      else
                        if isNothing solution then "Planning error!"
                        else "Success!"

      Just (finalPlan, finalWorld) =  solution

      result    = [("utterance", showJSON utterance),
                   ("trees",     showJSON (map show trees)),
                   ("goals",     if length trees >= 1 then showJSON (map show goals)
                                 else JSNull),
                   ("plan",      if (length goals == 1 && null disambiguity) && isJust solution then
                                   showJSON (duplicate finalPlan)
				 else JSNull),
                   ("output",    showJSON output),
                   ("suggestions", if length goals == 1 && isJust solution 
                                    then 
                                      showJSON $ suggest finalWorld
                                    else
                                      showJSON $ suggest currentWorld),
                   ("disambiguity", if null (filter (not . null) disambiguity) 
                                        then JSNull  
                                        else showJSON disambiguity),
                  ("states", if length goals == 1 then 
                                showJSON $ stats
                             else
                                JSNull) ]

duplicate :: [String] -> [String]
duplicate [] = []
duplicate (x:xs) = ("I do: " ++ x) : x : duplicate xs

-- | Parse a JSValue to a Maybe Id
parseId :: JSValue -> Result (Maybe Id)
parseId JSNull = return Nothing
parseId (JSString str) = return . return . fromJSString $ str

parseStrategy :: JSValue -> Result Strategy
parseStrategy (JSString str) = 
  case fromJSString str of
       "0" -> return AStar
       "1" -> return BFS
       other   -> error other

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

ok :: Result a -> a
ok (Ok res) = res
ok (Error err) = error err
