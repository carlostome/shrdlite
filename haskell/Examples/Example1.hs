module Example where

import Shrdlite
import Plan
import Interpreter
import ShrdliteGrammar
import CombinatorParser
import Suggestions
import Data.Map as M
import DataTypes
  
worldE  :: World
worldE   = [["b","d"],["f"],["g","c"],["m"],["i","h","j"],[],[],["k","a"],["l","e"],[]]
           

objects :: Objects
objects = M.fromList [ ("b",Object Small White Brick)
                     , ("d",Object Small Green Plank)
                     , ("f",Object Small Black Ball)
		     , ("g",Object Large Red Plank)
		     , ("c",Object Large Blue Table)
		     , ("m",Object Small Blue Box)
		     , ("i",Object Large Yellow Pyramid)
		     , ("h",Object Small Red Table)
		     , ("j",Object Small Red Pyramid)
		     , ("k",Object Large Yellow Box)
		     , ("a",Object Large Green Brick)
		     , ("l",Object Large Red Box)
		     , ("e",Object Large White Ball)
                     ]

pos :: Map Id (Int,Int)
pos = M.fromList [ ("b",(0,1))
                 , ("d",(0,2))
                 , ("f",(1,1))
		 , ("g",(2,1))
		 , ("c",(2,2))
		 , ("m",(3,1))
		 , ("i",(4,1))
		 , ("h",(4,2))
		 , ("j",(4,3))
		 , ("k",(7,1))
		 , ("a",(7,2))
		 , ("l",(8,1))
		 , ("e",(8,2))
                 ]
       
--utterance = ["put","the","ball","in","a","box","in","a","box","on","the","floor"]
--utterance = ["take", "the", "white", "ball", "in", "a", "box"]
utterance =["put","all","balls","in","a","box"]

goals :: Utterance -> [Goal]
goals utterance =
	let trees = parse command utterance :: [Command] in
		[goal | tree <- trees, goal <- interpret worldE Nothing objects tree] :: [Goal]


worldS = WState { _holding     = Nothing,
                  _positions   = pos,
                  _world       = worldE,
                  _objectsInfo = objects
		}
