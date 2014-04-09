module Examples.Example2 where

import Shrdlite
import Plan
import Interpreter
import ShrdliteGrammar
import CombinatorParser
import Data.Map as M
import DataTypes
  
worldE  :: World
worldE   = [["sb"],["bb"],["yb"],["lb"],[]]
           

objects :: Objects
objects = M.fromList [ ("sb",Object Small Black Ball)
		     , ("bb",Object Large Blue Box)
		     , ("yb",Object Large Yellow Box)
		     , ("lb",Object Large White Ball)
                     ]

pos :: Map Id (Int,Int)
pos = M.fromList [ ("sb",(0,1))
                 , ("bb",(1,1))
                 , ("yb",(2,1))
                 , ("lb",(3,1)) ]
                                
       
utterance =["put", "every" ,"ball" ,"in", "a" ,"box"]

goals :: Utterance -> [Goal]
goals utterance =
	let trees = parse command utterance :: [Command] in
		[goal | tree <- trees, goal <- interpret worldE Nothing objects tree] :: [Goal]


worldS = WState { holding     = Nothing,
                  positions   = pos,
                  world       = worldE,
                  objectsInfo = objects
		}
