module Example where

import Shrdlite
import ShrdliteGrammar
import CombinatorParser
import Data.Map as M
import DataTypes
  
world  :: World
world   = [["ball1", "box1"],["brick1","table2"],["ball3"],["ball4","box2","table1","brick3"],[]]

objects :: Objects
objects = M.fromList [ ("box1",Object Large Red Box)
                     , ("ball1",Object Small White Ball)
                     , ("brick1",Object Small Green Brick)
					 , ("table2",Object Large White Table)
					 , ("ball3",Object Small White Ball)
					 , ("brick3",Object Large Blue Brick)
					 , ("table1",Object Small Yellow Table)
					 , ("box2",Object Small Green Box)
					 , ("ball4",Object Small Red Ball)
                     ]

--utterance = ["put","the","ball","in","a","box","in","a","box","on","the","floor"]
--utterance = ["take", "the", "white", "ball", "in", "a", "box"]
utterance = ["take", "the", "white", "table"]

goals :: Utterance -> [Goal]
goals utterance =
	let trees = parse command utterance :: [Command] in
		[goal | tree <- trees, goal <- interpret world Nothing objects tree] :: [Goal]
