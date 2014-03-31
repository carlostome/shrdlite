module Example where

import Shrdlite
import ShrdliteGrammar
import CombinatorParser
import Data.Map as M
import DataTypes
  
world  :: World
world   = [["ball","box1"],[],[],["box2"]]

objects :: Objects
objects = M.fromList [ ("ball",Object Small Red Ball)
                     , ("box1",Object Small Yellow Box)
                     , ("box2",Object Large Blue Box)
                     ]

utterance = ["put","the","ball","in","a","box"]
