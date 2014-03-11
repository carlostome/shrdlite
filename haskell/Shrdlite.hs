#!/usr/bin/env runhaskell

-- You need the 'json' package: cabal install json

-- Test from the command line:
-- runhaskell Shrdlite.hs < ../examples/medium.json

module Main where 

import ShrdliteGrammar
import CombinatorParser
import Text.JSON
import Data.List (findIndex)

type Utterance = [String]
type Id = String
type World = [[Id]]
type Objects = JSObject JSValue
type Goal = Bool
type Plan = [String]


main :: IO ()
main = getContents >>= putStrLn . encode . jsonMain . ok . decode


jsonMain :: JSObject JSValue -> JSValue
jsonMain jsinput = makeObj result
    where 
      utterance = ok (valFromObj "utterance" jsinput) :: Utterance
      world     = ok (valFromObj "world"     jsinput) :: World
      holding   = ok (valFromObj "holding"   jsinput) :: Id
      objects   = ok (valFromObj "objects"   jsinput) :: Objects

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
                   ("output",    showJSON output)
                  ]


interpret :: World -> Id -> Objects -> Command -> [Goal]
interpret world holding objects tree = [True]


solve :: World -> Id -> Objects -> Goal -> Plan
solve world holding objects goal = ["I picked it up . . .", "pick " ++ show col, ". . . and I dropped it down", "drop " ++ show col]
    where
      Just col = findIndex (not . null) world


ok :: Result a -> a
ok (Ok res) = res
ok (Error err) = error err

