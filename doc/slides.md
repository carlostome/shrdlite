### How it works

1. Receives the set of parse trees derived from the utterance given.
2. Interprets each parse tree in the current world.
3. Given a goal and the current world, plans how to achieve it with minimum cost.
4. Returns the output to the interface.

* * *

### Interpretation: Ambiguities 

#### Two kinds of ambiguities:

+ There is more than one tree that can deliver any number of goals.
  + We don't handle this kind of ambiguity.
+ A tree can be interpreted in different ways.
  + We send as an output a set of sentences with the same meaning but more concretion.

* * *

### Interpretation: Quantifiers (I)

Goals are defined in this way:

```haskell
data Goal = And [Goal]
          | Or [Goal]
          | MoveObj Id Relation Id
          | TakeObj Id deriving (Eq, Show)
```

* * *

### Interpretation: Quantifiers (II)

*Example 1*: Put all balls in a box

```haskell
And [ Or [MoveObj ball1 Inside box1, MoveObj ball1 Inside box2, ...]
    , Or [MoveObj ball2 Inside box1, MoveObj ball2 Inside box2, ...]]
```

* * *

### Interpretation: Quantifiers (and III)
*Example 2*: Put the ball in a box

```haskell
Or [ Or [MoveObj ball Inside box1, MoveObj ball Inside box2]]
```

If there is more than one ball, this results in an ambiguity error and we suggest the user an unique 
description of all the objects that matches the given criteria.

* * *

### Planning: State space

#### State representation

```haskell
data WorldState = WState { _holding     :: Maybe Id,
                           _positions   :: M.Map Id (Int, Int),
                           _world       :: World,
-- Useful, but not part of the state (it doesn't change with the actions).
                           _objectsInfo :: M.Map Id Object } deriving Show
```

```haskell
actions :: WorldState -> [Action]
isSolution :: WorldState -> Goal -> Bool
transition :: WorldState -> Action -> WorldState
```
* * *

### Planning: BFS

+ Easy to implement.
+ Fairly efficient for simple tasks.
+ Cost function:
  + c(pick an object) = 1
  + c(drop an object) = 1
  + Doesn't take into account how long the arm moves to perform each action.

* * *

### Planning: A*

+ Still easy to implement.
+ More efficient at some tasks.
+ Not as efficient at others: 
  + The heuristics add some overhead to the process. 

* * *
### Planning: Heuristics (on top of / inside)

```haskell

heuristicAStar :: WorldState -> Goal -> Int
heuristicAStar worldState goal@(MoveObj id1 Ontop id2)
  case _holding worldState of
    Nothing  ->
      if id2 /= "Floor" 
         && relationHolds worldState id2 Above id1 then
        movesToFreeId1 + 2 * (y1 - y2)
      else
        movesToFreeId1 + movesToFreeId2
    Just obj 
      | obj == id1 -> movesToFreeId2
      | otherwise  -> movesToFreeId1 + movesToFreeId2
    where
      movesToFreeId1 = ...
      movesToFreeId2 = ... 
```
* * *
### Planning: Heuristics (Leftof)

```haskell
heuristicAStar :: WorldState -> Goal -> Int
heuristicAStar worldState goal@(MoveObj id1 Leftof id2)
  Leftof -> [cost1 + cost2 | (index1, cost1) <- costs1
                           , (index2, cost2) <- costs2
                           , index1 < index2]
    where
      costs1 = zip [1..] $ calculateCosts id1
      costs2 = zip [1..] $ calculateCosts id2
      calculateCosts id = ...
```
* * *
### Planning: Heuristics (And/Or)

```haskell
heuristicAStar :: WorldState -> Goal -> Int
heuristicAStar worldState (And goals) =
  maximum $  map (heuristicAStar worldState) goals
heuristicAStar worldState (Or goals) =
  minimum $  map (heuristicAStar worldState) goals
```

* * *

### Planning: Comparison
![Comparison table (states travelled)](http://i.gyazo.com/6f37371c2d626a348c7b21079eaf91dc.png)

* * *

### Suggestions

+ Suggest some interesting use cases
+ Show that system can handle difficult tasks
+ Simple moevements, but hard to solve

+ E.g.
  + `put the white brick on the table`
  + `put the green ball inside the red box`

* * *

### Generating suggestions

+ Generate all possible movements
+ Sort them by hardness
  + Hardness from existing heuristics

* * *

### Smallest object description (I)

+ Objects have three attributes
+ Constructor
  + `Object Size Color Form`
+ Instantiation 
  + `Object small red brick`
  + `Object AnySize AnyColor brick`

* * *

### Smallest object description (II)

+ Often redundant attributes, because object is unique
+ Keep only relevant attributes
  + Always keep `Form`

+ Generate all combinations of attributes
+ Check if object is unique
+ Take description with fewest attributes

* * *

### Ambiguity in the description

+ If ambiguity, relate object to others
+ Do the same as above with its attributes

* * *

### Extensions implemented

+ Quantifiers
+ Ambiguity resolution
+ Suggestions
+ Handling complex worlds
  + Heuristics
+ *Partial-order planner*
  + Not finished

* * *
### Future lines

+ Handle all kinds of ambiguities
  + More than one tree with valid interpretation.
  + More than one object with the exact same description.

