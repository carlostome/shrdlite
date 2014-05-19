### How it works

1. Receives the set of parse trees derived from the utterance given.
2. Interprets each parse tree in the current world.
3. Given a goal and the current world, plans how to achieve it with minimum cost.
4. Returns the output to the interface.


* * *

### Interpretation (I)

#### Two kind of ambiguities:

+ There is more than one tree that can deliver any number of goals.
  + We don't handle this kind of ambiguity.
+ A tree can be interpreted in different ways.
  + We send as an output a set of sentences with the same meaning but more concretion.

* * *

### Interpretation (II)
#### Quantifiers
Goals are defined in this way:

```haskell
data Goal = And [Goal]
          | Or [Goal]
          | MoveObj Id Relation Id
          | TakeObj Id deriving (Eq, Show)
```

*Example 1*: Put all balls in a box

```haskell
And [ Or [MoveObj ball1 Inside box1, MoveObj ball1 Inside box2, ...]
    , Or [MoveObj ball2 Inside box1, MoveObj ball2 Inside box2, ...]]
```

*Example 2*: Put the ball in a box

```haskell
Or [ Or [MoveObj ball Inside box1, MoveObj ball Inside box2]]
```

If there is more than one ball, this results in an ambiguity error and we suggest the user an unique 
description of all the objects that matches the given criteria.

* * *

### Planning (I)

#### State representation

A state consists of:

```haskell
data WorldState = WState { _holding     :: Maybe Id,
                           _positions   :: M.Map Id (Int, Int),
                           _world       :: World,
                           -- Useful, but not part of the state (it doesn't change with the actions).
                           _objectsInfo :: M.Map Id Object } deriving Show
```


* * *

### Planning (II)

#### First approach: BFS

Classic breadth-first search algorithm:

+ Easy to implement.
+ Fairly efficient for simple tasks.
+ Cost function:
  + c(pick an object) = 1
  + c(drop an object) = 1
  + Doesn't take into account how long the arm moves to perform each action.

* * *

### Planning (III)

#### Second approach: A*

BFS is not always enough. A* turned out to be:
+ Still easy to implement.
+ More efficient at some tasks.
+ Not as efficient at others: 
  + The heuristics add some overhead to the process. 

* * *
### Planning (IV)

##### Heuristics

```haskell

heuristicAStar worldState (And goals) =
  maximum $  map (heuristicAStar worldState) goals
heuristicAStar worldState (Or goals) =
  minimum $  map (heuristicAStar worldState) goals
heuristicAStar worldState goal@(MoveObj id1 rel id2)
    case rel of
      Ontop -> case _holding worldState of
                 Nothing  ->
                   if id2 /= "Floor" && relationHolds worldState id2 Above id1 then
                     movesToFreeId1 + 2 * (y1 - y2)
                   else
                     movesToFreeId1 + movesToFreeId2
                 Just obj 
                   | obj == id1 -> movesToFreeId2
                   | otherwise  -> movesToFreeId1 + movesToFreeId2
                      
        where
          movesToFreeId1 = 2 * length (_world worldState !! x1) - y1
          movesToFreeId2 = if id2 == "Floor" then 2 * minimum (map length (_world worldState))
                           else 2 * length (_world worldState !! x2) - y2

      Leftof -> [cost1 + cost2 | (index1, cost1) <- costs1
                               , (index2, cost2) <- costs2
                               , index1 < index2]
      where
        costs1 = zip [1..] $ calculateCosts id1
        costs2 = zip [1..] $ calculateCosts id2
        calculateCosts id = map (stackheuristicAStar id) $ _world worldState
```

* * *

### Planning (V)

#### Comparison
| World        | Utterance      | A* | BFS | Improvement | 
| ------------- |:-------------:| -----:|-----:|-----:|
|    Small   | put the white ball in a box on the floor                | 13     | 141     | 984.61%    
|    Small   | put all boxes on the floor                              | 62     | 75      | 20.96%     
|    Medium  | move the large ball inside a yellow box on the floor    | 17     | 11633   | 68329.41%  
|    Medium  | put the brick that is to the left of a pyramid in a box | 9      | 257     | 2755.55%   
|    Complex | put all boxes on the floor                              | 393788 | 969239  | 146.13%    
|    Complex | put all red objects on the floor                        | 274328 | 599034  | 118.36%    

* * *

### Suggestions


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

