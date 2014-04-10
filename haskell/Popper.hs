module Popper where

import ShrdliteGrammar
import DataTypes
import qualified Data.Sequence as Seq
import qualified Data.List as List
import qualified Data.Foldable as Whatev
-- PopState = (Actions, Ordering of actions, Causal links between actions)
-- data PopState = ([Action],[LessThan],[Clink])
--                  deriving (Eq, Ord, Show)
-- FIFO queue 
-- data Seq (Conjunct,Action) = Seq (Conjunct,Action)

-- data Goal = (Conjunct,Action)

-- <     
-- data Order = [LessThan]
data LessThan = LessThan Action Action
                deriving (Eq, Ord, Show)
-- Causal links
data Clink = Clink Action Conjunct Action
             deriving (Eq, Ord, Show)
-- Single 'Feature atom' from precondition or effect conjunction
-- Disjunction? tricky queries?
-- Clear Id <=> explicitly saying there is no object ontop of Id
data Conjunct = Rel Id Relation Id | Clear Id | Neg Conjunct
                deriving (Eq, Ord, Show)
-- Is new or old. Has a name, preconditions and effects
data Action = Action Id [Conjunct] [Conjunct]
              deriving (Eq, Ord)
instance Show Action where
    show (Action id _ _) = id
    
-- pop starter - and 'stopper'?
-- Translates world from stacks to features
-- Calls pop with 'null plan' - init state and goals -
--                and a pool of actions

-- Translates high lvl actions into move i or put i
-- populator ::


pop :: ([Action],[LessThan],[Clink]) -> Seq.Seq (Conjunct,Action) -> [Action] -> [([Action],[LessThan],[Clink])]
pop (actions, order, clinks) agenda actionpool
  | Seq.null agenda = [(actions, order, clinks)]
  | otherwise = 
   concat [ pop (actions2, order3, clinks2) agenda2 actionpool | 
          action <- possibleActions goal order actions actionpool, 
          
          let actions2 = updateActions action actions,
          let order2 = updateOrder action goal actions order,
          let clinks2 = updateClinks action goal clinks,
          let agenda2 = updateAgenda action goal agenda actions,
    
          order3 <- newWorldAdditions actions2 order2 clinks2
   ]
  where goal = head (Whatev.toList (Seq.take 1 agenda)) -- lol.. yes

  
updateActions :: Action -> [Action] -> [Action]
updateActions a actions 
  | elem a actions = 
    actions
  | otherwise = 
    (a:actions)

updateAgenda :: Action -> (Conjunct,Action) -> Seq.Seq (Conjunct,Action) -> [Action] -> Seq.Seq (Conjunct,Action) 
updateAgenda (Action id pre eff) goal agenda actions
  | elem (Action id pre eff) actions = 
    Seq.drop 1 agenda
  | otherwise = 
    (Seq.drop 1 agenda)
      Seq.>< (Seq.fromList (zip pre (replicate (length pre) (Action id pre eff))))

      
-- Makes two identical lessthan sometimes -- end and a2 equal
updateOrder :: Action -> (Conjunct,Action) -> [Action] -> [LessThan] -> [LessThan]
updateOrder a1 (q,a2) actions order 
  | elem a1 actions = 
    (LessThan a1 a2):order
  | otherwise = 
    (LessThan a1 finish):(LessThan start a1):(LessThan a1 a2):order    
  where start  = last actions
        finish = actions !! (length actions - 2)
        
updateClinks :: Action -> (Conjunct,Action) -> [Clink] -> [Clink]
updateClinks a1 (q,a2) clinks = (Clink a1 q a2):clinks

-- Action-picking

possibleActions :: (Conjunct,Action) -> [LessThan] -> [Action] -> [Action] -> [Action]
possibleActions goal order actions actionpool = 
  actions2 ++ actionpool2 
    where actions3 = filter ((snd goal) /= ) actions2
          actions2 = filter (orderCons goal order) (filter (goalInEff goal) actions)
          actionpool2 = filter (goalInEff goal) actionpool
  
goalInEff :: (Conjunct,Action) -> Action -> Bool
goalInEff goal (Action _ _ effs) = elem (fst goal) effs

orderCons :: (Conjunct,Action) -> [LessThan] -> Action -> Bool
orderCons goal order a = (not (elem a (transClos order (snd goal)))) 
                           && (a /= (snd goal))
                           
transClos :: [LessThan] -> Action -> [Action]
transClos order a = transClos2 secondStep order
  where secondStep = stepForward order a

transClos2 :: [Action] -> [LessThan] -> [Action] 
transClos2 closure order 
  | closure == closureUntilNow = closure
  | otherwise                  = transClos2 closureUntilNow order
  where closureUntilNow = 
          List.nub $ closure ++ (concat (map (stepForward order) closure))
            
-- Base case correct? probably since it's  <  and not  <=
stepForward :: [LessThan] -> Action -> [Action]
stepForward [] p = [] 
stepForward (LessThan a1 a2 : ords) p
  | a1 == p       = a2 : stepForward ords p
  | otherwise     = stepForward ords p 


  
-- Order-picking  
  
newWorldAdditions :: [Action] -> [LessThan] -> [Clink] -> [[LessThan]]
newWorldAdditions actions order clinks
  | null (findThreats actions clinks order) = 
    [order]
  | otherwise =  
    concat  
    [ newWorldAdditions actions (order ++ [c]) clinks
    | (a,cl) <- findThreats actions clinks order,
       c <- possibleOrd order (a,cl)   
    ] 
  
findThreats :: [Action] -> [Clink] -> [LessThan] -> [(Action,Clink)]
findThreats actions clinks order =
  [(a,cl) | a <- actions, cl <- clinks, isThreat order (a,cl) ]


possibleOrd :: [LessThan] -> (Action,Clink) -> [LessThan]
possibleOrd order (a,cl) = [ x | c <- [-1,1], 
                             let x = buildCrap c (a,cl),
                             isConsistent order x cl
                            ]

isConsistent :: [LessThan] -> LessThan -> Clink -> Bool                       
isConsistent order (LessThan at1 at2) (Clink a1 _ a2)
  | at2 == a1 =
    not ((elem at1 (transClos order a1)) || 
         (elem at1 (transClos order a2)))     
  | at1 == a2 = 
    not ((elem a1 (transClos order at2)) || 
         (elem a2 (transClos order at2)))

                       
buildCrap :: Int -> (Action,Clink) -> LessThan

buildCrap (-1) (at,Clink a1 _ a2) = LessThan at a1 
buildCrap  1   (at,Clink a1 _ a2) = LessThan a2 at

-- Haskell abuse!! 
isThreat :: [LessThan] -> (Action,Clink) -> Bool
isThreat order (Action id pre eff,Clink a1 q a2) = 
  (not ((elem (Action id pre eff) (transClos order a2)) || 
       (elem a1 (transClos order (Action id pre eff)))))
   && (elem (Neg q) eff)
  
-- Backlog

-- Implement working v1
-- edit stack q

-- Implement working v2

-- Handle any/all quantifiers
-- (Fix disjunctive preconditions)
-- => Can handle advanced queries?

-- Preserve the entities relation to its 'location objects'?

-- Better datastructures