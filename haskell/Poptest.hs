module PopTest where

import Popper
import ShrdliteGrammar
import DataTypes
import qualified Data.Sequence as Seq
import qualified Data.List as List
import qualified Data.Foldable as Whatev

-- Closed world assumption -- Calculate this..
main = head $ pop nulltriple agenda actionpool
    where starteffects = [Rel "ball" Ontop "floor1", Clear "ball", 
                          Rel "box" Ontop "floor2", Clear "box", 
                          Clear "floor3"
                         ]
          endpreconds  = [Rel "ball" Ontop "floor2"]
          nulltriple = ([(Action "end" endpreconds []),(Action "start" [] starteffects)]
                        ,[(LessThan (Action "start" [] starteffects) (Action "end" endpreconds []))] 
                        ,[] )
          agenda = foldr (Seq.<|) Seq.empty [(conj,(Action "end" endpreconds [])) | conj <- endpreconds] 
          actionpool = [ Action "m_ball_f1_f2" 
                       [Rel "ball" Ontop "floor1", Clear "ball", Clear "floor2"] 
                       [Rel "ball" Ontop "floor2", Clear "floor1", Not (Rel "bsll" Ontop "floor1"), Not (Clear "floor2")]
                     ,
                       Action "m_ball_f1_f3" 
                       [Rel "ball" Ontop "floor1", Clear "ball", Clear "floor3"] 
                       [Rel "ball" Ontop "floor3", Clear "floor1", Not (Rel "bsll" Ontop "floor1"), Not (Clear "floor3")]
                     ,
                       Action "m_box_f2_f1" 
                       [Rel "box" Ontop "floor2", Clear "box", Clear "floor1"] 
                       [Rel "box" Ontop "floor1", Clear "floor2", Not (Rel "box" Ontop "floor2"), Not (Clear "floor1")]
                     
                     ,
                       Action "m_box_f2_f3" 
                       [Rel "box" Ontop "floor2", Clear "box", Clear "floor3"] 
                       [Rel "box" Ontop "floor3", Clear "floor2", Not (Rel "box" Ontop "floor2"), Not (Clear "floor3")]
                     ]                                                             
                     
                     
--let order  = [(LessThan (Action "a" [] []) (Action "b" [] [])), (LessThan (Action "c" [] []) (Action "d" [] [])), (LessThan (Action "b" [] []) (Action "c" [] []))]
--let order2 = [(LessThan (Action "a" [] []) (Action "b" [] [])), (LessThan (Action "c" [] []) (Action "d" [] [])), (LessThan (Action "b" [] []) (Action "d" [] []))]

--let orderTest    = [(LessThan (Action "a" [Clear "ball"] []) (Action "b" [] [Clear "ball"]))]
--let goalTest     = (Clear "ball",Action "a" [Clear "ball"] [])
--let actionsTest  = [Action "a" [Clear "ball"] [],Action "b" [] [Clear "ball"]]