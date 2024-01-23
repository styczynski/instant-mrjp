module IR.RegisterAllocation.PEO (perfectEliminationOrdering) where

import           Data.List
import qualified Data.Map                                    as Map
import qualified Data.Set                                    as Set
import           IR.RegisterAllocation.InterferenceGraph


perfectEliminationOrdering :: InterferenceGraph -> [String]
perfectEliminationOrdering = runSimpleBFS

runSimpleBFS :: InterferenceGraph -> [String]
runSimpleBFS (IG g) = run [] [Map.keys g]
  where run _   [[]] = [] 
        run acc sets
          | all isSingleton sets = concat sets ++ acc
          | otherwise            = singleIteration acc sets
        singleIteration acc ([v]:sets)    = run (v:acc) $ sets      `split` v
        singleIteration acc ((v:vs):sets) = run (v:acc) $ (vs:sets) `split` v
        singleIteration _ ([]:_)          = error "internal error. runSimpleBFS: empty set"
        singleIteration acc []            = acc
        split [] _ = []
        split (set:sets) v =
            let (neighbours, nonneighbours) = partition (isNeighbour v) set
            in if any null [neighbours, nonneighbours]
                 then set:(sets `split` v)
                 else nonneighbours:neighbours:(sets `split` v)
        isNeighbour v u = u `Set.member` iNodeOut (g Map.! v)

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False
