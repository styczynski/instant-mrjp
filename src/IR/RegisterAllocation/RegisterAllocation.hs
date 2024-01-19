module IR.RegisterAllocation.RegisterAllocation where

import           Data.Bifunctor
import           Data.List
import qualified Data.Map                                             as Map
import           Data.Maybe
import           IR.Flow.CFG
import           IR.Flow.Liveness
import           IR.Flow.SSA
import           IR.Optimisation.DeadCode
import           IR.Syntax.Syntax
import           IR.Utils
import           IR.RegisterAllocation.InterferenceGraph
import           IR.RegisterAllocation.PerfectEliminationOrdering
import           IR.RegisterAllocation.SequenceColouring
import           IR.RegisterAllocation.Spilling

import qualified Backend.X64.Parser.Constructor as X64

data RegisterAllocation = RegAlloc {
    regAlloc  :: Map.Map ValIdent X64.Reg,
    numLocals :: Int
}

emptyRegisterAllocation :: RegisterAllocation
emptyRegisterAllocation = RegAlloc {
    regAlloc = Map.empty
    , numLocals = 0
}

-- Colour the interference graph, spilling variables if necessary.
getColouredInterferenceGraph :: SSA Liveness -> (InterferenceGraph, SSA Liveness)
getColouredInterferenceGraph (SSA g_) = go g_ 0
    where
        go g spilledLocals =
            let interference = buildInterferenceGraph g
                order = perfectEliminationOrdering interference
            in case sequenceColouring order interference of
                Right coloured -> (coloured, removeDeadCodeSSA $ SSA g)
                Left ns ->
                    let spills = map (spill g spilledLocals) ns
                        best   = head $ sortOn estCost spills
                        newCFG = analyseLiveness (spilledCFG best)
                    in go newCFG (spilledLocals + 1)

getRegisterAllocation :: CFG a -> InterferenceGraph -> RegisterAllocation
getRegisterAllocation (CFG cfg_) ig_ =
    let ptrLocals = concatMap (mapMaybe findPtrLocals . nodeCode) $ Map.elems cfg_
        locCnt = length $ dedup ptrLocals
        alloc = Map.fromList $ map (first ValIdent) $ Map.toList (getColouring ig_)
    in  RegAlloc alloc locCnt

usedRegs :: RegisterAllocation -> [X64.Reg]
usedRegs (RegAlloc r _) = dedup $ Map.elems r

findPtrLocals :: Instr a -> Maybe Integer
findPtrLocals instr = case instr of
    ILoad _ _ (PLocal _ _ n)  -> Just n
    IStore _ _ (PLocal _ _ n) -> Just n
    _                         -> Nothing