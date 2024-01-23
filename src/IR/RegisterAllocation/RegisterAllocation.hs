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
import           IR.RegisterAllocation.PEO
import           IR.RegisterAllocation.SequenceColouring
import           IR.RegisterAllocation.RegisterSpilling

import qualified Backend.X64.Parser.Constructor as X64

data RegisterAllocation = RegisterAllocation {
    regAlloc  :: Map.Map IRValueName X64.Reg,
    numLocals :: Int
} deriving (Show)

emptyRegisterAllocation :: RegisterAllocation
emptyRegisterAllocation = RegisterAllocation {
    regAlloc = Map.empty
    , numLocals = 0
}

findPtrLocals :: Instr a -> Maybe Integer
findPtrLocals instr = case instr of
    ILoad _ _ (PLocal _ _ n)  -> Just n
    IStore _ _ (PLocal _ _ n) -> Just n
    _                         -> Nothing

getColouredInterferenceGraph :: SSA a Liveness -> (InterferenceGraph, SSA a Liveness)
getColouredInterferenceGraph (SSA g_) = go g_ 0
    where
        go g spilledLocals =
            let interference = buildInterferenceGraph g
                order = perfectEliminationOrdering interference
            in case seqColouring order interference of
                Right coloured -> (coloured, removeDeadCodeSSA $ SSA g)
                Left ns ->
                    let spills = map (spill g spilledLocals) ns
                        best   = head $ sortOn _spillEstimatedCost spills
                        newCFG = analyseLiveness (_spillSourceCFG best)
                    in go newCFG (spilledLocals + 1)

getRegisterAllocation :: CFG a d -> InterferenceGraph -> RegisterAllocation
getRegisterAllocation (CFG cfg_) ig_ =
    let ptrLocals = concatMap (mapMaybe findPtrLocals . nodeCode) $ Map.elems cfg_
        locCnt = length $ dedup ptrLocals
        alloc = Map.fromList $ map (first IRValueName) $ Map.toList (getColouring ig_)
    in  RegisterAllocation alloc locCnt

usedRegs :: RegisterAllocation -> [X64.Reg]
usedRegs (RegisterAllocation r _) = dedup $ Map.elems r

