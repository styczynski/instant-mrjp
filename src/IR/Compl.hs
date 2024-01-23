{-# LANGUAGE DeriveTraversable #-}
module IR.Compl where

import Data.Tuple.Append

import Control.Lens hiding (Const)
import Data.Functor
import           Data.Bifunctor                              (Bifunctor (first))
import           IR.Flow.CFG
import           IR.Flow.Liveness               (Liveness,
                                                              analyseLiveness,
                                                              emptyLiveness)
import           IR.Flow.SSA
import           IR.Optimisation.CFGTransform          (inlineTrivialBlocks,
                                                              removeUnreachable)
import           IR.Optimisation.DeadCode
import           IR.Optimisation.Pipeline
import IR.RegisterAllocation.InterferenceGraph
import IR.RegisterAllocation.RegisterAllocation
import IR.Utils
import IR.Phi
import IR.Syntax.Syntax
import qualified Data.Map as Map
import Reporting.Logs
import           IR.Syntax.Print                     as PrintUtils (Print,
                                                                          printTree,
                                                                          printTreeWithInstrComments)

nodeHead :: Node a b -> b
nodeHead node = let firstInstr = head $ nodeCode node
                in single firstInstr

nodeTail :: Node a b -> b
nodeTail node = let lastInstr = last $ nodeCode node
                in single lastInstr


showCfgs :: [(CFG a d, Method a)] -> String
showCfgs cfgs = unlines $ map showCfg cfgs
  where
    showCfg (g, m@(Mthd _ _ (QIdent _ (IRTargetRefName i1) (IRTargetRefName i2)) _ body)) =
      "CFG for " ++ i1 ++ "." ++ i2 ++ ":\n" ++ show g ++ "Code: " ++ printTree m

cfgsToMthds :: b -> [(CFG a d, Method a)] -> [Method b]
cfgsToMthds default_ = map (\(g, Mthd _ r i ps _) ->
    Mthd default_ (default_ <$ r) (default_ <$ i) (map (default_ <$) ps) (map (fmap (const default_)) $ linearise $ g))

showCfgsWithLiveness :: [(CFG a Liveness, Method a)] -> String
showCfgsWithLiveness cfgs = unlines $ map showCfg cfgs
  where
    showCfg (CFG g, m@(Mthd _ _ (QIdent _ (IRTargetRefName i1) (IRTargetRefName i2)) _ _)) =
      "CFG for " ++ i1 ++ "." ++ i2 ++ ":\n" ++ show (CFG g) ++ concatMap showLiveness (Map.elems g) ++ "Code: " ++ printTree m
    showLiveness node =
        "Liveness at start of " ++ toStr (node ^. nodeLabel) ++ ": " ++ show (nodeHead node) ++ "\n" ++
           "Liveness at end of " ++ toStr (node ^. nodeLabel) ++ ": " ++ show (nodeTail node) ++ "\n"

showEspWithLiveness :: Metadata a -> [Method (a, Liveness)] -> String
showEspWithLiveness meta mthds = PrintUtils.printTreeWithInstrComments (Program emptyLiveness (emptyLiveness <$ meta) (map (fmap (snd)) mthds))


runCodeGenerationStep :: Metadata a -> [(CFG a (), Method a)] -> String -> LattePipeline ()
runCodeGenerationStep meta cfgs comment = do
    let esp = PrintUtils.printTree $ Program () (() <$ meta) (cfgsToMthds () cfgs)
    printLogInfoStr $ ">> GENERATED INTERMEDIATE STEP " ++ comment ++ "\n\n"
    printLogInfoStr $ showCfgs cfgs
    printLogInfoStr $ ">> END STEP " ++ comment ++ "\n\n"

runCodeGenerationWithLivenessStep :: Metadata a -> [(CFG a Liveness, Method a)] -> String -> LattePipeline ()
runCodeGenerationWithLivenessStep meta cfgs comment = do
    let esp = showEspWithLiveness (() <$ meta) (cfgsToMthds ((), emptyLiveness) $ map (\(cfg, method) -> (mapCFGPos (const ()) cfg, () <$ method)) cfgs)
    printLogInfoStr $ ">> GENERATED INTERMEDIATE STEP " ++ comment ++ "\n\n"
    printLogInfoStr $ showCfgsWithLiveness cfgs
    printLogInfoStr $ ">> END STEP " ++ comment ++ "\n\n"

runCodeGenerationWithLivenessAndIGsStep :: Metadata a -> [(SSA a Liveness, Method a, InterferenceGraph)] -> String -> LattePipeline ()
runCodeGenerationWithLivenessAndIGsStep meta cfgs comment = do
    let cfgs' = map (\(SSA c, m, _) -> (c, m)) cfgs
        esp = showEspWithLiveness (() <$ meta) (cfgsToMthds ((), emptyLiveness) $ map (\(cfg, method) -> (mapCFGPos (const ()) cfg, () <$ method)) cfgs')
        ig_ = map (\(_, _, g) -> g) cfgs
    printLogInfoStr $ ">> GENERATED INTERMEDIATE STEP " ++ comment ++ "\n\n"
    printLogInfoStr $ showCfgsWithLiveness cfgs'
    printLogInfoStr $ show ig_
    printLogInfoStr $ ">> END STEP " ++ comment ++ "\n\n"


data CompiledProg a =
    CompiledProg a (Metadata a) [(CFG a Liveness, Method a, RegisterAllocation)]
    deriving (Show)

instance Eq (CompiledProg a) where
    (==) (CompiledProg _ meta1 methods1) (CompiledProg _ meta2 methods2) =
        let normalizeMethod = (\(cfg, method, reg) -> (() <$ cfg, () <$ method)) in
        let methods1' = map normalizeMethod methods1 in
        let methods2' = map normalizeMethod methods2 in
        (() <$ meta1) == (() <$ meta2) && methods1' == methods2'

instance Functor (CompiledProg) where
    fmap f (CompiledProg pos meta methods) = CompiledProg (f pos) (fmap f meta) (map (\(cfg, method, reg) -> (mapCFGPos f cfg, fmap f method, reg)) methods)



compl_ :: (Show a) => (Program a) -> LattePipeline (CompiledProg a)
compl_ (Program pos meta mthds) = do
    let cfgs = zip (map cfg mthds) mthds
    let linearCFGs = map (uncurry removeUnreachable) cfgs
    let cfgsAnnotated = map (first analyseLiveness) linearCFGs
    let ssaCode = map (\(g, mthd) -> (transformToSSA g mthd, mthd)) cfgsAnnotated
    let optCode = map (\(ssa, mthd) -> (optimise ssa mthd, mthd)) ssaCode
    let optWithLiveness = map (first (\(SSA g) -> SSA $ analyseLiveness g)) optCode
    let allocatedCfgs = map (\(cfg_, mthd) ->
            let (ig_, cfg') = getColouredInterferenceGraph cfg_
            in  (cfg', mthd, ig_)) optWithLiveness
    let unfoldedPhi = map (\(SSA c, m, g) -> (unfoldPhi (SSA c) m (getRegisterAllocation c g), m, g)) allocatedCfgs
    optCfgs <- mapM (\(c, m, g) -> do
        c' <- inlineTrivialBlocks c
        let (c'', m'') = removeUnreachable c' m
        return (c'', m'', g)) unfoldedPhi
    let finalCfgs = map (\(c, m, g) -> (removeDeadCode $ analyseLiveness c, m, g)) optCfgs
    printLogInfoStr $ ">> GENERATED CFGS\n\n" ++ (showCfgs cfgs)
    runCodeGenerationStep meta linearCFGs "Removing unreachable blocks..."
    runCodeGenerationWithLivenessStep meta cfgsAnnotated "Analysing liveness..."
    runCodeGenerationStep meta (map (first unwrapSSA) ssaCode) "Transforming to SSA..."

    printLogInfoStr $ ">> ENTIRE SSA CODE DUMP \n\n"
    printLogInfoStr $ show ssaCode
    printLogInfoStr $ ">> END SSA CODE DUMP \n\n"

    runCodeGenerationStep meta (map (first unwrapSSA) optCode) "Optimising ..."

    printLogInfoStr $ ">> ENTIRE OPTIMIZED CODE DUMP \n\n"
    printLogInfoStr $ show optCode
    printLogInfoStr $ ">> END OPTIMIZED CODE DUMP \n\n"

    runCodeGenerationWithLivenessStep meta (map (first unwrapSSA) optWithLiveness) "Reanalysing liveness..."
    runCodeGenerationWithLivenessAndIGsStep meta allocatedCfgs "Allocating registers..."
    runCodeGenerationStep meta (map (\(c, m, _) -> (c, m)) unfoldedPhi) "Unfolding phis..."
    runCodeGenerationStep meta (map (\(c, m, _) -> (c, m)) optCfgs) "Inlining trivial jumps..."
    runCodeGenerationWithLivenessStep meta (map (\(c, m, _) -> (c, m)) finalCfgs) "Final liveness analysis..."
    return $ CompiledProg pos meta (map (\(c, m, g) -> (c, m, getRegisterAllocation c g)) finalCfgs)
