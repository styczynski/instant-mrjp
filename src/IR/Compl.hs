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
--import IR.CodeGen.Generator
import IR.Syntax.Syntax
import qualified Data.Map as Map
import Reporting.Logs
import           IR.Syntax.Print                     as PrintEsp (Print,
                                                                          printTree,
                                                                          printTreeWithInstrComments)
--- Debug

-- Extract the element from the first instruction in the block.
nodeHead :: Node a b -> b
nodeHead node = let firstInstr = head $ nodeCode node
                in single firstInstr

-- Extract the element from the last instruction in the block.
nodeTail :: Node a b -> b
nodeTail node = let lastInstr = last $ nodeCode node
                in single lastInstr


showCfgs :: [(CFG a d, Method a)] -> String
showCfgs cfgs = unlines $ map showCfg cfgs
  where
    showCfg (g, Mthd _ _ (QIdent _ (SymIdent i1) (SymIdent i2)) _ _) =
      "CFG for " ++ i1 ++ "." ++ i2 ++ ":\n" ++ show g

cfgsToMthds :: b -> [(CFG a d, Method a)] -> [Method b]
cfgsToMthds default_ = map (\(g, Mthd _ r i ps _) ->
    Mthd default_ (default_ <$ r) (default_ <$ i) (map (default_ <$) ps) (map (fmap (const default_)) $ linearise $ g))

showCfgsWithLiveness :: [(CFG a Liveness, Method a)] -> String
showCfgsWithLiveness cfgs = unlines $ map showCfg cfgs
  where
    showCfg (CFG g, Mthd _ _ (QIdent _ (SymIdent i1) (SymIdent i2)) _ _) =
      "CFG for " ++ i1 ++ "." ++ i2 ++ ":\n" ++ show (CFG g) ++ concatMap showLiveness (Map.elems g)
    showLiveness node =
        "Liveness at start of " ++ toStr (node ^. nodeLabel) ++ ": " ++ show (nodeHead node) ++ "\n" ++
           "Liveness at end of " ++ toStr (node ^. nodeLabel) ++ ": " ++ show (nodeTail node) ++ "\n"

showEspWithLiveness :: Metadata a -> [Method (a, Liveness)] -> String
showEspWithLiveness meta mthds = PrintEsp.printTreeWithInstrComments (Program emptyLiveness (emptyLiveness <$ meta) (map (fmap (snd)) mthds))


genEspStep :: Metadata a -> [(CFG a (), Method a)] -> String -> LattePipeline ()
genEspStep meta cfgs comment = do
    let esp = PrintEsp.printTree $ Program () (() <$ meta) (cfgsToMthds () cfgs)
    printLogInfoStr $ ">> GENERATED INTERMEDIATE STEP " ++ comment ++ "\n\n"
    printLogInfoStr $ showCfgs cfgs
    printLogInfoStr $ ">> END STEP " ++ comment ++ "\n\n"

genEspWithLivenessStep :: Metadata a -> [(CFG a Liveness, Method a)] -> String -> LattePipeline ()
genEspWithLivenessStep meta cfgs comment = do
    let esp = showEspWithLiveness (() <$ meta) (cfgsToMthds ((), emptyLiveness) $ map (\(cfg, method) -> (mapCFGPos (const ()) cfg, () <$ method)) cfgs)
    printLogInfoStr $ ">> GENERATED INTERMEDIATE STEP " ++ comment ++ "\n\n"
    printLogInfoStr $ showCfgsWithLiveness cfgs
    printLogInfoStr $ ">> END STEP " ++ comment ++ "\n\n"

genEspWithLivenessAndIGsStep :: Metadata a -> [(SSA a Liveness, Method a, InterferenceGraph)] -> String -> LattePipeline ()
genEspWithLivenessAndIGsStep meta cfgs comment = do
    let cfgs' = map (\(SSA c, m, _) -> (c, m)) cfgs
        esp = showEspWithLiveness (() <$ meta) (cfgsToMthds ((), emptyLiveness) $ map (\(cfg, method) -> (mapCFGPos (const ()) cfg, () <$ method)) cfgs')
        ig_ = map (\(_, _, g) -> g) cfgs
    printLogInfoStr $ ">> GENERATED INTERMEDIATE STEP " ++ comment ++ "\n\n"
    printLogInfoStr $ showCfgsWithLiveness cfgs'
    printLogInfoStr $ show ig_
    printLogInfoStr $ ">> END STEP " ++ comment ++ "\n\n"

--- End debug

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
    let cfgsLin = map (uncurry removeUnreachable) cfgs
    let cfgsWithLiveness = map (first analyseLiveness) cfgsLin
    let ssaCode = map (\(g, mthd) -> (transformToSSA g mthd, mthd)) cfgsWithLiveness
    let optimisedCode = map (\(ssa, mthd) -> (optimise ssa mthd, mthd)) ssaCode
    let optimisedWithLiveness = map (first (\(SSA g) -> SSA $ analyseLiveness g)) optimisedCode
    let allocatedCfgs = map (\(cfg_, mthd) ->
            let (ig_, cfg') = getColouredInterferenceGraph cfg_
            in  (cfg', mthd, ig_)) optimisedWithLiveness
    let unfoldedPhi = map (\(SSA c, m, g) -> (unfoldPhi (SSA c) m (getRegisterAllocation c g), m, g)) allocatedCfgs
    optimisedCfgs <- mapM (\(c, m, g) -> do
        c' <- inlineTrivialBlocks c
        let (c'', m'') = removeUnreachable c' m
        return (c'', m'', g)) unfoldedPhi
    let finalCfgs = map (\(c, m, g) -> (removeDeadCode $ analyseLiveness c, m, g)) optimisedCfgs
    printLogInfoStr $ ">> GENERATED CFGS\n\n" ++ (showCfgs cfgs)
    genEspStep meta cfgsLin "Removing unreachable blocks..."
    genEspWithLivenessStep meta cfgsWithLiveness "Analysing liveness..."
    genEspStep meta (map (first unwrapSSA) ssaCode) "Transforming to SSA..."

    printLogInfoStr $ ">> ENTIRE SSA CODE DUMP \n\n"
    printLogInfoStr $ show ssaCode
    printLogInfoStr $ ">> END SSA CODE DUMP \n\n"

    genEspStep meta (map (first unwrapSSA) optimisedCode) "Optimising Espresso..."

    printLogInfoStr $ ">> ENTIRE OPTIMIZED CODE DUMP \n\n"
    printLogInfoStr $ show optimisedCode
    printLogInfoStr $ ">> END OPTIMIZED CODE DUMP \n\n"

    genEspWithLivenessStep meta (map (first unwrapSSA) optimisedWithLiveness) "Reanalysing liveness..."
    genEspWithLivenessAndIGsStep meta allocatedCfgs "Allocating registers..."
    genEspStep meta (map (\(c, m, _) -> (c, m)) unfoldedPhi) "Unfolding phis..."
    genEspStep meta (map (\(c, m, _) -> (c, m)) optimisedCfgs) "Inlining trivial jumps..."
    genEspWithLivenessStep meta (map (\(c, m, _) -> (c, m)) finalCfgs) "Final liveness analysis..."
    --show $ (map (\(c, m, _) -> (c, m)) finalCfgs)
    return $ CompiledProg pos meta (map (\(c, m, g) -> (c, m, getRegisterAllocation c g)) finalCfgs)
