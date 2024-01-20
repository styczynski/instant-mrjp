module IR.Compl where

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
import IR.CodeGen.Generator
import IR.Syntax.Syntax
import Reporting.Logs
import qualified Data.Map as Map
import           IR.Syntax.Print                     as PrintEsp (Print,
                                                                          printTree,
                                                                          printTreeWithInstrComments)

-- Extract the element from the first instruction in the block.
nodeHead :: Node a -> a
nodeHead node = let firstInstr = head $ nodeCode node
                in single firstInstr

-- Extract the element from the last instruction in the block.
nodeTail :: Node a -> a
nodeTail node = let lastInstr = last $ nodeCode node
                in single lastInstr


showCfgs :: [(CFG a, Method a)] -> String
showCfgs cfgs = unlines $ map showCfg cfgs
  where
    showCfg (g, Mthd _ _ (QIdent _ (SymIdent i1) (SymIdent i2)) _ _) =
      "CFG for " ++ i1 ++ "." ++ i2 ++ ":\n" ++ show g

cfgsToMthds ::  a -> [(CFG a, Method b)] -> [Method a]
cfgsToMthds default_ = map (\(g, Mthd _ r i ps _) ->
    Mthd default_ (default_ <$ r) (default_ <$ i) (map (default_ <$) ps) (linearise g))

showCfgsWithLiveness :: [(CFG Liveness, Method a)] -> String
showCfgsWithLiveness cfgs = unlines $ map showCfg cfgs
  where
    showCfg (CFG g, Mthd _ _ (QIdent _ (SymIdent i1) (SymIdent i2)) _ _) =
      "CFG for " ++ i1 ++ "." ++ i2 ++ ":\n" ++ show (CFG g) ++ concatMap showLiveness (Map.elems g)
    showLiveness node =
        "Liveness at start of " ++ toStr (nodeLabel node) ++ ": " ++ show (nodeHead node) ++ "\n" ++
           "Liveness at end of " ++ toStr (nodeLabel node) ++ ": " ++ show (nodeTail node) ++ "\n"

showEspWithLiveness :: Metadata a -> [Method Liveness] -> String
showEspWithLiveness meta mthds = PrintEsp.printTreeWithInstrComments (Program emptyLiveness (emptyLiveness <$ meta) mthds)


genEspStep :: Metadata () -> [(CFG (), Method ())] -> String -> LattePipeline ()
genEspStep meta cfgs comment = do
    let esp = PrintEsp.printTree $ Program () meta (cfgsToMthds () cfgs)
    printLogInfoStr $ ">> GENERATED INTERMEDIATE STEP " ++ comment ++ "\n\n"
    printLogInfoStr $ showCfgs cfgs
    printLogInfoStr $ ">> END STEP " ++ comment ++ "\n\n"

genEspWithLivenessStep :: Metadata () -> [(CFG Liveness, Method ())] -> String -> LattePipeline ()
genEspWithLivenessStep meta cfgs comment = do
    let esp = showEspWithLiveness meta (cfgsToMthds emptyLiveness cfgs)
    printLogInfoStr $ ">> GENERATED INTERMEDIATE STEP " ++ comment ++ "\n\n"
    printLogInfoStr $ showCfgsWithLiveness cfgs
    printLogInfoStr $ ">> END STEP " ++ comment ++ "\n\n"

genEspWithLivenessAndIGsStep :: Metadata () -> [(SSA Liveness, Method (), InterferenceGraph)] -> String -> LattePipeline ()
genEspWithLivenessAndIGsStep meta cfgs comment = do
    let cfgs' = map (\(SSA c, m, _) -> (c, m)) cfgs
        esp = showEspWithLiveness meta (cfgsToMthds emptyLiveness cfgs')
        ig_ = map (\(_, _, g) -> g) cfgs
    printLogInfoStr $ ">> GENERATED INTERMEDIATE STEP " ++ comment ++ "\n\n"
    printLogInfoStr $ showCfgsWithLiveness cfgs'
    printLogInfoStr $ show ig_
    printLogInfoStr $ ">> END STEP " ++ comment ++ "\n\n"


compl_ :: (Program ()) -> LattePipeline String
compl_ (Program _ meta mthds) = do
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
    let optimisedCfgs = map (\(c, m, g) -> let(c', m') = removeUnreachable (inlineTrivialBlocks c) m in (c', m', g)) unfoldedPhi
    let finalCfgs = map (\(c, m, g) -> (removeDeadCode $ analyseLiveness c, m, g)) optimisedCfgs
    printLogInfoStr $ ">> GENERATED CFGS\n\n" ++ (showCfgs cfgs)
    genEspStep meta cfgsLin "Removing unreachable blocks..."
    genEspWithLivenessStep meta cfgsWithLiveness "Analysing liveness..."
    genEspStep meta (map (first unwrapSSA) ssaCode) "Transforming to SSA..."
    genEspStep meta (map (first unwrapSSA) optimisedCode) "Optimising Espresso..."
    genEspWithLivenessStep meta (map (first unwrapSSA) optimisedWithLiveness) "Reanalysing liveness..."
    genEspWithLivenessAndIGsStep meta allocatedCfgs "Allocating registers..."
    genEspStep meta (map (\(c, m, _) -> (c, m)) unfoldedPhi) "Unfolding phis..."
    genEspStep meta (map (\(c, m, _) -> (c, m)) optimisedCfgs) "Inlining trivial jumps..."
    genEspWithLivenessStep meta (map (\(c, m, _) -> (c, m)) finalCfgs) "Final liveness analysis..."
    return $ generate meta (map (\(c, m, g) -> (c, m, getRegisterAllocation c g)) finalCfgs)
