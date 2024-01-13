module Linearized.Linearizer where

import Control.Monad.Except(runExceptT)
import Control.Monad.State

import qualified Program.Syntax as A
import qualified Linearized.Syntax as B
import qualified IR.Syntax.Syntax as C

import Reporting.Errors.Position
import qualified Reporting.Errors.Def as Errors
import Reporting.Logs
import Linearized.Syntax (IRPosition(..))
import Linearized.Env
import Linearized.Def
import qualified Linearized.Converter as Converter
import qualified Optimizer.Env as Optimizer
import qualified Data.Text as T
import Utils.Time

--import qualified Linearized.Optimizer.ReferenceCounters as ORefCounters
import qualified Linearized.Optimizer.ValuePropagator as OValuePropagator
import qualified Linearized.Optimizer.CommonExpressions as OExpressionSubstituter
import qualified Linearized.IRConverter as OFIRConverter

type LinearizerResult = Either Errors.Error (LinearTranslatorEnv (), C.Program IRPosition)

posFrom :: Position -> IRPosition
posFrom pos = IRPosition 0 (pos, pos)

runLinearizer :: (A.Program Position) -> LinearConverter () (C.Program IRPosition)
runLinearizer prog = do
    rawIR <- Converter.transformProgram prog
    ir <- return $ fmap (posFrom) rawIR
    currentTime <- liftPipelineOpt nanos
    (_, optimizedIR) <- findFixedPoint [(runInternal "Value propagator" OValuePropagator.run OValuePropagator.initialState), (runInternal "Expression substituter" OExpressionSubstituter.run OExpressionSubstituter.initialState)] (0, currentTime) ir
    --ir <- runInternal "Value propagator" OValuePropagator.run OValuePropagator.initialState ir
    --ir <- runInternal "Expression substituter" OExpressionSubstituter.run OExpressionSubstituter.initialState ir
    --ir <- runInternal "Value propagator" OValuePropagator.run OValuePropagator.initialState ir
    --ir <- runInternal "Expression substituter" OExpressionSubstituter.run OExpressionSubstituter.initialState ir
    --irWithCounters <- runInternal "Reference counters embedding" ORefCounters.run ORefCounters.initialState optimizedIR 
    irWithCounters <- return optimizedIR
    fir <- runInternal "Convert to FIR" OFIRConverter.run OFIRConverter.initialState irWithCounters
    liftPipelineOpt $ printLogInfo $ T.pack $ "Linearizer terminated"
    return fir
    where
        findFixedPoint :: [B.Program IRPosition -> LinearConverter () (B.Program IRPosition)] -> (Int, Int) -> (B.Program IRPosition) -> LinearConverter () (Int, B.Program IRPosition)
        findFixedPoint fns (callNo, startTime) prog = do
            prevState <- oStateGet id
            newProg <- foldM (\oldProg fn -> fn oldProg) prog fns
            oStateSet (\_ -> prevState)
            currentTime <- liftPipelineOpt nanos
            timeElapsedMs <- return $ div (currentTime - startTime) 1000000
            liftPipelineOpt $ printLogInfoStr $ "Optimizing IR round " ++ (show $ callNo+1) ++ " (took " ++ (show timeElapsedMs) ++ " ms)" 
            if newProg /= prog && timeElapsedMs <= 4000 then do
                findFixedPoint fns (callNo+1, startTime) newProg
            else return (callNo, newProg)

linearizeToIR ::  Optimizer.OptimizerEnv () -> A.Program Position -> LattePipeline LinearizerResult
linearizeToIR oEnv prog@(A.Program pos defs) = do
    either (return . Left) (\((ir), env) -> return $ Right (env, ir)) =<< runExceptT (runStateT (runLinearizer prog) $ createInitialEnv oEnv)
    --rintLogInfo $ T.pack $ "Linearizer terminated !!!"
    --liftIO $ evaluate $ force k
