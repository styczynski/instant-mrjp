module Linearized.Optimizer.ValuePropagator(run, initialState) where
import Data.List
import Control.Monad.State

import Control.Monad.Except hiding (void)
import Control.Monad.Reader hiding (void)
import Control.Monad.State hiding (void)

import qualified Linearized.Syntax as L
import qualified Linearized.Optimizer.Values as VP
import qualified Utils.Containers.IDMap as IM
import Linearized.Optimizer.Liveness
import Linearized.Def

import Data.List

import Reporting.Logs
import qualified Reporting.Errors.Def as Errors

type VPEnv = (L.Name L.IRPosition, L.Value L.IRPosition)]
type ValuePropagator a = LinearConverter VPEnv a

initialState :: VPEnv
initialState = []

run :: L.Program L.IRPosition -> RCEnnricher (L.Program L.IRPosition)
run (L.Program p sts funs strs) = do
    let dupErr = (idMapFailure "ValuePropagator" Errors.ILNEDuplicateFunctionName)
    nfuncs <- IM.mapElemsM dupErr (\_ (L.Fun p l t args body) -> (\nbody -> L.Fun p l t args nbody) <$> propagateValues (analize body) args) funs
    return (L.Program p sts nfuncs strs)
