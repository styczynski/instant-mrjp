module Optimizer.Optimizer(optimize) where

import qualified Program.Syntax as Syntax
import qualified Optimizer.ConstPropagation as ConstPropagation
import Reporting.Logs

optimize :: Syntax.Program -> LattePipeline Syntax.Program
optimize prog = do
    ConstPropagation.foldConstants prog