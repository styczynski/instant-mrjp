module Linearized.Converter where

import qualified Program.Syntax as A
import qualified Linearized.Syntax as B

import Reporting.Errors.Position
import Reporting.Logs
import Linearized.Syntax (IRPosition(..))
import Linearized.Env
import Linearized.Def

class  (A.IsSyntax ma Position, B.IsIR mb) => IRAST ma mb where
    doTransform :: ma Position -> LinearConverter (mb Position)
    over :: [ma Position] -> LinearConverter [mb Position]
    over = mapM transform

    transform :: ma Position -> LinearConverter (mb Position)
    transform ast = doTransform ast
    -- return . B.modifyPos (\_ -> posFrom ast)  =<< 

instance IRAST A.Program B.Program where
    doTransform (A.Program a tds) = return $ B.Program a [] [] []

