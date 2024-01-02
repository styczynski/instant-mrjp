module Linearized.Converter where

import qualified Program.Syntax as A
import qualified Linearized.Syntax as B

import Reporting.Errors.Position
import Linearized.Syntax (IRPosition(..))

posFrom :: (A.IsSyntax s Position) => (s Position) -> IRPosition
posFrom ast = let startPos = A.getPos ast in IRPosition 0 (startPos, startPos)

class  (A.IsSyntax ma Position) => IRAST ma mb where
    doTransform :: ma Position -> mb IRPosition
    over :: [ma Position] -> [mb IRPosition]
    over = map transform

    transform :: ma Position -> mb IRPosition
    transform ast = fmap (\_ -> posFrom ast) $ doTransform ast

instance IRAST A.Program B.Program where
    doTransform (A.Program a tds) = B.Program a (over tds)
