-- Unfolding of trivial phi functions done at the end of SSA transformation.
-- Final elimination of phi functions depends on register allocation, and thus
-- is located in X86_64.Phi.
module IR.Flow.Phi (removeTrivialPhis, unfoldTrivialPhi) where

import Control.Lens hiding (Const)

import           Data.Maybe
import           IR.Flow.CFG
import           IR.Syntax.Syntax

removeTrivialPhis :: CFG a d -> CFG a d
removeTrivialPhis = linearMap (\n -> n & nodeBody %~ (mapMaybe unfoldTrivialPhi))

isVal :: Val a -> IRValueName -> Bool
isVal (VVal _ _ vi) vi' = vi == vi'
isVal _ _               = False

unfoldTrivialPhi :: Instr a -> Maybe (Instr a)
unfoldTrivialPhi instr = case instr of
    IPhi _ _ []               -> Nothing
    IPhi _ i phiVars | all (\(PhiVar _ _ val) -> isVal val i) phiVars -> Nothing
    IPhi pos i (PhiVar _ _ val:phiVars)
        | all (\(PhiVar _ _ val') -> (fmap (const ()) val) == (fmap (const ()) val')) phiVars -> Just $ ISet pos i val
    IPhi pos i [PhiVar _ _ val] -> Just $ ISet pos i val
    IPhi pos i vars -> Just $ IPhi pos i (filter (\(PhiVar _ _ val) -> not $ isVal val i) vars)
    _                         -> Just instr
