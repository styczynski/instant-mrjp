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

unfoldTrivialPhi :: Instr a -> Maybe (Instr a)
unfoldTrivialPhi instr = case instr of
    IPhi _ _ []               -> Nothing
    -- All variants are the same as destination (empty assignments).
    IPhi _ i phiVars | all (\(PhiVar _ _ val) -> isVal val i) phiVars -> Nothing
    -- All variants are the same (simple set).
    IPhi pos i (PhiVar _ _ val:phiVars)
        | all (\(PhiVar _ _ val') -> (fmap (const ()) val) == (fmap (const ()) val')) phiVars -> Just $ ISet pos i val
    -- Only one variant (simple set).
    IPhi pos i [PhiVar _ _ val] -> Just $ ISet pos i val
    -- Remove all empty assignment variants.
    IPhi pos i vars -> Just $ IPhi pos i (filter (\(PhiVar _ _ val) -> not $ isVal val i) vars)
    _                         -> Just instr

isVal :: Val a -> ValIdent -> Bool
isVal (VVal _ _ vi) vi' = vi == vi'
isVal _ _               = False
