{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Linearized.Optimizer.CommonExpressions(run, initialState) where
import Data.List
import Control.Monad.State
import Control.Lens

import Control.Monad.Except hiding (void)
import Control.Monad.Reader hiding (void)
import Control.Monad.State hiding (void)

import qualified Linearized.Syntax as L
import qualified Linearized.Optimizer.Values as VP
import qualified Utils.Containers.IDMap as IM
import qualified Data.Map as M
import Linearized.Optimizer.Liveness
import Linearized.Def
import Control.DeepSeq
import GHC.Generics (Generic)

import Data.List

import Reporting.Logs
import qualified Reporting.Errors.Def as Errors

data ESEnv = ESEnv
  {
    _esVarsMap :: IM.Map (L.Name L.IRPosition, L.Expr L.IRPosition)
    , _esExprMap :: M.Map (L.Expr L.IRPosition) (L.Name L.IRPosition)
  } deriving (Show, Generic, NFData)

makeLensesWith abbreviatedFields ''ESEnv

type ExpressionSubsituter a = LinearConverter ESEnv a

initialState :: ESEnv
initialState = ESEnv {
    _esVarsMap = IM.empty
    , _esExprMap = M.empty
}

run :: L.Program L.IRPosition -> ExpressionSubsituter (L.Program L.IRPosition)
run (L.Program p sts funs strs) = do
    let dupErr = (idMapFailure "ExpressionSubsituter" Errors.ILNEDuplicateFunctionName)
    nfuncs <- IM.mapElemsM dupErr (\_ (L.Fun p l t args body) -> (\nbody -> return $ L.Fun p l t args nbody) =<< eliminateCommonSubexpressions body) funs
    return (L.Program p sts nfuncs strs)

eliminateCommonSubexpressions :: [L.Stmt L.IRPosition] -> ExpressionSubsituter [L.Stmt L.IRPosition]
eliminateCommonSubexpressions stmts = foldM (eliminateCommons) [] stmts

eliminateCommons :: [L.Stmt L.IRPosition] -> (L.Stmt L.IRPosition) -> ExpressionSubsituter [L.Stmt L.IRPosition]
eliminateCommons emitted s@(L.VarDecl p t n e) = do
    m <- findExistingExpr e
    case m of
        Nothing -> do
            addVar n e
            return $ emitted ++ [s]
        Just g -> do
            b <- checkValid e g emitted
            if b then return $ emitted ++ [L.VarDecl p t n (L.Val p (L.Var p g))]
            else do
                addVar n e
                return $ emitted ++ [s]
eliminateCommons emitted s@(L.Assign p t tg e) = do
    m <- findExistingExpr e
    case m of
        Nothing -> 
            case tg of
                L.Variable _ n -> do
                    addVar n e
                    return $ emitted ++ [s]
                _ -> return $ emitted ++ [s]
        Just g -> do
            b <- checkValid e g emitted
            if b then return $ emitted ++ [L.Assign p t tg (L.Val p (L.Var p g))]
            else case tg of
                    L.Variable _ n -> do
                        addVar n e
                        return $ emitted ++ [s]
                    _ -> return $ emitted ++ [s]
eliminateCommons emitted s = return $ emitted ++ [s]


findExistingExpr :: L.Expr L.IRPosition -> ExpressionSubsituter (Maybe (L.Name L.IRPosition))
findExistingExpr (L.Val _ (L.Const _ _)) = return Nothing
findExistingExpr (L.NewObj _ _) = return Nothing
findExistingExpr (L.NewArray _ _ _) = return Nothing
findExistingExpr (L.Call _ _ _) = return Nothing
findExistingExpr (L.MCall _ _ _ _) = return Nothing
findExistingExpr (L.MemberAccess _ _ _) = return Nothing
findExistingExpr (L.ArrAccess _ _ _) = return Nothing
findExistingExpr e = oStateGet ((M.lookup e) . (^. exprMap))

addVar :: (L.Name L.IRPosition) -> (L.Expr L.IRPosition) -> ExpressionSubsituter ()
addVar name@(L.Name _ n) expr = do --oStateSet (\env -> env & commonExprs %~ M.insert expr name) 
    currentVal <- oStateGet ((IM.lookup n) . (^. varsMap))
    case currentVal of
        Nothing -> do
            newVarsMap <- (IM.insertM (idMapFailure "ValuePropagator.addVar" Errors.ILNEDuplicateValueMapping) (name, expr)) =<< oStateGet (^. varsMap)
            oStateSet (\env -> env & exprMap %~ (M.insert expr name) & varsMap .~ newVarsMap)
        (Just _) -> do
            oStateSet (\env -> env & exprMap %~ (M.insert expr name))

isWLabel :: (L.Label L.IRPosition) -> Bool
isWLabel (L.Label _ name) = any ((flip isPrefixOf) name) ["_I", "_W"]
isWLabel _ = False

checkValid :: (L.Expr L.IRPosition) -> (L.Name L.IRPosition) -> [L.Stmt L.IRPosition] -> ExpressionSubsituter Bool
checkValid expr exprName emitted = do
    existingExpr <- getBoundExpr exprName
    if expr /= existingExpr then return False
    else
        checkEmittedLabels exprName emitted
    where
        checkEmittedLabels :: (L.Name L.IRPosition) -> [L.Stmt L.IRPosition] -> ExpressionSubsituter Bool
        checkEmittedLabels exprName ((L.SetLabel _ label):ss) = return $ isWLabel label
        checkEmittedLabels exprName (L.Assign _ t (L.Variable _ h) _:ss) = 
            if h == exprName then return True
            else checkEmittedLabels exprName ss
        checkEmittedLabels expr (L.VarDecl _ _ h _:ss) =
            if h == exprName  then return True
            else checkEmittedLabels exprName  ss
        checkEmittedLabels exprName (_:ss) = checkEmittedLabels exprName ss

getBoundExpr :: (L.Name L.IRPosition) -> ExpressionSubsituter (L.Expr L.IRPosition)
getBoundExpr (L.Name _ name) = --oStateGet ((M.lookup e) . (^. exprMap))
    return . snd =<< (IM.findM (idMapFailure "ValuePropagator.getBoundExpr" Errors.ILNEMissingValueMapping) name) =<< oStateGet (^. varsMap)