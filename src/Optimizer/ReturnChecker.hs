{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Optimizer.ReturnChecker(
    run
    , initialState
) where

import Control.Lens

import qualified Typings.Env as TypeChecker

import qualified Reporting.Errors.Def as Errors
import Reporting.Errors.Position
import Utils.Similarity
import Reporting.Logs
import qualified Program.Syntax as Syntax
import Control.Monad.Except hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.State hiding (mapM)

import Optimizer.Def
import Optimizer.Env
import Data.Either

data ReturnCheckerEnv = ReturnCheckerEnv {
    _rcClassName :: Maybe String
    , _rcFunctionName :: Maybe String
}

makeLensesWith abbreviatedFields ''ReturnCheckerEnv

initialState :: ReturnCheckerEnv
initialState = ReturnCheckerEnv {
    _rcClassName = Nothing
    , _rcFunctionName = Nothing
}

withFunctionContext :: (Syntax.Ident Position) -> Optimizer ReturnCheckerEnv a -> Optimizer ReturnCheckerEnv a
withFunctionContext (Syntax.Ident _ fnName) m = do
    withOState (\env -> env & functionName .~ Nothing) . return =<< withOState (\env -> env & functionName .~ Just fnName) m

withClassContext :: (Syntax.Ident Position) -> Optimizer ReturnCheckerEnv a -> Optimizer ReturnCheckerEnv a
withClassContext (Syntax.Ident _ clsName) m = do
    withOState (\env -> env & className .~ Nothing) . return =<< withOState (\env -> env & className .~ Just clsName) m

createMissingReturnError :: (Syntax.Stmt Position) -> Optimizer ReturnCheckerEnv a
createMissingReturnError stmt = do
    r <- oStateGet (\env -> (env^.className, env^.functionName))
    tcState <- tcState
    case r of
        (Nothing, (Just fnName)) -> do
            fn <- maybe (failure (\(tcEnv, oEnv) -> Errors.InternalOptimizerFailure tcEnv oEnv "createMissingReturnError" $ Errors.IOPTECannotFindFunctionOrClass fnName)) return $ TypeChecker.findFunction tcState fnName
            failure (\(tcEnv, oEnv) -> Errors.FunctionLacksReturn tcEnv oEnv fn stmt)
        ((Just clsName), (Just methodName)) -> do
            (cls, member) <- either (\_ -> failure (\(tcEnv, oEnv) -> Errors.InternalOptimizerFailure tcEnv oEnv "createMissingReturnError" $ Errors.IOPTECannotFindFunctionOrClass methodName)) (maybe (failure (\(tcEnv, oEnv) -> Errors.InternalOptimizerFailure tcEnv oEnv "createMissingReturnError" $ Errors.IOPTECannotFindFunctionOrClass methodName)) return) $ TypeChecker.findMemberOf tcState False clsName methodName
            failure (\(tcEnv, oEnv) -> Errors.MethodLacksReturn tcEnv oEnv cls member stmt)

run :: Syntax.Program Position -> Optimizer ReturnCheckerEnv (Syntax.Program Position)
run prog = do
    verifyReturn prog

class (Syntax.IsSyntax a Position) => ASTVerifiable a where
    doVerifyReturn :: a Position -> Optimizer ReturnCheckerEnv (a Position)

    verifyReturn :: a Position -> Optimizer ReturnCheckerEnv (a Position)
    verifyReturn ast = do
        withStateT (optimizerQuit $ Syntax.getPos ast) . return =<< withStateT (optimizerEnter $ Syntax.getPos ast) (doVerifyReturn ast)

instance ASTVerifiable Syntax.Program where
    doVerifyReturn (Syntax.Program a defs) = do
        ndefs <- mapM verifyReturn defs
        return $ Syntax.Program a ndefs

instance ASTVerifiable Syntax.Definition where
    doVerifyReturn (Syntax.FunctionDef a (Syntax.VoidT tp) n as b) = do
        let rb = removePastReturn b
        nb <- withFunctionContext n $ checkVoidBlock rb
        return $ Syntax.FunctionDef a (Syntax.VoidT tp) n as nb
    doVerifyReturn (Syntax.FunctionDef a t n as b) = do
        let rb = removePastReturn b
        withFunctionContext n $ verifyReturn b
        return $ Syntax.FunctionDef a t n as rb
    doVerifyReturn (Syntax.ClassDef a c p decls) = do
        ndecls <- withClassContext c $ mapM verifyReturn decls
        return (Syntax.ClassDef a c p ndecls)

instance ASTVerifiable Syntax.ClassDecl where
    doVerifyReturn (Syntax.MethodDecl a (Syntax.VoidT tp) n as b) = do
        let rb = removePastReturn b
        nb <- withFunctionContext n $ checkVoidBlock rb
        return (Syntax.MethodDecl a (Syntax.VoidT tp) n as nb)
    doVerifyReturn (Syntax.MethodDecl a t n as b) = do
        let rb = removePastReturn b
        withFunctionContext n $ verifyReturn rb
        return (Syntax.MethodDecl a t n as rb)
    doVerifyReturn d = return d

instance ASTVerifiable Syntax.Block where
    doVerifyReturn (Syntax.Block pos stmts) = do
        checks <- return $ map checkS stmts
        -- FunctionLacksReturn
        if not $ any isRight checks then createMissingReturnError (last $ fst $ partitionEithers checks) else return (Syntax.Block pos stmts)
        where
            checkB :: Syntax.Block Position -> Either (Syntax.Stmt Position) (Syntax.Stmt Position)
            checkB (Syntax.Block _ stmts) = let checks = map checkS stmts in if any isRight checks then Right (head $ snd $ partitionEithers checks) else last checks
            checkS :: Syntax.Stmt Position -> Either (Syntax.Stmt Position) (Syntax.Stmt Position)
            checkS stmt@(Syntax.ReturnValue _ _) = Right stmt
            checkS (Syntax.IfElse _ (Syntax.Lit _ (Syntax.Bool _ True)) s1 _) = checkS s1
            checkS (Syntax.IfElse _ (Syntax.Lit _ (Syntax.Bool _ False)) _ s2) = checkS s2
            checkS (Syntax.IfElse _ _ s1 s2) = checkS s1 >> checkS s2
            checkS stmt@(Syntax.While _ (Syntax.Lit _ (Syntax.Bool _ True)) _) = Right stmt
            checkS (Syntax.While _ _ s) = checkS s
            checkS (Syntax.BlockStmt _ b) = checkB b
            checkS stmt = Left stmt

checkVoidBlock b@(Syntax.Block a stmts) =
    if stmts == [] then return (Syntax.Block a [Syntax.ReturnVoid BuiltIn])
    else 
        case last stmts of
            Syntax.ReturnVoid _ -> return b
            _ -> return (Syntax.Block a (stmts ++ [Syntax.ReturnVoid BuiltIn]))

removePastReturn (Syntax.Block a stmts) = (Syntax.Block a (firstUntilReturn stmts))
    where
        firstUntilReturn (h:t) = case h of
                            Syntax.ReturnVoid _ -> [h]
                            Syntax.ReturnValue _ _ -> [h]
                            Syntax.BlockStmt a b -> Syntax.BlockStmt a (removePastReturn b) : firstUntilReturn t
                            _ -> h : firstUntilReturn t
        firstUntilReturn [] = []