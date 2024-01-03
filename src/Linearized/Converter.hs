module Linearized.Converter where

import qualified Program.Syntax as A
import qualified Linearized.Syntax as B

import qualified Data.Map as M
import Data.List

import Control.Lens hiding(transform, over)

import Reporting.Errors.Position
import Reporting.Logs
import Linearized.Syntax (IRPosition(..))
import Linearized.Env
import Linearized.Def

import Control.Monad

class  (A.IsSyntax ma Position, B.IsIR mb) => IRAST ma mb where
    doTransform :: ma Position -> LinearConverter (mb Position)
    over :: [ma Position] -> LinearConverter [mb Position]
    over = mapM transform

    transform :: ma Position -> LinearConverter (mb Position)
    transform ast = doTransform ast
    -- return . B.modifyPos (\_ -> posFrom ast)  =<< 

data FnProto = FnProto Position (B.Label Position) (B.Type Position) [A.Arg Position] [A.Stmt Position]

newNameFor :: (A.Ident Position) -> (B.Type Position) -> LinearConverter (B.Name Position)
newNameFor (A.Ident pos n) t = do
    (B.Name _ n') <- newName t
    lcStateSet (\env -> env & varMap %~ M.insert n n')
    return $ B.Name pos n'

newName :: (B.Type Position) -> LinearConverter (B.Name Position)
newName t = do
    i <- lcStateGet (^. varNameCounter)
    let n = "t_"++show i
    lcStateSet (\env -> env & varNameCounter %~ (+1) & varType %~ M.insert n t)
    return $ B.Name (B.getPos t) n

newLabel :: String -> LinearConverter (B.Label Position)
newLabel prefix = do
    i <- lcStateGet (^. varNameCounter)
    lcStateSet (\env -> env & varNameCounter %~ (+1))
    return (B.Label Undefined $ prefix++show i)


ct :: (Show a) => A.Type a -> B.Type a
ct (A.BoolT p) = B.ByteT p
ct (A.ByteT p) = B.ByteT p
ct (A.VoidT p) = B.ByteT p
ct (A.IntT p) = B.IntT p
ct ast = B.Reference $ A.getPos ast

-- collectFn (A.FunctionDef _ t (A.Ident _ name) args block) = do
--             if elem name (map (\(B.Fun l _ _ _) ->l) funcs) then
--                 return []--transF name args block
--             else return [toEntity $ B.Fun name (ct t) [] []]
collectFunctions :: [A.Definition Position] -> LinearConverter [B.Function Position]
collectFunctions defs = do
    let fns = concatMap collectFromDef defs
    let fnPrototypes = M.fromList $ (map (\fn@(B.Fun _ (B.Label _ id) _ _ _) -> (id, fn)) builtIns) ++ map (\(FnProto pos name@(B.Label _ id) retType _ _) -> (id, B.Fun pos name retType [] [])) fns
    lcStateSet (\env -> env & functions .~ fnPrototypes)
    mapM_ (transformFunction >=> setFunction) fns
    lcStateGet (\env -> M.elems (env ^. functions) \\ builtIns)
    where
        collectFromDef :: A.Definition Position -> [FnProto]
        collectFromDef (A.FunctionDef pos t (A.Ident idpos name) args (A.Block _ stmts)) = [FnProto pos (B.Label idpos name) (ct t) args stmts]
        collectFromDef (A.ClassDef _ (A.Ident _ clname) _ mems) = concatMap (collectFromClassDecl clname) mems
        collectFromClassDecl :: String -> A.ClassDecl Position -> [FnProto]
        collectFromClassDecl _ (A.FieldDecl _ _ _) = []
        collectFromClassDecl clname (A.MethodDecl pos t (A.Ident idpos n) args  (A.Block _ stmts)) =
            [FnProto pos (B.Label idpos ("_"++clname++"_"++n)) (ct t) (A.Arg Undefined (A.ClassT Undefined (A.Ident Undefined clname)) (A.Ident Undefined "this") : args) stmts]
        builtIns =
            [
                B.Fun BuiltIn (B.Label BuiltIn "_Array_toString") (B.Reference BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_Object_toString") (B.Reference BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_Object_getHashCode") (B.IntT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_Object_equals") (B.ByteT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_String_equals") (B.ByteT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_String_getHashCode") (B.IntT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_String_toString") (B.Reference BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_String_substring") (B.Reference BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_String_length") (B.IntT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_String_indexOf") (B.IntT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_String_getBytes") (B.Reference BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_String_endsWith") (B.ByteT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_String_startsWith") (B.ByteT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_String_concat") (B.Reference BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "_String_charAt") (B.IntT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "printString") (B.ByteT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "printInt") (B.ByteT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "printByte") (B.ByteT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "printBoolean") (B.ByteT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "printBinArray") (B.ByteT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "byteToString") (B.Reference BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "boolToString") (B.Reference BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "intToString") (B.Reference BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "print") (B.ByteT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "error") (B.ByteT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "readInt") (B.IntT BuiltIn) [] [],
                B.Fun BuiltIn (B.Label BuiltIn "readString") (B.Reference BuiltIn) [] []
            ]
transformFunction :: FnProto -> LinearConverter (B.Function Position)
transformFunction (FnProto pos name@(B.Label _ id) retType args stmts) = do
    nstmts <- over stmts
    nargs <- mapM (\(A.Arg _ t n) -> newNameFor n (ct t) <&> (,) (ct t)) args
    return $ B.Fun pos name retType nargs nstmts

instance IRAST A.Program B.Program where
    doTransform (A.Program a tds) = do
        fns <- collectFunctions tds
        return $ B.Program a [] fns []

instance IRAST A.Stmt B.Stmt where
    doTransform ast = return $ B.Return $ A.getPos ast