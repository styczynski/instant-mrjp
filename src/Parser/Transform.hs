module Parser.Transform where

-- This module converts the AST from bnfc
-- to my custom AST

import Data.Char

import Reporting.Logs
import Parser.Types
import qualified Parser.Gen.AbsLatte as A
import qualified Program.Syntax as B
import qualified Reporting.Errors.Position as P

import Control.Monad.State hiding (void)

class RawAST ma mb where
    transform :: ma a -> mb a
    over :: [ma a] -> [mb a]
    over = map transform

instance RawAST A.Program' B.Program where
    transform (A.Program a tds) = B.Program a (over tds)

instance RawAST A.TopDef' B.Definition where
    transform (A.FnDef a t mid args b) = B.FunctionDef a (transform t) (transform mid) (over args) (transform b)
    transform (A.ClDef a mid cle mems) = B.ClassDef a (transform mid) (transform cle) (over mems)

instance RawAST A.ClMember' B.ClassDecl where
    transform (A.Field a t mid) = B.FieldDecl a (transform t) (transform mid)
    transform (A.Method a t mid args b) = B.MethodDecl a (transform t) (transform mid) (over args) (transform b)

instance RawAST A.Arg' B.Arg where
    transform (A.Arg a t mid) = B.Arg a (transform t) (transform mid)

instance RawAST A.Type' B.Type where
    transform (A.Var a) = B.InfferedT a
    transform (A.Void a) = B.VoidT a
    transform (A.Array a t) = B.ArrayT a (transform t)
    transform (A.Class a mid) = transform mid

instance RawAST A.Stmt' B.Stmt where
    transform (A.Empty a) = B.Empty a
    transform (A.BStmt a b) = B.BlockStmt a (transform b)
    transform (A.Decl a t is) = B.VarDecl a (map (\i -> (transform t, transform i)) is)
    transform (A.Ass a e1 e2) = B.Assignment a (transform e1) (transform e2)
    transform (A.Incr a e) = B.Assignment a (transform e) (B.BinaryOp a (B.Add a) (transform e) (B.Lit a (B.Int a 1)))
    transform (A.Decr a e) = B.Assignment a (transform e) (B.BinaryOp a (B.Sub a) (transform e) (B.Lit a (B.Int a 1)))
    transform (A.Ret a e) = B.ReturnValue a (transform e)
    transform (A.VRet a) = B.ReturnVoid a
    transform (A.Cond a e s1 ) = B.IfElse a (transform e) (transform s1) (B.Empty a)
    transform (A.CondElse a e s1 s2) = B.IfElse a (transform e) (transform s1) (transform s2)
    transform (A.While a e s) = B.While a (transform e) (transform s)
    transform (A.SExp a e) = B.ExprStmt a (transform e)
    transform (A.For a t mid e s) = B.BlockStmt a (B.Block a [
        B.VarDecl a [
            (B.IntT a, B.Init a (withPrefix "n__" mid) (B.Lit a (B.Int a 0))), (B.InfferedT a, B.Init a (withPrefix "a__" mid) (transform e))
            ],
        B.While a (B.BinaryOp a (B.Lt a)
                    (B.Var a (withPrefix "n__" mid))
                    (B.Member a (B.Var a (withPrefix "a__" mid)) (B.Ident a "length") Nothing))
                (B.BlockStmt a (B.Block a [
                    B.VarDecl a [(transform t, B.Init a (transform mid) (B.ArrAccess a (B.Var a (withPrefix "a__" mid)) (B.Var a (withPrefix "n__" mid)) Nothing))],
                    transform s,
                    B.Assignment a (B.Var a (withPrefix "n__" mid)) (B.BinaryOp a (B.Add a) (B.Var a (withPrefix "n__" mid)) (B.Lit a (B.Int a 1)))]
                    ))])

instance RawAST A.Expr' B.Expr where
    transform (A.ECast a mid e) = B.Cast a (transform mid) (transform e)
    transform (A.EVar a mid) = B.Var a (transform mid)
    transform (A.ELitInt a i) = B.Lit a (B.Int a i)
    transform (A.ELitTrue a) = B.Lit a (B.Bool a True)
    transform (A.ELitFalse a) = B.Lit a (B.Bool a False)
    transform (A.ELitNull a) = B.Lit a (B.Null a)
    transform (A.EApp a e es) = B.App a (transform e) (over es)
    transform (A.EMember a e mid) = B.Member a (transform e) (transform mid) Nothing
    transform (A.ENew a t) = 
        let nt = transform t in
        case nt of
            B.ArrayT a2 tt -> B.NewObj a tt (Just (B.Lit a2 (B.Int a2 0)))
            _ -> B.NewObj a nt Nothing
    transform (A.ENewArray a t e) = B.NewObj a (transform t) (Just $ transform e)
    transform (A.EArr a e1 e2) = B.ArrAccess a (transform e1) (transform e2) Nothing
    transform (A.EString a str) = B.Lit a (B.String a (delim str))
        where
            delim ('\\':'n':xs) = '\n' : delim xs
            delim ('\\':'t':xs) = '\t' : delim xs
            delim ('\\':'\"':xs) = '\"' : delim xs
            delim ('\\':'\'':xs) = '\'' : delim xs
            delim ('\\':'\\':xs) = '\\' : delim xs
            delim ('\\':xs) = let (num, rest) = readNum xs in chr num : delim rest
                where
                    readNum ss = 
                        let s = takeF isDigit ss []
                            i = read s :: Int
                            l = length s
                            ns = drop l ss
                        in case ns of
                            ('\\':'&':r) -> (i,r)
                            _ -> (i,ns)
                    takeF f (s:ss) acc | f s = takeF f ss (s:acc)
                    takeF _ _ acc = reverse acc

            delim (x:xs) = x : delim xs
            delim [] = []
    transform (A.Neg a e) = B.UnaryOp a (B.Neg a) (transform e)
    transform (A.Not a e) = B.UnaryOp a (B.Not a) (transform e)
    transform (A.EAdd a e1 (A.Plus a2) e2) = B.BinaryOp a (B.Add a2) (transform e1) (transform e2)
    transform (A.EAdd a e1 (A.Minus a2) e2) = B.BinaryOp a (B.Add a2) (transform e1) (B.UnaryOp a (B.Neg a) (transform e2))
    transform (A.EMul a e1 (A.Times a2) e2) = B.BinaryOp a (B.Mul a2) (transform e1) (transform e2)
    transform (A.EMul a e1 (A.Div a2) e2) = B.BinaryOp a (B.Div a2) (transform e1) (transform e2)
    transform (A.EMul a e1 (A.Mod a2) e2) = B.BinaryOp a (B.Mod a2) (transform e1) (transform e2)
    transform (A.EAnd a e1 e2) = B.BinaryOp a (B.And a) (transform e1) (transform e2)
    transform (A.EOr a e1 e2) = B.BinaryOp a (B.Or a) (transform e1) (transform e2)
    transform (A.ERel a e1 cmp e2) = B.BinaryOp a (transform cmp) (transform e1) (transform e2)


-- transformAST :: A.Program' a -> B.Program a
-- transformAST (A.Program a tds) = B.Program a (map transformD tds)

instance RawAST A.MIdent' B.Ident where
    transform (A.MIdent a (A.Ident s)) = B.Ident a s

withPrefix :: String -> A.MIdent' a -> B.Ident a
withPrefix pref id = let (B.Ident a s) = transform id in B.Ident a (pref++s)

instance RawAST A.ClassExt' B.OptionalName where
    transform (A.EmptyExt a) = B.NoName a
    transform (A.Ext a mid) = B.Name a $ transform mid

-- transformD (A.FnDef a t mid args b) = B.FunctionDef a (transform t) (transform mid) (map desugarArg args) (transform b)
-- transformD (A.ClDef a mid cle mems) = B.ClassDef a (transform mid) (transform cle) (map desugarMem mems)

-- desugarMem (A.Field a t mid) = B.FieldDecl a (transform t) (transform mid)
-- desugarMem (A.Method a t mid args b) = B.MethodDecl a (transform t) (transform mid) (map desugarArg args) (transform b)

-- desugarArg (A.Arg a t mid) = B.Arg a (transform t) (transform mid)

instance RawAST A.Block' B.Block where
    transform (A.Block a stmts) = B.Block a (over stmts)

instance RawAST A.Item' B.DeclItem where
    transform (A.NoInit a mid) = B.NoInit a (transform mid)
    transform (A.Init a mid e) = B.Init a (transform mid) (transform e)

-- transform (A.Var a) = B.InfferedT a
-- transform (A.Void a) = B.VoidT a
-- transform (A.Array a t) = B.ArrayT a (transform t)
-- transform (A.Class a mid) = transform mid

instance RawAST A.RelOp' B.BinOp where
    transform (A.LTH a) = B.Lt a
    transform (A.GTH a) = B.Gt a
    transform (A.LE a) = B.Le a
    transform (A.GE a) = B.Ge a
    transform (A.EQU a) = B.Equ a
    transform (A.NE a) = B.Neq a

instance RawAST A.MIdent' B.Type where
    transform (A.MIdent a (A.Ident s)) =
        case s of
            "int" -> B.IntT a
            "var" -> B.InfferedT a
            "void" -> B.VoidT a
            "byte" -> B.ByteT a
            "string" -> B.StringT a
            "boolean" -> B.BoolT a
            _ -> B.ClassT a (B.Ident a s)

type ASTTransformerState = Int
type ASTTransformer a = (StateT ASTTransformerState LattePipeline) a

_transformAST :: String -> RawProgram -> ASTTransformer (B.Program P.Position)
_transformAST file (RawProgram prog _) =
    let ast = (transform prog) :: (B.Program A.BNFC'Position) in
    mapM (postTransform file) ast
    --return $ fmap ((\(Just (l,c)) -> P.Position (P.TokenUID 0) file l c)) ast
    where
        postTransform :: String -> A.BNFC'Position -> ASTTransformer P.Position
        postTransform file (Just (l, c)) = do
            modify (+1)
            uid <- get
            return $ P.Position (P.TokenUID uid) file l c
        postTransform _ _ = return $ P.Undefined

transformAST :: String -> RawProgram -> LattePipeline (B.Program P.Position)
transformAST file prog =
    evalStateT (_transformAST file prog) 0