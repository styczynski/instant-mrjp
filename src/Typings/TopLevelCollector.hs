{-# LANGUAGE ImpredicativeTypes #-}
module Typings.TopLevelCollector where

import qualified Data.Map as M
import qualified Utils.Containers.IDMap as IM
import Reporting.Logs
import Program.Syntax
import qualified Data.Text as T
import qualified Data.Key as K
import qualified Typings.Types as Type
import Control.Monad.State
import Typings.Def
import Typings.Env
import Reporting.Errors.Def as Errors
import Reporting.Errors.Position

import Control.Monad.Trans.Class

import qualified Typings.InheritanceHierarchy as Hierarchy

groupByKey :: (Ord k) => (v -> k) -> [v] -> M.Map k [v]
groupByKey getkey
  = M.fromListWith (++) . fmap (\val -> (getkey val, [val]))

argsToMapping :: [Arg Position] -> TypeChecker (IM.Map (Type.Name, Type.Type))
argsToMapping = fmap IM._unsafeFrom . mapM (\(Arg pos t id@(Ident _ name)) -> assureProperType t >> return (id, t)) --assureProperType t >> return t

assureProperType :: Type Position -> TypeChecker ()
assureProperType t =
    case t of
        InfferedT pos -> todoImplementError "Inffered type instead of a proper type" --fail ("Inffered type instead of a proper type", pos)
        _ -> return ()

collectClasses :: [Definition Position] -> TypeChecker [Type.Class]
collectClasses ((FunctionDef _ _ _ _ _):xs) = collectClasses xs
collectClasses [] = return []
collectClasses ((def@(ClassDef pos id parent decls)):xs) = do
    rest <- collectClasses xs
    members <- mapM memberOf decls
    -- Corect all classes to extend Object
    parent' <- correctExtends parent
    return $ Type.Class id parent' members def : rest
    where
        correctExtends :: OptionalName Position -> TypeChecker (OptionalName Position)
        correctExtends n@(Name p i) = return n
        correctExtends (NoName p) = return $ Name p (Ident p "Object")
        memberOf :: ClassDecl Position -> TypeChecker Type.Member
        memberOf decl@(FieldDecl pos t id) = assureProperType t >> return (Type.Field id t decl)
        memberOf decl@(MethodDecl pos t id args _) = return . ($ decl) . Type.Method id t  =<< (argsToMapping args)

collectFunctions :: [Definition Position] -> TypeChecker [Type.Function]
collectFunctions ((ClassDef _ _ _ _):xs) = collectFunctions xs
collectFunctions [] = return []
collectFunctions ((decl@(FunctionDef pos t id args _)):xs) = do
    f <- return . ($ decl) . Type.Fun id t =<< (argsToMapping args)
    rest <- collectFunctions xs
    return $ f : rest

checkDuplicates :: (Type.TypeContext t) => (t -> [t] -> TypeChecker ()) -> M.Map String [t] -> TypeChecker (M.Map String t)
checkDuplicates onErr entries = K.mapWithKeyM (fn onErr) entries
    where
        fn :: (t -> [t] -> TypeChecker ()) -> String -> [t] -> TypeChecker t
        fn onErr name (h:rest) = do
            return $ printLogInfo $ "Class '" <> (T.pack name) <> "'"
            case rest of
                [] -> return h
                xs -> do
                    onErr h xs
                    return h

checkRedeclarationInClasses :: [Type.Class] -> TypeChecker ()
checkRedeclarationInClasses cls = do
    -- Member duplicates within a class
    let memberNames = map (\(Type.Class _ _ mems _) -> map (\m -> (Type.stringName m, m)) mems) cls
    --    dupsM = map duplicates memberNames
    --mapM_ throwOnDuplicates dupsM
    -- Type check of same name methods in parents
    let members = map (membersUpTree cls 0) cls
    mapM_ (checkInheritedMemberTypes cls) members
    where
        membersUpTree :: [Type.Class] -> Int -> Type.Class -> (Type.Class, [(String, Int, Type.Member)])
        membersUpTree cls i c@(Type.Class _ par mems _) =
            let localMemberNames = map (\m -> (Type.stringName m, i, m)) mems
            in case par of
                NoName _ -> (c, localMemberNames)
                Name _ parentClass -> (c, localMemberNames ++ snd (membersUpTree cls (i+1) (findClass cls parentClass)))
        findClass :: [Type.Class] -> Ident Position -> Type.Class
        findClass cls (Ident _ n) = head $ filter (\(Type.Class (Ident _ nn) _ _ _) -> n == nn) cls
        -- throwOnDuplicates :: [(String, Type.Member)] -> TypeChecker ()
        -- throwOnDuplicates ds =
        --     case ds of
        --         ((n,m):_) -> failure $ Errors.DuplicateMember cls m1 [m2]
        --         [] -> return ()
        checkInheritedMemberTypes :: [Type.Class] -> (Type.Class, [(String, Int, Type.Member)]) -> TypeChecker ()
        checkInheritedMemberTypes cls members = walk $ members
            where
                walk :: (Type.Class, [(String, Int, Type.Member)]) -> TypeChecker ()
                walk (c, ((a,_,m1@(Type.Field (Ident p _) _ _)):(b,_, m2@(Type.Field (Ident p2 _) _ _)):r)) | a == b = failure $ Errors.DuplicateMember c m1 [m2]
                walk (c, ((a,_, m1@(Type.Field (Ident p _) _ _)):(b,_, m2@(Type.Method _ _ _ _)):r)) | a == b = failure $ Errors.DuplicateMember c m1 [m2]
                walk (c, ((a,_, m1@(Type.Method (Ident p _) _ _ _)):(b,_, m2@(Type.Field _ _ _)):r)) | a == b = failure $ Errors.DuplicateMember c m1 [m2]
                --walk ((a,_, m1@(Type.Method (Ident p _) t1 t2 _)):bb@(b,_, m2@(Type.Method _ tt1 tt2 _)):r) | a == b = do
                    -- cond <- tcanBeCastUp cls (FunT Undefined t1 t2) (FunT Undefined tt1 tt2)
                    -- if cond then walk (bb:r)
                    -- else throwError ("Method "++a++" has an incompatible type with overriden method in a parent class", p)
                walk (c, (_:r)) = walk (c, r)
                walk (_, []) = return ()
                walk (_, _) = return ()

addBuiltinFunctions :: [Type.Function] -> TypeChecker [Type.Function]
addBuiltinFunctions fns = return $ builtIn ++ fns
    where
        args types = IM._unsafeFrom $ map (\(i, t) -> (Ident Undefined $ "_arg_" ++ show i, t)) $ zip [1..] types
        noargs = IM.empty
        void = VoidT BuiltIn
        bool = BoolT BuiltIn
        int = IntT BuiltIn
        byte = ByteT BuiltIn
        name = Ident BuiltIn
        string = StringT BuiltIn
        array = ArrayT BuiltIn
        object = class_ "Object"
        class_ s = ClassT BuiltIn (name s)
        builtinFunctionDecl :: Definition Position
        builtinFunctionDecl = FunctionDef BuiltIn (VoidT BuiltIn) (Ident BuiltIn "") [] (Block BuiltIn [])
        builtIn = [
                Type.Fun (name "printString") void (args [string]) builtinFunctionDecl,
                Type.Fun (name "printInt") void (args [int]) builtinFunctionDecl,
                Type.Fun (name "printBoolean") void (args [bool]) builtinFunctionDecl,
                Type.Fun (name "printBinArray") void (args [array byte]) builtinFunctionDecl,
                Type.Fun (name "byteToString") string (args [byte]) builtinFunctionDecl,
                Type.Fun (name "boolToString") string (args [bool]) builtinFunctionDecl,
                Type.Fun (name "intToString") string (args [int]) builtinFunctionDecl,
                Type.Fun (name "print") void (args [object]) builtinFunctionDecl,
                Type.Fun (name "error") void noargs builtinFunctionDecl,
                Type.Fun (name "readInt") int noargs builtinFunctionDecl,
                Type.Fun (name "readString") string noargs builtinFunctionDecl,
                Type.Fun (name "run_gc") void noargs builtinFunctionDecl
            ]

addBuiltinClasses :: [Type.Class] -> TypeChecker [Type.Class]
addBuiltinClasses cls = return $ builtIn ++ cls
    where
        args types = IM._unsafeFrom $ map (\(i, t) -> (Ident Undefined $ "_arg_" ++ show i, t)) $ zip [1..] types
        noargs = IM.empty
        void = VoidT BuiltIn
        bool = BoolT BuiltIn
        int = IntT BuiltIn
        byte = ByteT BuiltIn
        name = Ident BuiltIn
        string = StringT BuiltIn
        array = ArrayT BuiltIn
        object = class_ "Object"
        class_ s = ClassT BuiltIn (name s)
        builtinClassDecl :: Definition Position
        builtinClassDecl = ClassDef BuiltIn (Ident BuiltIn "") (NoName BuiltIn) []
        builtinMethodDecl :: ClassDecl Position
        builtinMethodDecl = MethodDecl BuiltIn (VoidT BuiltIn) (Ident BuiltIn "") [] (Block BuiltIn [])
        builtIn = [
                Type.Class (name "Object") (NoName BuiltIn) [
                    Type.Method (name "equals") bool (args [class_ "Object"]) builtinMethodDecl,
                    Type.Method (name "getHashCode") int noargs builtinMethodDecl,
                    Type.Method (name "toString") (class_ "String") noargs builtinMethodDecl
                ] builtinClassDecl,
                Type.Class (name "String") (justName $ name "Object") [
                    Type.Method (name "charAt") int (args [int]) builtinMethodDecl,
                    Type.Method (name "equals") bool (args [class_ "Object"]) builtinMethodDecl,
                    Type.Method (name "concat") (class_ "String") (args [class_ "String"]) builtinMethodDecl,
                    Type.Method (name "startsWith") bool (args [class_ "String"]) builtinMethodDecl,
                    Type.Method (name "endsWith") bool (args [class_ "String"]) builtinMethodDecl,
                    Type.Method (name "getBytes") (array byte) noargs builtinMethodDecl,
                    Type.Method (name "indexOf") int (args [class_ "String", int]) builtinMethodDecl,
                    Type.Method (name "length") int noargs builtinMethodDecl,
                    Type.Method (name "substring") (class_ "String") (args [int, int]) builtinMethodDecl,
                    Type.Method (name "toString") string noargs builtinMethodDecl,
                    Type.Method (name "getHashCode") int noargs builtinMethodDecl
                ] builtinClassDecl,
                Type.Class (name "Array") (justName $ name "Object") [
                    Type.Field (name "elements") object builtinMethodDecl,
                    Type.Field (name "length") int builtinMethodDecl,
                    Type.Field (name "elementSize") int builtinMethodDecl,
                    Type.Method (name "toString") string noargs builtinMethodDecl
                ] builtinClassDecl
            ]


collectDefinitions :: [Definition Position] -> TypeChecker ()
collectDefinitions defs = do
    cls <- checkDuplicates (\h l -> failure $ Errors.DuplicateClass h l) =<< return . groupByKey Type.stringName =<< addBuiltinClasses =<< collectClasses defs
    fns <- checkDuplicates (\h l -> failure $ Errors.DuplicateFun h l) =<< return . groupByKey Type.stringName =<< addBuiltinFunctions =<< collectFunctions defs
    -- cls <- return $ M.map (replaceEmptyParent "Object") cls
    hierarchy <- Hierarchy.constructInheritanceHierarchy cls
    Hierarchy.checkLoops hierarchy cls
    modify (setupDefEnv fns cls hierarchy)
    where
        replaceEmptyParent :: String -> Type.Class -> Type.Class
        replaceEmptyParent defaultParent (Type.Class name (NoName p) members def) = Type.Class name (Name p (Ident p defaultParent)) members def
        replaceEmptyParent _ cls = cls