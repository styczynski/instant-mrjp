{-# LANGUAGE ImpredicativeTypes #-}
module Typings.TopLevelCollector where

import qualified Data.Map as M
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

typeFromArg :: Arg Position -> TypeChecker (Type.Type)
typeFromArg (Arg pos t id) = assureProperType t >> return t

assureProperType :: Type Position -> TypeChecker ()
assureProperType t =
    case t of
        InfferedT pos -> failure $ Errors.UnknownFailure "Inffered type instead of a proper type" --fail ("Inffered type instead of a proper type", pos)
        _ -> return ()

collectClasses :: [Definition Position] -> TypeChecker [Type.Class]
collectClasses ((FunctionDef _ _ _ _ _):xs) = collectClasses xs
collectClasses [] = return []
collectClasses ((def@(ClassDef pos id parent decls)):xs) = do
    rest <- collectClasses xs
    members <- mapM memberOf decls
    return $ Type.Class id parent members def : rest
    where
        memberOf :: ClassDecl Position -> TypeChecker Type.Member
        memberOf decl@(FieldDecl pos t id) = assureProperType t >> return (Type.Field id t decl)
        memberOf decl@(MethodDecl pos t id args _) = return . ($ decl) . Type.Method id t  =<< (mapM typeFromArg args)

collectFunctions :: [Definition Position] -> TypeChecker [Type.Function]
collectFunctions ((ClassDef _ _ _ _):xs) = collectFunctions xs
collectFunctions [] = return []
collectFunctions ((decl@(FunctionDef pos t id args _)):xs) = do 
    f <- return . ($ decl) . Type.Fun id t =<< (mapM typeFromArg args)
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


collectDefinitions :: [Definition Position] -> TypeChecker ()
collectDefinitions defs = do
    cls <- checkDuplicates (\h l -> failure $ Errors.DuplicateClass h l) =<< return . groupByKey Type.stringName =<< collectClasses defs
    fns <- checkDuplicates (\h l -> failure $ Errors.DuplicateFun h l) =<< return . groupByKey Type.stringName =<< collectFunctions defs
    -- cls <- return $ M.map (replaceEmptyParent "Object") cls
    hierarchy <- Hierarchy.constructInheritanceHierarchy cls
    Hierarchy.checkLoops hierarchy cls
    modify (setupDefEnv fns cls hierarchy)
    where
        replaceEmptyParent :: String -> Type.Class -> Type.Class
        replaceEmptyParent defaultParent (Type.Class name (NoName p) members def) = Type.Class name (Name p (Ident p defaultParent)) members def
        replaceEmptyParent _ cls = cls