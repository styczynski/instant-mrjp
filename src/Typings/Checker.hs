{-# LANGUAGE ImpredicativeTypes #-}
module Typings.Checker where

import Typings.Def
import Typings.Env
import qualified Typings.Types as Type
import qualified Reporting.Errors.Def as Errors
import qualified Typings.TopLevelCollector as Collector
import Program.Syntax
import Data.Maybe
import qualified Data.Map as M
import qualified Utils.Graphs as G
import qualified Utils.C3 as C3
import Control.Monad.Except(runExceptT)
import Control.Monad.State
import Reporting.Errors.Position
import Reporting.Logs

import Utils.Similarity
import Typings.TypeChecking
import Typings.InheritanceHierarchy
import Control.Monad.Extra

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
{-# INLINE concatMapM #-}
concatMapM op = foldr f (pure [])
    where f x xs = do x <- op x; if null x then xs else do xs <- xs; pure $ x++xs

checkInheritanceDuplicatedMembers :: TypeChecker ()
checkInheritanceDuplicatedMembers = do
    (Hierarchy graph _) <- classes
    errors <- listToMaybe <$> concatMapM (\className -> handleLinearizationErorrs (G.getNode graph className) $ (mapMaybeM (uncurry checkChain) . M.toList) <$> C3.linearizeNodeContent graph Type.classMembers Type.stringName className) (G.keys graph)
    maybe (return ()) (failure) errors
    where
        checkChain :: String -> [(Type.Class, Type.Member)] -> TypeChecker (Maybe Errors.Error)
        checkChain _ [_] = return Nothing
        checkChain _ [] = return Nothing
        checkChain _ ((c, m):others) = do
            incompat <- filterM (fmap not . compareDefs m . snd) others
            if null incompat then return Nothing else return $ Just $ Errors.DuplicateMembersInChain c m incompat

        compareDefs :: Type.Member -> Type.Member -> TypeChecker Bool
        compareDefs (Type.Method _ t1 t2 _) (Type.Method _ tt1 tt2 _) = canBeCastUp (FunT Undefined t1 t2) (FunT Undefined tt1 tt2)
        compareDefs _ _ = return $ False

        handleLinearizationErorrs :: Maybe Type.Class -> Either String (TypeChecker [Errors.Error]) -> TypeChecker [Errors.Error]
        handleLinearizationErorrs (Just cls) (Left err) = return [Errors.InheritanceLinearizationProblem cls err]
        handleLinearizationErorrs _ (Right v) = v


checkForMain :: TypeChecker ()
checkForMain = do
    env <- get
    case findFunction env "main" of
        Nothing -> failure $ Errors.NoMain env
        (Just fn@(Type.Fun _ retType _ _)) | (not $ similar retType (IntT Undefined)) -> failure $ Errors.InvalidMainReturn fn env
        (Just fn@(Type.Fun _ _ args _)) | length args > 0 -> failure $ Errors.MainHasArgs fn env
        (Just _) -> return ()

performTypeCheck :: Program Position -> TypeChecker (Program Position, [Type.Class])
performTypeCheck prog@(Program pos defs) = do
    Collector.collectDefinitions defs
    checkForMain
    checkInheritanceDuplicatedMembers
    Typings.TypeChecking.checkTypes prog
    return (prog, [])

checkTypes :: Program Position -> LattePipeline TypeCheckingResult
checkTypes prog@(Program pos defs) = do
    result <- return $ runExceptT (evalStateT (performTypeCheck prog) initialEnv)
    result

    -- let classes = addBuiltInTypes $ map changeEmptyParent classDefs
    -- checkCyclesInClasses classes
    -- checkRedeclarationInClasses classes
    -- funDefs <- getFunctions defs
    -- let functions = addBuiltInFunctions funDefs
    -- checkRedeclarationInFunctions functions
    -- checkForMain functions
    -- np <- runReaderT (checkP prog) (classes, functions, [])
