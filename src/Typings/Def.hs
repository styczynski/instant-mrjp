{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Typings.Def where

import Control.Lens

import qualified Reporting.Errors.Def as Errors
import qualified Program.Syntax as Syntax
import qualified Typings.Types as Type
import qualified Data.FuzzySet.Simple as Fuzz
import Reporting.Errors.Position
import Reporting.Logs
import Typings.Env
import Control.Monad.Except hiding (void)
import Control.Monad.Reader hiding (void)
import Control.Monad.State hiding (void)
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE

import Control.Monad.Trans.Class


type TypeChecker a = (StateT TypeCheckerEnv (ExceptT Errors.Error LattePipeline)) a

type TypeCheckingResult = Either Errors.Error (TypeCheckerEnv, Syntax.Program Position, [Type.Class])

liftPipelineTC :: LattePipeline a -> TypeChecker a
liftPipelineTC = lift . lift

failure :: Errors.Error -> TypeChecker a
failure err = throwError err

todoImplementError :: String -> TypeChecker a
todoImplementError msg = do
    env <- tcEnv
    failure $ Errors.UnknownFailure env msg

tcEnv :: TypeChecker TypeCheckerEnv
tcEnv = get

tcEnvGet :: (TypeCheckerEnv -> t) -> TypeChecker t
tcEnvGet = (flip (<$>)) tcEnv

tcEnvSet :: (TypeCheckerEnv -> TypeCheckerEnv) -> TypeChecker ()
tcEnvSet = modify 