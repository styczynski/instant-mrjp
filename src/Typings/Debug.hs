module Typings.Debug where

import Control.Lens

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.FuzzySet.Simple as Fuzz
import Data.Maybe

import Typings.Env
import qualified Typings.Types as Type

import Utils.Similarity

data QueryFn = QueryFn String (Maybe (Type.Type, [Type.Type]))

class TCEnvQuerable q r | q -> r where
    query :: TypeCheckerEnv -> q -> [r]
    (<--?) :: TypeCheckerEnv -> q -> [r]
    (<--?) = query

instance TCEnvQuerable QueryFn Type.Function where
    query env q@(QueryFn name _) = filter (matchFn q) $ mapMaybe ((findFunction env) . T.unpack . snd) $ Fuzz.find (T.pack name) (env^.definedFunsFuzz)
        where
            matchFn :: QueryFn -> Type.Function -> Bool
            matchFn (QueryFn _ (Just (ret, args))) fn@(Type.Fun _ fnRet fnArgs _) = (similar ret fnRet) && (similar args $ Type.funcArgsTypes fn)
            matchFn _ _ = True

--a^.file