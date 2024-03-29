{-# LANGUAGE FlexibleInstances #-}
module Backend.X64.Def where

import Control.Lens hiding (Const)

import Control.Monad.Except hiding (void)
import Control.Monad.Reader hiding (void)
import Control.Monad.State hiding (void)

import qualified Data.Map                      as Map
import qualified Data.HashMap.Strict           as HashMap
import qualified Data.HashSet                  as HashSet

import qualified Typings.Env as TypeChecker
import qualified Reporting.Errors.Def as Errors
import Reporting.Logs
import Backend.X64.Env
import IR.Flow.Liveness
import IR.Syntax.Syntax
import IR.CodeGen.Stack
import           IR.CodeGen.Consts
import IR.Class
import IR.Size

import IR.RegisterAllocation.RegisterAllocation
import qualified Backend.X64.Parser.Constructor as X64

data ASMAnno = ASMAnno String

instance (Show a) => X64.CommentProvider a ASMAnno where
    toComment pos (ASMAnno comment) = comment ++ " at " ++ show pos
    defaultComment pos = Just $ show pos

type Generator a v = (StateT (GeneratorEnv) (ReaderT GeneratorContext (ExceptT Errors.Error (X64.ASMGeneratorT a (ASMAnno) LattePipeline)))) v

comment :: String -> Maybe ASMAnno
comment = Just . ASMAnno

traceEnabled :: Bool
traceEnabled = False

gen :: (X64.ASMGeneratorT a (ASMAnno) LattePipeline) v -> Generator a v
gen = lift . lift . lift


argLoc :: Integer -> X64.Loc
argLoc idx = case idx of
    0 -> X64.LocReg X64.RDI
    1 -> X64.LocReg X64.RSI
    2 -> X64.LocReg X64.RDX
    3 -> X64.LocReg X64.RCX
    4 -> X64.LocReg X64.R8
    5 -> X64.LocReg X64.R9
    _ -> X64.LocMem (X64.RBP, (fromInteger idx - 6) * 8 + 8)

gEnv :: Generator a GeneratorEnv
gEnv = get

gContext :: (GeneratorContext -> t) -> Generator a t
gContext f = do 
    c <- asks f
    return c

gEnvGet :: (GeneratorEnv -> t) -> Generator a t
gEnvGet = (flip (<$>)) gEnv

gEnvSet :: (GeneratorEnv -> GeneratorEnv) -> Generator a ()
gEnvSet = modify 

updateLive :: Liveness -> Generator a ()
updateLive l = do
    gEnvSet (\env -> env & live .~ l)
    updateLocs


updateLocs :: Generator a ()
updateLocs = do
    l <- gEnvGet (^. live)
    forM_ (HashMap.toList $ liveIn l `HashMap.union` liveOut l) updateLoc
    where
        updateLoc :: (String, (Int, SType ())) -> Generator a ()
        updateLoc (s, (_, t)) = do
            let vi = IRValueName s
            mbcol <- gContext (Map.lookup vi . regAlloc . (^. regs))
            case mbcol of
                Just reg_ -> do
                    gEnvSet (\env -> env & vars %~ Map.insert vi (VarState vi t (X64.asLoc reg_)))
                    return ()
                Nothing -> return ()

getLoc :: IRValueName -> Generator a X64.Loc
getLoc vi = do
    mbvar <- gEnvGet (Map.lookup vi . (^. vars))
    case mbvar of
        Just var -> return $ var ^. varLoc
        Nothing  -> do 
            vs <- gEnvGet (^. vars)
            error $ "internal error. value not found " ++ toStr vi ++ " in vars " ++ (show vs)

getValLoc :: Val b -> Generator a X64.Loc
getValLoc val = case val of
    VInt _ n    -> return $ X64.LocConst (fromInteger n)
    VNegInt _ n -> return $ X64.LocConst (fromInteger $ -n)
    VTrue _     -> return $ X64.LocConst 1
    VFalse _    -> return $ X64.LocConst 0
    VVal _ _ vi -> do
        varS <- getVarS vi
        return $ varS ^. varLoc 
    VNull {}    -> return $ X64.LocConst 0

getPreservedRegs :: Generator a [X64.Reg]
getPreservedRegs = do
    l <- gEnvGet (^. live)
    cols <- gContext (regAlloc . (^. regs))
    let unchanged = HashMap.filterWithKey (\k _ -> not $ k `HashSet.member` liveKill l) (liveOut l)
        occupied = Map.filterWithKey (\vi _ -> toStr vi `HashMap.member` unchanged) cols
    traceM' $ "occupied regs: " ++ show occupied ++ ", because unchange = " ++ show unchanged
    return $ Map.elems occupied

newStrConst :: String -> Generator a Const
newStrConst s = do
    (c, cs) <- gEnvGet (constsAdd s . (^. consts))
    gEnvSet (\env -> env & consts .~ cs)
    return c

label :: IRLabelName -> Generator a IRLabelName
label l = gContext ((\f -> f l) . (^. labelGen))

setStack :: Stack -> Generator a ()
setStack s = gEnvSet (\env -> env & stack .~ s)

getVarS :: IRValueName -> Generator a VarState
getVarS vi = do
    mb <- gEnvGet (Map.lookup vi . (^. vars))
    case mb of
        Nothing -> error $ "internal error. no varS for var " ++ show vi
        Just g  -> return g

getClass :: IRTargetRefName -> Generator a CompiledClass
getClass i = do
    mb <- gContext (Map.lookup i . (^. classes))
    case mb of
        Nothing -> error $ "internal error. no class " ++ toStr i
        Just cl -> return cl

varSize :: VarState -> X64.Size
varSize varS = typeSize $ varS ^. varType 

isLive :: IRValueName -> Generator a Bool
isLive (IRValueName vi) = do
    l <- gEnvGet (^. live)
    return $ HashMap.member vi $ liveIn l

fullTrace :: Generator a ()
fullTrace = return ()

traceM' :: String -> Generator a ()
traceM' s = when traceEnabled (do
    gEnvSet (\env -> env & traceIdx %~ (+1))
    idx <- gEnvGet (^. traceIdx)
    return ()
    )
