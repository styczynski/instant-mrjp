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
import           IR.Registers
import IR.Flow.Liveness
import IR.Syntax.Syntax
import IR.CodeGen.Stack
import           IR.CodeGen.Consts
import IR.Class
import IR.Size

import IR.RegisterAllocation.RegisterAllocation
import qualified Backend.X64.Parser.Constructor as X64

type Generator a = (StateT (GeneratorEnv) (ReaderT GeneratorContext (ExceptT Errors.Error (X64.ASMGeneratorT () () LattePipeline)))) a

traceEnabled :: Bool
traceEnabled = False

liftGenerator :: (X64.ASMGeneratorT () () LattePipeline) a -> Generator a
liftGenerator = lift . lift . lift

-- isReg :: Loc -> Bool
-- isReg loc = case loc of
--     LocReg _ -> True
--     _        -> False

-- asReg :: Loc -> Reg
-- asReg loc = case loc of
--     LocReg r -> r
--     _        -> error "asReg: not a reg"

-- For n-th argument of a function (starting from zero) give
-- the location it is stored in. Assumes only the return
-- address is stored after arguments on stack, the consumer
-- must correct for the actual offset.
argLoc :: Integer -> X64.Loc
argLoc idx = case idx of
    0 -> X64.LocReg X64.RDI
    1 -> X64.LocReg X64.RSI
    2 -> X64.LocReg X64.RDX
    3 -> X64.LocReg X64.RCX
    4 -> X64.LocReg X64.R8
    5 -> X64.LocReg X64.R9
    _ -> X64.LocMem (X64.RBP, (fromInteger idx - 6) * 8 + 8)

gEnv :: Generator GeneratorEnv
gEnv = get

gContext :: (GeneratorContext -> t) -> Generator t
gContext f = do 
    c <- asks f
    return c

gEnvGet :: (GeneratorEnv -> t) -> Generator t
gEnvGet = (flip (<$>)) gEnv

gEnvSet :: (GeneratorEnv -> GeneratorEnv) -> Generator ()
gEnvSet = modify 

updateLive :: Liveness -> Generator ()
updateLive l = do
    gEnvSet (\env -> env & live .~ l)
    updateLocs


updateLocs :: Generator ()
updateLocs = do
    l <- gEnvGet (^. live)
    forM_ (HashMap.toList $ liveIn l `HashMap.union` liveOut l) updateLoc
    where
        updateLoc :: (String, (Int, SType ())) -> Generator ()
        updateLoc (s, (_, t)) = do
            let vi = ValIdent s
            mbcol <- gContext (Map.lookup vi . regAlloc . (^. regs))
            case mbcol of
                Just reg_ -> do
                    gEnvSet (\env -> env & vars %~ Map.insert vi (VarState vi t (convertReg reg_)))
                    return ()
                Nothing -> return ()

getLoc :: ValIdent -> Generator X64.Loc
getLoc vi = do
    mbvar <- gEnvGet (Map.lookup vi . (^. vars))
    case mbvar of
        Just var -> return $ var ^. varLoc
        Nothing  -> do 
            vs <- gEnvGet (^. vars)
            error $ "internal error. value not found " ++ toStr vi ++ " in vars " ++ (show vs)

getValLoc :: Val a -> Generator X64.Loc
--getValLoc val = return $ argLoc 0
getValLoc val = case val of
    VInt _ n    -> return $ X64.LocConst (fromInteger n)
    VNegInt _ n -> return $ X64.LocConst (fromInteger $ -n)
    VTrue _     -> return $ X64.LocConst 1
    VFalse _    -> return $ X64.LocConst 0
    VVal _ _ vi -> do
        varS <- getVarS vi
        return $ varS ^. varLoc 
    VNull {}    -> return $ X64.LocConst 0

getPreservedRegs :: Generator [Reg]
getPreservedRegs = do
    l <- gEnvGet (^. live)
    cols <- gContext (regAlloc . (^. regs))
    let unchanged = HashMap.filterWithKey (\k _ -> not $ k `HashSet.member` liveKill l) (liveOut l)
        occupied = Map.filterWithKey (\vi _ -> toStr vi `HashMap.member` unchanged) cols
    traceM' $ "occupied regs: " ++ show occupied ++ ", because unchange = " ++ show unchanged
    return $ Map.elems occupied

newStrConst :: String -> Generator Const
newStrConst s = do
    (c, cs) <- gEnvGet (constsAdd s . (^. consts))
    gEnvSet (\env -> env & consts .~ cs)
    return c

-- Generate a label in the context of the current method.
label :: LabIdent -> Generator LabIdent
label l = gContext ((\f -> f l) . (^. labelGen))

setStack :: Stack -> Generator ()
setStack s = gEnvSet (\env -> env & stack .~ s)

-- Get the description of a variable.
getVarS :: ValIdent -> Generator VarState
getVarS vi = do
    mb <- gEnvGet (Map.lookup vi . (^. vars))
    case mb of
        Nothing -> error $ "internal error. no varS for var " ++ show vi
        Just g  -> return g

-- Get class metadata.
getClass :: SymIdent -> Generator CompiledClass
getClass i = do
    mb <- gContext (Map.lookup i . (^. classes))
    case mb of
        Nothing -> error $ "internal error. no class " ++ toStr i
        Just cl -> return cl

-- Get the size of a variable.
varSize :: VarState -> X64.Size
varSize varS = typeSize $ varS ^. varType 

-- Is the variable currently alive.
isLive :: ValIdent -> Generator Bool
isLive (ValIdent vi) = do
    l <- gEnvGet (^. live)
    return $ HashMap.member vi $ liveIn l

-- Debug

fullTrace :: Generator ()
fullTrace = return ()
-- fullTrace = do
--     l <-gEnvGet (^. live)
--     traceM' ("live: " ++ show (HashMap.keys $ liveIn l))
--     varSs <- gEnvGet (Map.elems . (^. vars))
--     s <- gEnvGet (^. stack)
--     traceM' ("stack: " ++ show (stackReservedSize s) ++ " + " ++ show (stackOverheadSize s))
--     mapM_ (\vs -> traceM' ("value " ++ toStr (varName vs) ++ ", "
--             ++ "type: " ++ show (vs ^. varType)
--             ++ " loc: " ++ show (vs ^. varLoc))) varSs

traceM' :: String -> Generator ()
traceM' s = when traceEnabled (do
    gEnvSet (\env -> env & traceIdx %~ (+1))
    idx <- gEnvGet (^. traceIdx)
    return ()
    --traceM ("{" ++ show idx ++ "}  " ++ s)
    )
