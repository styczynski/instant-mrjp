module Backend.X86.RegisterAllocator where


import Data.List (nub, (\\), sort)
import Data.Char
import Data.Maybe (fromJust, fromMaybe, isJust, listToMaybe)
import Data.Functor ((<$>))
import Data.Map (fromListWith, toList)
import Control.Monad.State

import Debug.Trace

import qualified Backend.X86.Syntax as X
import Linearized.Syntax
import Linearized.Optimizer.Values
import Linearized.Optimizer.Liveness

type ValMap = [(Name IRPosition, [X.Value IRPosition])]
data Free = Free | Busy (Name IRPosition) deriving (Eq,Show)
type Interval = (Free, Integer, Integer)
type RegIntervals = [(X.Reg IRPosition, [Interval])]

type StackSize = Integer
type RegState = (RegIntervals, StackSize, ValMap)

st (Reference _) = 0x08
st (IntT _) = 0x04
st (ByteT _) = 0x01

allocateRegisters :: LivenessAnalysis IRPosition ->  [(Type IRPosition, Name IRPosition)] -> [(Name IRPosition, [X.Value IRPosition])] -> RegState 
allocateRegisters sst args regMap = (fstt computedState, stackSize, valMap)
  where
    freeRegs = [X.R11 noPosIR, X.R10 noPosIR, X.R9 noPosIR, X.R8 noPosIR, X.RDX noPosIR, X.RCX noPosIR, X.RAX noPosIR, X.RSI noPosIR, X.RDI noPosIR]
    intervals = informationToIntervals sst
    stmtIntervals = filter (\(n,_,_) -> not $ elem n getArgsNames) intervals
    
    numberOfStatements = fromIntegral $ length sst

    thrd (_,_,x) = x
    fstt (x,_,_) = x
    sndd (_,x,_) = x
    
    initialState = (map (\r -> (r,[(Free, 1, numberOfStatements)])) freeRegs, 8, [])
    
    stateWithArgs = foldl (useValsMapIntervals) initialState regMap

    computedState = foldl insertIntervals stateWithArgs $ groupByName stmtIntervals    

    valMap = thrd computedState
    stackSize = sndd computedState - 8

    useValsMapIntervals state (name, [X.Register _ r]) =
        let ints = filter (\(n,_,_) -> n == name) intervals
        in insertIntoReg name r ints state
    useValsMapIntervals (f,s,m) (name, [mem]) =
        let ints = filter (\(n,_,_) -> n == name) intervals
            state = if length ints > 0 then 
                        (f,s,insertValue (name,mem) m)
                    else (f,s,m)
        in case capable ints state of
            Just r -> insertIntoReg name r ints state
            Nothing -> state
    insertIntervals state (name, ints') =
        let ints = map (\(a,b) -> (name,a,b)) ints' in
        case capable ints state of
            Just r -> insertIntoReg name r ints state
            Nothing -> spillRegister name state

    stripNames = map (\x -> (sndd x, thrd x))
    getArgsNames = map snd args
    groupByName ints = toList $ fromListWith (++) [(k, [(vf,vt)]) | (k, vf,vt) <- ints]

    capable ints state =
        let regss = fstt state
        in listToMaybe $ map fst $ filter (canFitInto ints) regss
    canFitInto ints (reg, intervals) = all (canFitIntoOne intervals) ints
    canFitIntoOne intervals int = any (fits int) intervals
    fits (n,s,e) (b,f,u) = b == Free && f <= s && e <= u

    spillRegister name (regss,stack,vmap) = 
        let t = findType name
        in (regss, stack + (st t), insertValue (name, X.Memory (noPosIR) (X.RBP noPosIR) Nothing (Just (-stack - (st t))) (Just t)) vmap)

    insertIntoReg name r ints (regss, ss, sm) =
        let t = findType name in
        (ins (X.topReg r) ints regss, ss, insertRegister (name, X.regSize t r) sm)
    ins r ints ((q,intervals):ss) | r == q = (r,insertInt ints intervals) : ss
                                  | otherwise = (q,intervals) : ins r ints ss
    insertInt ints intervals = foldl (insertIntOne) intervals ints
    insertIntOne ((b,f,t):ii) (n,s,e) | fits (n,s,e) (b,f,t) =
        let before = if s > f then [(Free,f,s-1)] else []
            after = if e < t then [(Free,e+1,t)] else []
        in before ++ [(Busy n, s,e)] ++ after ++ ii
    insertIntOne (i:ii) int = i : insertIntOne ii int
    -- TODO: Handle empty case gracefully
    insertIntOne _ _ = []
    insertRegister (n,r) = insertValue (n, X.Register noPosIR r)
    insertValue (n,v) ((m,l):ms) | m == n = (m,v : l) : ms
                          | otherwise = (m,l) : insertValue (n,v) ms
    insertValue (n,v) [] = [(n, [v])]

    findType n = 
        let d = filter (\(s,_,_) -> declared s == [n]) sst in
        if d /= [] then
            let ((VarDecl _ t _ _),_,_) = head d in t
        else fst $ head $ filter (\(t,m)->m==n) args