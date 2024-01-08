module Backend.X86.RegisterAllocator where


import Data.List (nub, (\\), sort)
import Data.Char
import Data.Maybe (fromJust, fromMaybe, isJust, listToMaybe)
import Data.Functor ((<$>))
import Data.Map (fromListWith, toList)
import Control.Monad.State

import Debug.Trace

import qualified Backend.X86.Syntax as X
-- import ValuePropagation
-- import LinearRepresentation
-- import LivenessAnalysis
import Linearized.Syntax
import Linearized.Optimizer.Values
import Linearized.Optimizer.Liveness

type ValMap = [(Name IRPosition, [X.Value IRPosition])]
data Free = Free | Busy (Name IRPosition) deriving (Eq,Show)
type Interval = (Free, Integer, Integer)
type RegIntervals = [(X.Reg IRPosition, [Interval])]

type StackSize = Integer
type RegState = (RegIntervals, StackSize, ValMap)

allocateRegisters :: LivenessInformation IRPosition ->  [(Type IRPosition, Name IRPosition)] -> [(Name IRPosition, [X.Value IRPosition])] -> RegState 
allocateRegisters sst args regMap = (fstt computedState, stackSize, valMap)
  where
    freeRegs = [X.R11 noPosIR, X.R10 noPosIR, X.R9 noPosIR, X.R8 noPosIR, X.RDX noPosIR, X.RCX noPosIR, X.RAX noPosIR, X.RSI noPosIR, X.RDI noPosIR]
    intervals = informationToIntervals sst
    stmtIntervals = filter (\(n,_,_) -> not $ elem n argNames) intervals
    
    numberOfStatements = fromIntegral $ length sst

    thrd (_,_,x) = x
    fstt (x,_,_) = x
    sndd (_,x,_) = x
    
    initialState = (map (\r -> (r,[(Free, 1, numberOfStatements)])) freeRegs, 8, [])
    
    stateWithArgs = foldl (insertIntervalsFromValMap) initialState regMap

    computedState = foldl insertIntervals stateWithArgs $ groupByName stmtIntervals    

    valMap = thrd computedState
    stackSize = sndd computedState - 8

    insertIntervalsFromValMap state (name, [X.Register _ r]) =
        let ints = filter (\(n,_,_) -> n == name) intervals
        in insertIntoReg name r ints state
    insertIntervalsFromValMap (f,s,m) (name, [mem]) =
        let ints = filter (\(n,_,_) -> n == name) intervals
            state = if length ints > 0 then 
                        (f,s,insV (name,mem) m)
                    else (f,s,m)
        in case capable ints state of
            Just r -> insertIntoReg name r ints state
            Nothing -> state
    insertIntervals state (name, ints') =
        let ints = map (\(a,b) -> (name,a,b)) ints' in
        case capable ints state of
            Just r -> insertIntoReg name r ints state
            Nothing -> insertSpill name state

    stripNames = map (\x -> (sndd x, thrd x))
    argNames = map snd args
    groupByName ints = toList $ fromListWith (++) [(k, [(vf,vt)]) | (k, vf,vt) <- ints]

    capable ints state =
        let regss = fstt state
        in listToMaybe $ map fst $ filter (canFit ints) regss
    canFit ints (reg, intervals) = all (canFitOne intervals) ints
    canFitOne intervals int = any (fits int) intervals
    fits (n,s,e) (b,f,u) = b == Free && f <= s && e <= u

    insertIntoReg name r ints (regss, ss, sm) =
        let t = findType name in
        (ins (X.topReg r) ints regss, ss, insR (name, X.regSize t r) sm)
    -- ins r _ ((q,_):_) | trace (show r++" "++show q) False = undefined
    ins r ints ((q,intervals):ss) | r == q = (r,insi ints intervals) : ss
                                  | otherwise = (q,intervals) : ins r ints ss
    insi ints intervals = foldl (insiOne) intervals ints
    insiOne ((b,f,t):ii) (n,s,e) | fits (n,s,e) (b,f,t) =
        let before = if s > f then [(Free,f,s-1)] else []
            after = if e < t then [(Free,e+1,t)] else []
        in before ++ [(Busy n, s,e)] ++ after ++ ii
    insiOne (i:ii) int = i : insiOne ii int
    insR (n,r) = insV (n, X.Register noPosIR r)
    insV (n,v) ((m,l):ms) | m == n = (m,v : l) : ms
                          | otherwise = (m,l) : insV (n,v) ms
    insV (n,v) [] = [(n, [v])]

    insertSpill name (regss,stack,vmap) = 
        let t = findType name
        in (regss, stack + (st t), insV (name, X.Memory (noPosIR) (X.RBP noPosIR) Nothing (Just (-stack - (st t))) (Just t)) vmap)
        where
            st (IntT _) = 0x04
            st (ByteT _) = 0x01
            st (Reference _) = 0x08

    findType n = 
        let d = filter (\(s,_,_) -> declared s == [n]) sst in
        if d /= [] then
            let ((VarDecl _ t _ _),_,_) = head d in t
        else fst $ head $ filter (\(t,m)->m==n) args