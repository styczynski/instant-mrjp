module IR.RegisterAllocation.InterferenceGraph (
    buildInterferenceGraph,
    getColouring,
    InterferenceGraph(..),
    InterferenceNode(..)
) where

import           Control.Monad.State
import qualified Data.HashMap.Strict           as HashMap
import qualified Data.HashSet                  as HashSet
import qualified Data.Map                      as Map
import           Data.Maybe
import qualified Data.Set                      as Set
import           IR.Flow.CFG hiding (addEdge)
import           IR.Flow.Liveness
import           IR.Syntax.Syntax
import           IR.Utils
import           IR.Loc
import           IR.Registers

import qualified Backend.X64.Parser.Constructor as X64

newtype InterferenceGraph = IG {
    ig :: Map.Map String InterferenceNode
}

data InterferenceNode = IN {
    iNodeLabel   :: String,
    iNodeColour  :: Maybe X64.Reg,
    iNodeRegPref :: X64.RegType,
    iNodeOut     :: Set.Set String
}

data InterferenceState = St {
    stIg      :: InterferenceGraph,
    stIgnored :: Set.Set String,
    stDivs    :: Integer,
    stCalls   :: Integer
}

getColouring :: InterferenceGraph -> Map.Map String X64.Reg
getColouring (IG g) = Map.map (fromJust . iNodeColour) g

buildInterferenceGraph :: CFG Liveness -> InterferenceGraph
buildInterferenceGraph g = stIg $ execState (go (linearise g)) (St (IG Map.empty) Set.empty 0 0)
    where
        go instrs = do
            forM_ instrs goOne
            ign <- gets stIgnored
            forM_ (Set.elems ign) removeNode
        goOne instr = do
            let live = single instr
            case instr of
                IOp _ vi lhs (OpDiv _) rhs | not $ isSimpleDiv rhs -> goDiv live vi lhs rhs X64.RAX
                IOp _ vi lhs (OpMod _) rhs | not $ isSimpleDiv rhs -> goDiv live vi lhs rhs X64.RDX
                IVCall _ call  -> do
                    createCallEdges call live
                    forM_ (HashMap.keysSet (liveIn live) `HashSet.intersection` HashMap.keysSet (liveOut live)) markSurvivesCall
                ICall _ _ call -> do
                    createCallEdges call live
                    forM_ (HashMap.keysSet (liveIn live) `HashSet.intersection` HashMap.keysSet (liveOut live)) markSurvivesCall
                ILoad _ _ (PParam _ _ n vi) | X64.isReg (X64.argLoc n) -> precolourI vi (X64.asReg $ X64.argLoc n)
                ILoad _ _ (PParam _ _ _ vi) -> addIgnored (toStr vi)      -- Parameter already on stack. Ignore interference.
                _ -> return ()
            addEdgesBetween (HashMap.keys $ liveOut live)
        goDiv live vi lhs rhs dest = do
            (divRax, divRdx) <- createDivNodes
            let interfering = divRax:divRdx:filter (/= toStr vi) (HashMap.keys $ liveOut live)
            addEdgesBetween interfering
            precolourI vi dest
            case lhs of
                VVal _ _ (ValIdent vil) -> addEdge vil divRdx
                _                       -> return ()
            case rhs of
                VVal _ _ (ValIdent vir) -> addEdge vir divRax >> addEdge vir divRdx
                _                       -> let (VVal _ _ (ValIdent vil)) = lhs in addEdge vil divRax

createCallEdges :: Call a -> Liveness -> State InterferenceState ()
createCallEdges call l = do
    let vals = case call of
                Call _ _ _ vs _     -> vs
                CallVirt _ _ _ vs -> vs
        valLocs = zip vals (map X64.argLoc [0..])
    forM_ valLocs (uncurry edgesForArg)
    where
        edgesForArg val (X64.LocReg reg_ ) = do
            n <- gets stCalls
            modify (\st -> st{stCalls = n + 1})
            let nArg = "~arg_" ++ show n ++ "_" ++ show reg_
                nodeArg = IN nArg (Just reg_) X64.CallerSaved Set.empty
                interfering = HashMap.keysSet $ liveUse l
                interfering' = case val of
                    VVal _ _ (ValIdent vi) -> HashSet.delete vi interfering
                    _                      -> interfering
            modify (\st -> st{stIg = IG $ Map.insert nArg nodeArg $ ig $ stIg st})
            forM_ interfering' (addEdge nArg)
            forM_ interfering' (`addEdge` nArg)
        edgesForArg _ _                                     = return ()

createDivNodes :: State InterferenceState (String, String)
createDivNodes = do
    n <- gets stDivs
    modify (\st -> st{stDivs = n + 1})
    let nRes = "~div_" ++ show n ++ "_rax"
        nRem = "~div_" ++ show n ++ "_rdx"
        nodeRes = IN nRes (Just X64.RAX) X64.CallerSaved Set.empty
        nodeRem = IN nRem (Just X64.RDX) X64.CallerSaved Set.empty
    modify (\st -> st{stIg = IG $ Map.insert nRes nodeRes $ ig $ stIg st})
    modify (\st -> st{stIg = IG $ Map.insert nRem nodeRem $ ig $ stIg st})
    addEdge nRes nRem
    addEdge nRem nRes
    return (nRes, nRem)

precolourI :: ValIdent -> X64.Reg -> State InterferenceState ()
precolourI (ValIdent vi) col = do
    node <- getNode vi
    let node' = node {iNodeColour = Just col}
    modify (\st -> st{stIg = IG $ Map.insert vi node' $ ig $ stIg st})

addEdgesBetween :: [String] -> State InterferenceState ()
addEdgesBetween xs =
    let edges = [(x1, x2) | x1 <- xs, x2 <- xs, x1 /= x2]
    in forM_ xs getNode >> forM_ edges (uncurry addEdge)

addEdge :: String -> String -> State InterferenceState ()
addEdge n1 n2 = do
    node1 <- getNode n1
    let node1' = node1 {iNodeOut = Set.insert n2 (iNodeOut node1)}
    modify (\st -> st{stIg = IG $ Map.insert n1 node1' $ ig $ stIg st})

addIgnored :: String -> State InterferenceState ()
addIgnored n = modify (\st -> st{stIgnored = Set.insert n (stIgnored st)})

getNode :: String -> State InterferenceState InterferenceNode
getNode n = do
    mbnode <- gets (Map.lookup n . ig . stIg)
    case mbnode of
        Just node -> return node
        Nothing -> do
            let node = IN n Nothing X64.CallerSaved Set.empty
            modify (\st -> st{stIg = IG $ Map.insert n node $ ig $ stIg st})
            return node

removeNode :: String -> State InterferenceState ()
removeNode n = do
    mbnode <- gets (Map.lookup n . ig . stIg)
    case mbnode of
        Nothing -> return ()
        Just node -> do
            modify (\st -> st{stIg = IG $ Map.delete n $ ig $ stIg st})
            forM_ (iNodeOut node) removeFromOthers
    where
        removeFromOthers :: String -> State InterferenceState ()
        removeFromOthers n' = modifyNode n' (\node' -> node'{iNodeOut = Set.delete n (iNodeOut node')})

markSurvivesCall :: String -> State InterferenceState ()
markSurvivesCall n = modifyNode n (\node -> node{iNodeRegPref = X64.CalleeSaved})

isSimpleDiv :: Val a -> Bool
isSimpleDiv val = case val of
    VInt _ n -> isPowerOfTwo n
    _        -> False

modifyNode :: String -> (InterferenceNode -> InterferenceNode) -> State InterferenceState ()
modifyNode n f = do
    mbnode <- gets (Map.lookup n . ig . stIg)
    case mbnode of
        Nothing -> return ()
        Just node -> do
            let node' = f node
            modify (\st -> st{stIg = IG $ Map.insert n node' $ ig $ stIg st})

instance Show InterferenceGraph where
    show (IG g) = unlines $ Map.elems $ Map.map showOne g
        where
        showOne node = iNodeLabel node ++ " " ++ showPref (iNodeRegPref node) ++ " " ++ showColour (iNodeColour node) ++ ": " ++ show (Set.elems (iNodeOut node))
        showColour Nothing    = ""
        showColour (Just col) = "(" ++ show col ++ ")"
        showPref t       = "(reg preference: " ++ show t ++ ")"
