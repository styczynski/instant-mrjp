module IR.RegisterAllocation.SequenceColouring (seqColouring) where

import           Control.Monad
import           Data.List
import qualified Data.Map                                    as Map
import           Data.Maybe
import qualified Data.Set                                    as Set
import           IR.Utils
import           IR.RegisterAllocation.InterferenceGraph

import qualified Backend.X64.Parser.Constructor as X64


seqColouring :: [String] -> InterferenceGraph -> Either [InterferenceNode] InterferenceGraph
seqColouring nodes g =
    let tryColoured = foldl' go g nodes
    in case Map.elems $ Map.filter (isNothing . iNodeColour) (ig tryColoured) of
        [] -> Right tryColoured
        xs -> Left $ dedupBy iNodeLabel $ concatMap (\x -> x:neighbours g x) xs

go :: InterferenceGraph -> String -> InterferenceGraph
go (IG g) n =
    case Map.lookup n g of
        Just node ->
            let colour = assignNodeColour (IG g) node
            in if isJust $ iNodeColour node
                then IG g
                else IG $ Map.insert n (node {iNodeColour = colour}) g
        Nothing -> error $ "internal error. seqColouring: node not found " ++ n

assignNodeColour :: InterferenceGraph -> InterferenceNode -> Maybe X64.Reg
assignNodeColour (IG g) node =
    let neighbourColours = Set.map (iNodeColour . (g Map.!)) (iNodeOut node)
        usedColours = Set.map fromJust $ Set.filter isJust neighbourColours
        freeColours = filter (not . (`Set.member` usedColours)) X64.allRegs
        prefColours = filter ((== iNodeRegPref node) . X64.regType) freeColours
        prefColour = find (const True) prefColours
        freeColour = find (const True) freeColours
    in msum [prefColour, freeColour]

neighbours :: InterferenceGraph -> InterferenceNode -> [InterferenceNode]
neighbours (IG g) node = map (g Map.!) (Set.elems (iNodeOut node))
