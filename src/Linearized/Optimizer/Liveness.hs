{-# LANGUAGE FlexibleContexts #-}
module Linearized.Optimizer.Liveness where

import qualified Data.Set as S
import Control.Monad.State

import qualified Linearized.Syntax as L
import qualified Linearized.Optimizer.Values as VP
import qualified Utils.Containers.IDMap as IM

type LiveIn a = [L.Name a]
type LiveOut a = [L.Name a]
type StmtWithLiveness a = (L.Stmt a, LiveIn a, LiveOut a)
type LivenessAnalysis a = [StmtWithLiveness a]

type LiveFrom = Integer
type LiveUntil = Integer
type LivenessInterval a = (L.Name a, LiveFrom, LiveUntil)

type LivenessEntry = (L.Stmt L.IRPosition, [L.Name L.IRPosition], [L.Name L.IRPosition])

analize :: [L.Stmt L.IRPosition] -> [LivenessEntry]
analize stmts = 
    let indexed = zip stmts [1..]
        succ = map (findSucc indexed) indexed
        inout = map (\(s,i,n) -> (s,i,n,S.fromList [],S.fromList [])) succ
    in map (\(s,_,_,tin,tout)->(s,S.toList tin,S.toList tout)) $ work inout
  where
    work :: [(L.Stmt L.IRPosition, Integer,[Integer],S.Set (L.Name L.IRPosition),S.Set (L.Name L.IRPosition))] -> [(L.Stmt L.IRPosition, Integer,[Integer],S.Set (L.Name L.IRPosition),S.Set (L.Name L.IRPosition))]
    work inout =
        let ninout = map (proc inout) inout in
        if ninout /= inout then work ninout
        else ninout
    proc inout (s,i,n,tin,tout) =
        let succin = select (\(_,ii,_,_,_) -> elem ii n) (\(_,_,_,sin,_)->sin) inout []
        in (s,i,n, S.union (S.fromList $ VP.used s) (tout `S.difference` (S.fromList $ VP.assigned s)), S.unions succin)
    select pred f (h:t) acc = if pred h then select pred f t (f h : acc) else select pred f t acc
    select _ _ [] acc = reverse acc

findSucc :: [(L.Stmt L.IRPosition, Integer)] -> (L.Stmt L.IRPosition, Integer) -> (L.Stmt L.IRPosition, Integer, [Integer])
findSucc ind (s,i) = 
    case s of
        (L.Jump p l) -> (s,i,[findIndex ind (L.SetLabel p l)])
        (L.JumpCmp p _ l _ _) -> (s,i,[i+1,findIndex ind (L.SetLabel p l)])
        (L.ReturnVal _ _ _) -> (s,i,[])
        (L.Return _) -> (s,i,[])
        _ -> (s,i,[i+1])
    where
        findIndex ind s = snd $ head $ filter (\(s',i)->s==s') ind

analizeProg (L.Program _ _ funs _) = IM.mapList (\_ f -> (getName f, analize $ getBody f)) funs
    where
        getBody (L.Fun _ _ _ _ _ body) = body
        getName (L.Fun _ _ l _ _ _) = l

analisisPrint livs = concat $ map printOne livs
    where
        printOne (l, ana) = l ++ "\n" ++ (concat $ map printA ana) ++ "\n"
        printA (s, tin, tout) = show s ++ "   "++show tin++"   " ++ show tout ++ "\n"

informationToIntervals :: LivenessAnalysis L.IRPosition -> [LivenessInterval L.IRPosition]
informationToIntervals ss = fst $ execState (mapM_ process ss) ([],1)
    where
        process (s,tin,tout) = add tin
        add tin = do
            curr <- snd <$> get
            map <- fst <$> get
            put (foldl (insert curr) map tin, curr + 1)
        insert c ((n,s,e):ss) m | n == m && c == e + 1 = (n,s,c) : ss
                                | otherwise = (n,s,e) : insert c ss m
        insert c [] m = [(m,c,c)]
