module IR.Utils where

import           Control.Monad (unless, when)
import           Data.Bits     (Bits, countLeadingZeros, (.&.))
import           Data.Foldable (find)
import           Data.Int
import           Data.List     (sort, sortOn)
import qualified Data.Map      as Map
import           Data.Maybe    (fromJust, fromMaybe)

isPowerOfTwo :: (Bits i, Integral i) => i -> Bool
isPowerOfTwo 0 = False
isPowerOfTwo n = n .&. (n-1) == 0


log2 :: Int32 -> Int
log2 = (31 -) . countLeadingZeros

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p a = do
  b <- p
  unless b a

whenM :: Monad m => m Bool -> m () -> m ()
whenM p a = do
  b <- p
  when b a

findDupsBy :: Ord k => (a -> k) -> [a] -> ([k], [a])
findDupsBy f ds = collect $ foldr checkForDup (Map.empty, []) ds
    where
    checkForDup a (m, dups) =
        let k = f a
        in  if Map.member k m then (m, (k, a) : dups) else (Map.insert k a m, dups)
    collect (m, dups) =
        let (ks, as) = unzip dups in (ks, foldr (\k as' -> m Map.! k : as') as ks)

findConflictsBy :: Ord k => (a -> k) -> (b -> k) -> [a] -> [b] -> ([k], [(a, b)])
findConflictsBy fa fb as bs = unzip $ foldr checkForConfl [] bs
    where
    m = Map.fromList $ zip (map fa as) as
    checkForConfl b confls =
        let k = fb b
        in  case Map.lookup k m of
                Nothing -> confls
                Just a  -> (k, (a, b)) : confls

dedup :: Ord a => [a] -> [a]
dedup xs = run (sort xs)
    where run []      = []
          run (x:xs') = x : run (dropWhile (== x) xs' )

dedupBy :: Ord k => (a -> k) -> [a] -> [a]
dedupBy f xs = run (sortOn fst $ map (\x -> (f x, x)) xs)
    where run []           = []
          run ((k, x):xs') = x : run (dropWhile ((== k) . fst) xs')


ffirst :: (Foldable f) => f a -> Maybe a
ffirst = find (const True)

single :: (Foldable f) => f a -> a
single xs = fromMaybe (error "single: empty structure") (ffirst xs)


fixpoint :: (Eq a, Show a) => (a -> a) -> a -> a
fixpoint = fixpointBy id


fixpointBy :: (Eq b, Show b) => (a -> b) -> (a -> a) -> a -> a
fixpointBy ord f x = fixpointBy' 0 ord f x
  where
    fixpointBy' iterNo ord f x = if iterNo > 100 then (error $ "Infinite fixpointBy iteration on: " ++ show (ord $ x) ++ " and " ++ show (ord $ f x)) else let x' = f x in if ord x == ord x' then x' else fixpointBy' (iterNo+1) ord f x'

fixpointM :: (Monad m, Eq a, Show a) => (a -> m a) -> a -> m a
fixpointM = fixpointByM id

fixpointByM :: (Monad m, Eq b, Show b) => (a -> b) -> (a -> m a) -> a -> m a
fixpointByM ord f x = fixpointByM' 0 ord f x
  where
    fixpointByM' iterNo ord f x =  do
      x' <- f x
      if iterNo > 100
        then return (error $ "Infinite fixpointBy iteration on: " ++ show (ord x) ++ " and " ++ show (ord $ x'))
        else if ord x == ord x'
          then return x'
          else fixpointByM' (iterNo+1) ord f x'


splitLast :: [a] -> (a, [a])
splitLast [] = error "empty"
splitLast xs =
    let (a, as) = foldr f (Nothing, []) xs
    in (fromJust a, as)
    where f x (Nothing, as) = (Just x, as)
          f x' (x, as)      = (x, x':as)