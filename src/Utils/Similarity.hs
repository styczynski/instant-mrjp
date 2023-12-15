{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Utils.Similarity where

import Data.Foldable

class PrettyPrint x where
    printi :: Int -> x -> String

class NearEq a where
    similar :: a -> a -> Bool
    filterSimilar :: (Traversable t) => a -> t a -> Maybe [a]
    filterSimilar item col = let colList = toList col in case filter (similar item) colList of
        [] -> Nothing
        sim -> Just sim
    (<?) :: (Traversable t) => a -> t a -> Maybe [a]
    (<?) = filterSimilar

instance NearEq String where
    similar x y = x == y

instance (NearEq a) => NearEq [a] where
    similar x y = all (uncurry similar) $ zip x y
-- instance {-# OVERLAPPABLE #-} (Eq a) => NearEq a where
--     similar x y = x == y

instance (PrettyPrint a) => NearEq a where
    similar x y = similar (printi 0 x) (printi 0 y)

-- instance (Traversable t, NearEq e) => NearEq (t e) where
--     similar x y = all (uncurry similar) $ zip (toList x) (toList y)
