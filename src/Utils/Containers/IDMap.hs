{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Utils.Containers.IDMap where


import qualified Data.Map.Ordered as OM
import Data.Foldable
import Data.Typeable
import Data.Maybe
import Control.Monad
import Control.DeepSeq
import GHC.Generics (Generic, Generic1)

class Idable k where
    getID :: k -> String

newtype Map v = Map (OM.OMap String v)
    deriving (Ord, Read, Show, Eq, Typeable, Traversable, Foldable, Generic, Generic1)

instance NFData (Map v) where rnf x = seq x ()
instance NFData1 (Map) where liftRnf _ = rwhnf

_unwrap :: Map v -> OM.OMap String v
_unwrap (Map m) = m

_wrap :: OM.OMap String v -> Map v
_wrap = Map

instance Functor Map where
        fmap f = _wrap . fmap f . _unwrap

empty :: Map v
empty = Map OM.empty

lookup :: String -> Map v -> Maybe v
lookup k (Map m) = OM.lookup k m

fromM :: (Idable v, Monad m, Foldable t) => (String -> m ()) -> t v -> m (Map v)
fromM errHandler = flip (insertSequenceM errHandler (\m v -> v)) empty

-- from :: (Idable v, Foldable t) => t v -> Map v
-- from = Map . OM.fromList . map (\v -> (getID v, v)) . toList

-- fromList :: (Idable v) => [v] -> Map v
-- fromList = from

findM :: (Monad m) => (String -> m ()) -> String -> Map v -> m v
findM errHandler name (Map m) = let result = OM.lookup name m in maybe (errHandler name) (const $ return ()) result >> (return . fromJust) result

findElemM :: (Monad m) => (String -> m ()) -> String -> Map v -> m (String, v, Int)
findElemM errHandler name (Map m) =
    let index = OM.findIndex name m in
    let result = (\i (k, v) -> (k, v, i)) <$> index <*> (OM.elemAt m =<< index) in
    maybe (errHandler name) (const $ return ()) result >> (return . fromJust) result

provideM :: (Idable v, Monad m) => (String -> m v) -> String -> Map v -> m (v, Map v)
provideM provider name (Map m) = maybe ((\newEl -> (newEl, Map $ m OM.|> (getID newEl, newEl))) <$> provider name) (\v -> return (v, Map m)) $ OM.lookup name m

insertM :: (Idable v, Monad m) => (String -> m ()) -> v -> Map v -> m (Map v)
--insertM v = _wrap . (flip (OM.|>)) ((getID v), v) . _unwrap
insertM errHandler v (Map m) = let m' = _wrap . flip (OM.|>) (getID v, v) in (when (OM.member (getID v) m) $ errHandler $ getID v) >> (return . m') m

insertManyM :: (Idable v, Monad m, Foldable t) => (String -> m ()) -> t v -> Map v -> m (Map v)
insertManyM errHandler vals m = concatM errHandler m =<< fromM errHandler vals

insertSequenceM :: (Idable v, Monad m, Foldable t) => (String -> m ()) -> (Map v -> v -> v) -> t v -> Map v -> m (Map v)
insertSequenceM errHandler appender vals m = foldM (\m v -> insertM errHandler (appender m v) m) m vals

concatSequenceM :: (Idable v, Monad m) => (String -> m ()) -> (Map v -> v -> v) -> Map v -> Map v -> m (Map v)
concatSequenceM errHandler appender vals = insertSequenceM errHandler appender $ elems vals

concatM :: (Idable v, Monad m) => (String -> m ()) -> Map v -> Map v -> m (Map v)
concatM errHandler (Map m1) (Map m2) =
    let m' = _wrap . (OM.|<>) m1 in
    let common = OM.elemAt (m1 OM./\| m2) 0 in
    maybe (return ()) (\(id, _) -> errHandler id) common >> (return . m') m2

concatMapsM :: (Idable v, Monad m, Foldable t) => (String -> m ()) -> t (Map v) -> m (Map v)
concatMapsM errHandler = foldM (concatM errHandler) empty

first :: Map v -> Maybe v
first = return . snd <=< flip OM.elemAt 0 . _unwrap

last :: Map v -> Maybe v
last (Map m) = (return . snd <=< flip OM.elemAt (OM.size m-1)) m

mapList :: (String -> v -> t) -> Map v-> [t]
mapList f = map (uncurry f) . OM.assocs . _unwrap

mapElemsM :: (Idable v, Idable v', Monad m) => (String -> m ()) -> (String -> v -> m v') -> Map v -> m (Map v')
mapElemsM errHandler f = (fromM errHandler) <=< (mapM (uncurry f) . OM.assocs . _unwrap)

elems :: Map v -> [v]
elems = mapList (\ _ x -> x)

keys :: (Idable v) => Map v -> [String]
keys = mapList (\k _ -> k)

overrideM :: (Idable v, Monad m) => (String -> m ()) -> v -> Map v -> m (Map v)
overrideM errHandler v (Map m) = let m' = _wrap . flip (OM.>|) (getID v, v) in (when (not $ OM.member (getID v) m) $ errHandler $ getID v) >> (return . m') m

delete :: (Idable v) => String -> Map v -> Map v
delete k = _wrap . OM.delete k . _unwrap

deleteMany :: (Idable v, Traversable t) => t String -> Map v -> Map v
deleteMany keys m = foldl (flip delete) m keys