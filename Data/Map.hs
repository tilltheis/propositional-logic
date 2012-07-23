module Data.Map
  ( Map
  , elems
  , empty
  , filter
  , insert
  , keys
  , lookup
  , singleton
  , toList
  , union
  ) where

import Prelude hiding (filter, lookup)
import qualified Prelude (filter, lookup)

import Data.List (deleteBy, insertBy, unionBy)
import Data.Function (on)

newtype Map k v = Map { toList :: [(k, v)] } deriving (Show, Eq)

instance (Ord k, Ord v) => Ord (Map k v) where
  compare = compare `on` toList

elems :: Map k a -> [a]
elems = map snd . toList

empty :: Map k a
empty = Map []

filter :: (a -> Bool) -> Map k a -> Map k a
filter p = Map . Prelude.filter (p . snd) . toList

insert :: Ord k => k -> a -> Map k a -> Map k a
insert k v = Map . insertNew . deleteOld . toList
  where insertNew = insertBy (compare `on` fst) (k, v)
        deleteOld = deleteBy ((==) `on` fst) (k, undefined)

keys :: Map k a -> [k]
keys = map fst . toList

lookup :: Ord k => k -> Map k a -> Maybe a
lookup k = Prelude.lookup k . toList

singleton :: k -> a -> Map k a
singleton k v = Map [(k, v)]

union :: Ord k => Map k a -> Map k a -> Map k a
union m1 m2 = Map $ unionBy ((==) `on` fst) (toList m1) (toList m2)