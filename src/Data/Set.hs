module Data.Set
  ( Set
  , empty
  , insert
  , toList
  ) where

import qualified Data.List (insert)

newtype Set a = Set { toList :: [a] } deriving (Show, Eq)

empty :: Set a
empty = Set []

insert :: Ord a => a -> Set a -> Set a
insert x s = if x `elem` toList s then s else Set . Data.List.insert x $ toList s