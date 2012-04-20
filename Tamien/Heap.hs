module Tamien.Heap
    ( Addr
    , Heap
    , empty
    , alloc
    , update
    , free
    , lookup
    , addresses
    , size
    ) where

import Prelude hiding (lookup)
import qualified Data.IntMap as M

newtype Addr = Addr Int
    deriving Show

data Heap a = Heap [Int] (M.IntMap a)

instance Show a => Show (Heap a) where
    show (Heap _ assigned) = show assigned

empty :: Heap a
empty = Heap [1..] M.empty

alloc :: a -> Heap a -> (Addr, Heap a)
alloc v (Heap (next:free) assigned)
    = (Addr next, Heap free (M.insert next v assigned))

update :: Addr -> a -> Heap a -> Heap a
update (Addr k) v (Heap free assigned)
    = (Heap free (M.insert k v assigned))

free :: Addr -> Heap a -> Heap a
free (Addr k) (Heap free assigned)
    = (Heap (k:free) (M.delete k assigned))

lookup :: Addr -> Heap a -> a
lookup (Addr k) (Heap _ assigned)
    = case M.lookup k assigned of
        Just x  -> x
        Nothing -> error $ "node " ++ show k ++ " is not in heap"

addresses :: Heap a -> [Addr]
addresses (Heap _ assigned) = map Addr $ M.keys assigned

size :: Heap a -> Int
size (Heap _ assigned) = M.size assigned
