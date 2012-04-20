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
import Control.Arrow (first)
import qualified Data.IntMap as M

newtype Addr = Addr Int
    deriving Eq

data Heap a = Heap [Int] (M.IntMap a)

instance Show Addr where
    show (Addr x) = '#':show x

instance Show a => Show (Heap a) where
    show (Heap _ assigned) = show $ map (first Addr) $ M.assocs assigned

instance Eq a => Eq (Heap a) where
    (Heap _ x) == (Heap _ y) = x == y

empty :: Heap a
empty = Heap [1..] M.empty

alloc :: a -> Heap a -> (Addr, Heap a)
alloc v (Heap (next:free) assigned)
    = (Addr next, Heap free (M.insert next v assigned))

update :: Addr -> a -> Heap a -> Heap a
update ak v (Heap free assigned)
    = (Heap free (updateMap ak (Just v) assigned))

free :: Addr -> Heap a -> Heap a
free ak@(Addr k) (Heap free assigned)
    = (Heap (k:free) (updateMap ak Nothing assigned))

updateMap :: Addr -> Maybe a -> M.IntMap a -> M.IntMap a
updateMap ak@(Addr k) v map
    = case M.updateLookupWithKey f k map of
        (Nothing, _) -> error $ "nonde " ++ show ak ++ " is not in the heap"
        (Just _,  x) -> x
    where f = const . const v

lookup :: Addr -> Heap a -> a
lookup ak@(Addr k) (Heap _ assigned)
    = case M.lookup k assigned of
        Just x  -> x
        Nothing -> error $ "node " ++ show ak ++ " is not in heap"

addresses :: Heap a -> [Addr]
addresses (Heap _ assigned) = map Addr $ M.keys assigned

size :: Heap a -> Int
size (Heap _ assigned) = M.size assigned
