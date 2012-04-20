module Tamien.HeapTest where

import Tamien.Heap
import Tamien.TestUtil

import Prelude hiding (lookup)
import Test.QuickCheck

prop_allocFree :: String -> Bool
prop_allocFree x
    = let (k, h) = alloc x empty
          h'     = free k h
      in h' == empty
                    

prop_allocLookup :: String -> Bool
prop_allocLookup x
    = let (k, h) = alloc x empty
      in lookup k h == x

prop_emptySize = size empty == 0

prop_allocSize :: String -> Bool
prop_allocSize x
    = size (snd (alloc x empty)) == 1

prop_allocFreeSize :: String -> Bool
prop_allocFreeSize x
    = let (k, h) = alloc x empty
          h'     = free k h
      in size h' == 0

prop_allocUpdate :: String -> String -> Gen Prop
prop_allocUpdate x x2
    = (x /= x2) ==>
        let (k, h) = alloc x empty
            h'     = update k x2 h
        in lookup k h' == x2

runChecks = do
    check "allocFree"       prop_allocFree
    check "allocLookup"     prop_allocLookup
    check "emptySize"       prop_emptySize 
    check "allocSize"       prop_allocSize
    check "allocFreeSize"   prop_allocFreeSize
    check "allocUpdate"     prop_allocUpdate

main = runChecks
