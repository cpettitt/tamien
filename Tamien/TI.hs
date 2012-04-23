module Tamien.TI where

import Tamien.Core
import qualified Tamien.Heap as H
import Tamien.Heap (Addr, Heap)

import Data.List (foldl')
import qualified Data.Map as M
import Data.Map (Map)

data TiState = TiState
    { tiStack   :: TiStack
    , tiDump    :: TiDump
    , tiHeap    :: TiHeap
    , tiGlobals :: TiGlobals
    , tiStats   :: TiStats
    }

type TiStack = [Addr]

data TiDump = DummyTiDump

type TiHeap = Heap Node

data Node = NApp Addr Addr
          | NSc Name [Name] CoreExpr
          | NNum Int

-- TODO consider patricia tree
type TiGlobals = Map Name Addr

data TiStats = TiStats Int

trace :: String -> String
trace = showResults . eval . compile . parse'

compile :: CoreProgram -> TiState
compile prog
    = TiState { tiStack   = stack
              , tiDump    = initialTiDump
              , tiHeap    = heap
              , tiGlobals = globals
              , tiStats   = initialTiStats
              }
    where
        sc_defs         = prog ++ preludeDefs ++ extraPreludeDefs
        (heap, globals) = buildInitialHeap sc_defs
        stack           = [lookupGlobal "main" globals]

eval :: TiState -> [TiState]
eval = undefined

showResults :: [TiState] -> String
showResults = undefined

initialTiDump :: TiDump
initialTiDump = DummyTiDump

initialTiStats :: TiStats
initialTiStats = TiStats 0

incrTiStatSteps :: TiStats -> TiStats
incrTiStatSteps = TiStats . (+ 1) . getTiStatSteps

getTiStatSteps :: TiStats -> Int
getTiStatSteps (TiStats n) = n

applyToTiStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToTiStats f state = state { tiStats = f (tiStats state) }

buildInitialHeap :: CoreProgram -> (TiHeap, TiGlobals)
buildInitialHeap = foldl' (uncurry allocSc) (H.empty, M.empty)

allocSc :: TiHeap -> TiGlobals -> CoreScDefn -> (TiHeap, TiGlobals)
allocSc heap globals (ScDefn name args body) = (heap', globals')
    where
        (addr, heap') = H.alloc (NSc name args body) heap
        globals'      = M.insert name addr globals

extraPreludeDefs :: CoreProgram
extraPreludeDefs = []

lookupGlobal :: Name -> TiGlobals -> Addr
lookupGlobal n = M.findWithDefault err n
    where err = error $ "Global " ++ n ++ " is not defined"
