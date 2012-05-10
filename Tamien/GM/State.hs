module Tamien.GM.State where

import Tamien.Core
import Tamien.Heap as H
import Tamien.Heap (Heap)

import qualified Data.Map as M

data GmState = GmState
    { gmCode    :: GmCode
    , gmStack   :: GmStack
    , gmHeap    :: GmHeap
    , gmGlobals :: GmGlobals
    , gmStats   :: GmStats
    }
    deriving Show

type GmCode = [Instruction]

data Instruction
    = PushGlobal Name
    | PushInt Int
    | Push Int
    | MkApp
    | Update Int
    | Pop Int
    | Unwind
    deriving (Eq, Show)

type GmStack = [Addr]

type GmHeap = Heap Node

data Node
    = NNum Int
    | NApp Addr Addr
    | NGlobal Int GmCode
    | NIndir Addr
    deriving Show

-- TODO candidate for a Patricia tree?
type GmGlobals = M.Map Name Addr

newtype GmStats = GmStats Int
    deriving Show

emptyState :: GmState
emptyState = GmState [] [] H.empty M.empty initialStats

initialStats :: GmStats
initialStats = GmStats 0

statIncSteps :: GmStats -> GmStats
statIncSteps (GmStats n) = GmStats (n + 1)

statGetSteps :: GmStats -> Int
statGetSteps (GmStats n) = n
