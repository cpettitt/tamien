module Tamien.TI where

import Tamien.Core
import qualified Tamien.Heap as H
import Tamien.Heap (Addr, Heap)

import Data.List (foldl')
import qualified Data.Map as M
import Data.Map (Map)
import Text.PrettyPrint

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
eval state = state : rest
    where rest | tiFinal state = []
               | otherwise     = eval next
          next = doAdmin (step state)

step :: TiState -> TiState
step state = dispatch (H.lookup (head $ tiStack state) (tiHeap state))
    where
        dispatch (NNum n)        = numStep state n
        dispatch (NApp a1 a2)    = apStep state a1 a2
        dispatch (NSc sc args e) = scStep state sc args e

numStep :: TiState -> Int -> TiState
numStep state n = error "Number applied as a function!"

apStep :: TiState -> Addr -> Addr -> TiState
apStep state a1 a2 = state { tiStack = a1 : tiStack state }

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep state sc args e = state { tiStack = stack', tiHeap = heap' }
    where
        stack      = tiStack state
        heap       = tiHeap state
        stack'     = a : drop (length args + 1) stack
        (a, heap') = instantiate e heap env
        env        = M.fromList bindings `M.union` tiGlobals state
        bindings   = zip args (getArgs heap stack)

-- drops first stack element, which is the supercombinator
getArgs :: TiHeap -> TiStack -> [Addr]
getArgs heap = map getArg . tail
    where getArg addr = arg
            where (NApp fun arg) = H.lookup addr heap

instantiate :: CoreExpr -> TiHeap -> M.Map Name Addr -> (Addr, TiHeap)
instantiate (Num n) heap env = H.alloc (NNum n) heap
instantiate (Var v) heap env = (M.findWithDefault err v env, heap)
    where err = error $ "Undefined name: " ++ show v
instantiate (App e1 e2) heap env
    = H.alloc (NApp a1 a2) heap2
    where (a1, heap1) = instantiate e1 heap  env
          (a2, heap2) = instantiate e2 heap1 env
instantiate (Constr tag arity) heap env = error "Constr unsupported"
instantiate (Let isrec defs e) heap env = error "Let(rec) unsupported"
instantiate (Case e alts) heap env = error "Case unsupported"

doAdmin :: TiState -> TiState
doAdmin = applyToTiStats incrTiStatSteps

tiFinal :: TiState -> Bool
tiFinal TiState {tiStack = [addr], tiHeap = heap} = isDataNode (H.lookup addr heap)
tiFinal TiState {tiStack = []} = error "Empty stack!"
tiFinal _                      = False

isDataNode :: Node -> Bool
isDataNode (NNum n) = True
isDataNode _        = False

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


-- PRETTY PRINTING RESULTS

showResults :: [TiState] -> String
showResults states
    = render $ vcat (map showState states) $+$
               showStats (last states)
    where s = head states

showState :: TiState -> Doc
showState state = nest 4 $ showStack state

showStack :: TiState -> Doc
showStack s
    = vcat [ text "Stack ["
           , nest 4 $ vcat (map (showStackItem s) (reverse (tiStack s)))
           , text "]"
           ]

showStackItem :: TiState -> Addr -> Doc
showStackItem s a = showAddr a <> text ":" <+> showNode s a (H.lookup a (tiHeap s))

showNode :: TiState -> Addr -> Node -> Doc
showNode s a (NNum n)       = text "NNum" <+> int n
showNode s a (NApp a1 a2)   = text "NApp" <+> showAddr a1 <+> showAddr a2
showNode s a (NSc name _ _) = text "NSc" <+> text name

showStats :: TiState -> Doc
showStats s = text "Steps taken =" <+> int (getTiStatSteps (tiStats s))

showAddr :: Addr -> Doc
showAddr a = text (take (5 - length str) (repeat ' ')) <> text str
    where str = show a
