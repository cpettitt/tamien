module Tamien.TI.Eval where

import Tamien.Core
import qualified Tamien.Heap as H
import Tamien.Heap (Addr, Heap)

import Data.List (mapAccumL)
import Data.Maybe (fromMaybe)
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
          | NIndir Addr

-- TODO consider patricia tree
type TiGlobals = [(Name, Addr)]

data TiStats = TiStats Int Int Int

showTrace :: String -> String
showTrace = showResults . trace

trace :: String -> [TiState]
trace = eval . compile . parse'

run :: String -> Int
run = getResult . last . trace

getResult :: TiState -> Int
getResult TiState { tiStack = [addr], tiHeap = heap }
    = case H.lookup addr heap of
        (NNum n) -> n
        _        -> error "Program not in a terminal state!"
getResult _ = error "More than one stack element remains!"

compile :: CoreProgram -> TiState
compile prog
    = TiState { tiStack   = stack
              , tiDump    = initialDump
              , tiHeap    = heap
              , tiGlobals = globals
              , tiStats   = initialStats
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
        dispatch (NApp a1 a2)    = appStep state a1 a2
        dispatch (NSc sc args e) = scStep state sc args e
        dispatch (NIndir a)     = indirStep state a

numStep :: TiState -> Int -> TiState
numStep _ _ = error "Number applied as a function!"

appStep :: TiState -> Addr -> Addr -> TiState
appStep state a1 _ = state { tiStack = a1 : tiStack state }

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep state sc args e
    | length args /= length bindings
        = error $ "Application of '" ++ sc ++ "' with too few arguments"
    | otherwise = state { tiStack = stack', tiHeap = heap2 }
    where
        stack      = tiStack state
        heap       = tiHeap state
        stack'     = a : drop (length args + 1) stack
        heap2      = H.update (stack !! length args) (NIndir a) heap1
        (a, heap1) = instantiate e heap env
        env        = bindings ++ tiGlobals state
        bindings   = zip args (getArgs heap stack)

indirStep :: TiState -> Addr -> TiState
indirStep state a = state { tiStack = a : tail (tiStack state) }

-- drops first stack element, which is the supercombinator
getArgs :: TiHeap -> TiStack -> [Addr]
getArgs heap = map getArg . tail
    where getArg addr = arg
            where (NApp _ arg) = H.lookup addr heap

instantiate :: CoreExpr -> TiHeap -> TiGlobals -> (Addr, TiHeap)
instantiate (Num n) heap _   = H.alloc (NNum n) heap
instantiate (Var v) heap env = (fromMaybe err (lookup v env), heap)
    where err = error $ "Undefined name: " ++ show v
instantiate (App e1 e2) heap env
    = H.alloc (NApp a1 a2) heap2
    where (a1, heap1) = instantiate e1 heap  env
          (a2, heap2) = instantiate e2 heap1 env
instantiate (Constr tag arity) heap env = error "Constr unsupported"
instantiate (Let isRec defs e) heap env = instantiate e heap' env'
    where (heap', env') = instantiateDefs isRec defs heap env
instantiate (Case e alts) heap env = error "Case unsupported"

instantiateDefs :: IsRec -> [(Name, CoreExpr)] -> TiHeap -> TiGlobals -> (TiHeap, TiGlobals)
instantiateDefs isRec defs heap env = (heap', env')
    where env' = zip (map fst defs) addrs ++ env
          (addrs, heap')  = go defs ([], heap) (if isRec then env' else env)
          go []             (as, h) _ = (reverse as, h)
          go ((_, expr):xs) (as, h) e
            = let (a, h') = instantiate expr h e
              in go xs (a:as, h') e

doAdmin :: TiState -> TiState
doAdmin state = state { tiStats = stats' }
    where stack  = tiStack state
          heap   = tiHeap state
          stats' = TiStats (steps + 1)
                           (max max_stack (length stack)) 
                           (max max_heap (H.size heap))
          (TiStats steps max_stack max_heap) = tiStats state

tiFinal :: TiState -> Bool
tiFinal TiState {tiStack = [addr], tiHeap = heap} = isDataNode (H.lookup addr heap)
tiFinal TiState {tiStack = []} = error "Empty stack!"
tiFinal _                      = False

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _        = False

initialDump :: TiDump
initialDump = DummyTiDump

initialStats :: TiStats
initialStats = TiStats 0 0 0

getStatSteps :: TiStats -> Int
getStatSteps (TiStats x _ _) = x

getStatMaxStack :: TiStats -> Int
getStatMaxStack (TiStats _ x _) = x

getStatMaxHeap :: TiStats -> Int
getStatMaxHeap (TiStats _ _ x) = x

buildInitialHeap :: CoreProgram -> (TiHeap, TiGlobals)
buildInitialHeap = mapAccumL allocSc H.empty

allocSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocSc heap (ScDefn name args body) = (heap', (name, addr))
    where
        (addr, heap') = H.alloc (NSc name args body) heap

extraPreludeDefs :: CoreProgram
extraPreludeDefs = []

lookupGlobal :: Name -> TiGlobals -> Addr
lookupGlobal n = fromMaybe err . lookup n
    where err = error $ "Global " ++ n ++ " is not defined"


-- PRETTY PRINTING RESULTS

showResults :: [TiState] -> String
showResults states
    = render $ vcat (zipWith showState [0..] states) $+$
               text "" $+$
               showStats (last states)

showState :: Int -> TiState -> Doc
showState n state = text "Step" <+> int n <+> text ":" $+$
                    nest 4 (showStack state)

showStack :: TiState -> Doc
showStack s
    = vcat [ text "Stack ["
           , nest 4 $ vcat (map (showStackItem s) (reverse (tiStack s)))
           , text "]"
           ]

showStackItem :: TiState -> Addr -> Doc
showStackItem s a = showAddr a <> text ":" <+> showNode s a (H.lookup a (tiHeap s))

showNode :: TiState -> Addr -> Node -> Doc
showNode _ _ (NNum n)       = text "NNum" <+> int n
showNode _ _ (NApp a1 a2)   = text "NApp" <+> showAddr a1 <+> showAddr a2
showNode _ _ (NSc name _ _) = text "NSc" <+> text name
showNode _ _ (NIndir a)     = text "NIndir" <+> showAddr a

showStats :: TiState -> Doc
showStats s = text "Steps taken     =" <+> int (getStatSteps stats) $+$
              text "Max stack depth =" <+> int (getStatMaxStack stats) $+$
              text "Max heap size   =" <+> int (getStatMaxHeap stats)
    where stats = tiStats s

showAddr :: Addr -> Doc
showAddr a = text (replicate (5 - length str) ' ') <> text str
    where str = show a
