module Tamien.TI.Eval where

import Tamien.Core
import qualified Tamien.Heap as H
import Tamien.Heap (Addr, Heap)

import Data.List (foldl', mapAccumL)
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

type TiDump = [TiStack]

type TiHeap = Heap Node

data Node = NApp Addr Addr
          | NSc Name [Name] CoreExpr
          | NNum Int
          | NIndir Addr
          | NPrim Name Primitive
          | NMarkedNode Node
    deriving Show

-- TODO consider patricia tree
type TiGlobals = [(Name, Addr)]

data TiStats = TiStats Int Int Int Int

data Primitive = Neg | Add | Sub | Mul | Div
    deriving Show

minGcHeapSize :: Int
minGcHeapSize = 20

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
        dispatch (NIndir a)      = indirStep state a
        dispatch (NPrim n p)     = primStep state n p

numStep :: TiState -> Int -> TiState
numStep state n
    | length stack == 1 && not (null dump)
        = state { tiStack = head dump, tiDump = tail dump }
    where stack = tiStack state
          dump  = tiDump state
numStep _ _     = error "Number applied as a function!"

appStep :: TiState -> Addr -> Addr -> TiState
appStep state a1 a2
    = case H.lookup a2 heap of
        (NIndir a3) -> state { tiHeap = H.update (head stack) (NApp a1 a3) heap }
        _           -> state { tiStack = a1 : stack }
    where heap  = tiHeap state
          stack = tiStack state

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep state sc args e
    | length args /= length bindings
        = error $ "Application of '" ++ sc ++ "' with too few arguments"
    | otherwise = state { tiStack = stack', tiHeap = heap' }
    where
        stack      = tiStack state
        heap       = tiHeap state
        stack'     = drop (length args) stack
        heap'      = instantiateAndUpdate e (stack !! length args) heap env
        env        = bindings ++ tiGlobals state
        bindings   = zip args (getArgs heap stack)

indirStep :: TiState -> Addr -> TiState
indirStep state a = state { tiStack = a : tail (tiStack state) }

primStep :: TiState -> Name -> Primitive -> TiState
primStep state n Neg = primNeg state n
primStep state n Add = primArith state n (+)
primStep state n Sub = primArith state n (-)
primStep state n Mul = primArith state n (*)
primStep state n Div = primArith state n (div)

primNeg :: TiState -> Name -> TiState
primNeg state name
    | length stack > 2 = error $ "Type error: more than one argument to primitive '" ++ name ++ "'"
    | length stack < 2 = error $ "Application of primitive '" ++ name ++ "' with too few arguments"
    | isDataNode arg = state { tiStack = drop 1 stack
                             , tiHeap = H.update (stack !! 1) (NNum (-n)) heap
                             }
    | otherwise = state { tiStack = [arg_addr]
                        , tiDump  = [stack !! 1] : tiDump state
                        }
    where
        arg_addr = head $ getArgs heap stack
        arg      = H.lookup arg_addr heap
        (NNum n) = arg
        heap     = tiHeap state
        stack    = tiStack state

primArith state name op
    | length stack > 3 = error $ "Type error: more than two arguments to primitive '" ++ name ++ "'"
    | length stack < 3 = error $ "Application of primitive '" ++ name ++ "' with too few arguments"
    | not (isDataNode a1) = state { tiStack = [a1_addr]
                                  , tiDump = drop 1 stack : tiDump state
                                  }
    | not (isDataNode a2) = state { tiStack = [a2_addr]
                                  , tiDump = [stack !! 2] : tiDump state
                                  }
    | otherwise
        = state { tiStack = drop 2 stack, tiHeap = H.update (stack !! 2) (NNum (op n1 n2)) heap }
    where a1_addr   = head args
          a2_addr   = args !! 1
          a1        = H.lookup a1_addr heap
          a2        = H.lookup a2_addr heap
          (NNum n1) = a1
          (NNum n2) = a2
          args      = getArgs heap stack
          heap      = tiHeap state
          stack     = tiStack state

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

instantiateAndUpdate :: CoreExpr -> Addr -> TiHeap -> TiGlobals -> TiHeap
instantiateAndUpdate (Num n) addr heap _ = H.update addr (NNum n) heap
instantiateAndUpdate (Var v) addr heap env
    = H.update addr (NIndir (fromMaybe err (lookup v env))) heap
    where err = error $ "Undefined name: " ++ show v
instantiateAndUpdate (App e1 e2) addr heap env
    = H.update addr (NApp a1 a2) heap2
    where (a1, heap1) = instantiate e1 heap env
          (a2, heap2) = instantiate e2 heap1 env
instantiateAndUpdate (Constr tag arity) addr heap env = error "Cosntr unsupported"
instantiateAndUpdate (Let isRec defs e) addr heap env = instantiateAndUpdate e addr heap' env'
    where (heap', env') = instantiateDefs isRec defs heap env
instnatiateAndUpdate (Case e alts) addr heap env = error "Case unsupported"

instantiateDefs :: IsRec -> [(Name, CoreExpr)] -> TiHeap -> TiGlobals -> (TiHeap, TiGlobals)
instantiateDefs isRec defs heap env = (heap', env')
    where env' = zip (map fst defs) addrs ++ env
          (addrs, heap')  = go defs ([], heap) (if isRec then env' else env)
          go []             (as, h) _ = (reverse as, h)
          go ((_, expr):xs) (as, h) e
            = let (a, h') = instantiate expr h e
              in go xs (a:as, h') e

doAdmin :: TiState -> TiState
doAdmin state = state2
    where stack   = tiStack state
          heap    = tiHeap state
          need_gc = H.size heap > minGcHeapSize
          stats'  = TiStats (steps + 1)
                            (max max_stack (length stack)) 
                            (max max_heap (H.size heap))
                            (if need_gc then gcs + 1 else gcs)
          (TiStats steps max_stack max_heap gcs) = tiStats state
          state1 = state { tiStats = stats' }
          state2 | need_gc   = gc state1
                 | otherwise = state1

tiFinal :: TiState -> Bool
tiFinal TiState {tiStack = [addr], tiDump = [], tiHeap = heap}
    = isDataNode (H.lookup addr heap)
tiFinal TiState {tiStack = []} = error "Empty stack!"
tiFinal _                      = False

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _        = False

initialDump :: TiDump
initialDump = []

initialStats :: TiStats
initialStats = TiStats 0 0 0 0

getStatSteps :: TiStats -> Int
getStatSteps (TiStats x _ _ _) = x

getStatMaxStack :: TiStats -> Int
getStatMaxStack (TiStats _ x _ _) = x

getStatMaxHeap :: TiStats -> Int
getStatMaxHeap (TiStats _ _ x _) = x

getStatGcs :: TiStats -> Int
getStatGcs (TiStats _ _ _ x) = x

buildInitialHeap :: CoreProgram -> (TiHeap, TiGlobals)
buildInitialHeap scs = (heap2, sc_addrs ++ prim_addrs)
    where (heap1, sc_addrs)   = mapAccumL allocSc H.empty scs
          (heap2, prim_addrs) = mapAccumL allocPrim heap1 primitives

primitives :: [(Name, Primitive)]
primitives = [ ("negate", Neg)
             , ("+", Add)
             , ("-", Sub)
             , ("*", Mul)
             , ("/", Div) ]

allocSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocSc heap (ScDefn name args body) = (heap', (name, addr))
    where (addr, heap') = H.alloc (NSc name args body) heap

allocPrim :: TiHeap -> (Name, Primitive) -> (TiHeap, (Name, Addr))
allocPrim heap (name, prim) = (heap', (name, addr))
    where (addr, heap') = H.alloc (NPrim name prim) heap

extraPreludeDefs :: CoreProgram
extraPreludeDefs = []

lookupGlobal :: Name -> TiGlobals -> Addr
lookupGlobal n = fromMaybe err . lookup n
    where err = error $ "Global " ++ n ++ " is not defined"

-- GARBAGE COLLECTION

gc :: TiState -> TiState
gc state = state { tiHeap = heap' }
    where heap  = tiHeap state
          roots = findRoots state
          heap' = scanHeap $ foldl' (flip markFrom) heap roots

findRoots :: TiState -> [Addr]
findRoots state
    = stackRoots ++ dumpRoots ++ globalRoots
    where stackRoots  = findStackRoots (tiStack state)
          dumpRoots   = findDumpRoots (tiDump state)
          globalRoots = findGlobalRoots (tiGlobals state)

findStackRoots :: TiStack -> [Addr]
findStackRoots = id

findDumpRoots :: TiDump -> [Addr]
findDumpRoots = concatMap findStackRoots

findGlobalRoots :: TiGlobals -> [Addr]
findGlobalRoots = map snd

markFrom :: Addr -> TiHeap -> TiHeap
markFrom a heap = go n
    where
        n                  = H.lookup a heap
        heap'              = H.update a (NMarkedNode n) heap
        go n@(NApp a1 a2)  = markFrom a1 $ markFrom a2 heap'
        go n@(NIndir a1)   = markFrom a1 heap'
        go (NMarkedNode _) = heap
        go _               = heap'

scanHeap :: TiHeap -> TiHeap
scanHeap heap = foldl' go heap $ H.addresses heap
    where go heap' a = case H.lookup a heap' of
                            (NMarkedNode n) -> H.update a n heap'
                            _               -> H.free a heap'

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
showNode _ _ (NPrim n prim) = text "NPrim" <+> text n <+> text (show prim)

showStats :: TiState -> Doc
showStats s = text "Steps taken     =" <+> int (getStatSteps stats) $+$
              text "Max stack depth =" <+> int (getStatMaxStack stats) $+$
              text "Max heap size   =" <+> int (getStatMaxHeap stats) $+$
              text "Number of GCs   =" <+> int (getStatGcs stats)
    where stats = tiStats s

showAddr :: Addr -> Doc
showAddr a = text (replicate (5 - length str) ' ') <> text str
    where str = show a
