module Tamien.GM.Eval where

import Tamien.Core
import Tamien.GM.Compiler
import Tamien.GM.State
import qualified Tamien.Heap as H
import Tamien.Heap (Addr, Heap)

import Data.List (foldl')
import qualified Data.Map as M
import Text.PrettyPrint

type Box a = a -> GmState -> GmState

type Unbox a = Addr -> GmState -> a

showTrace :: String -> String
showTrace = showResults . trace

trace :: String -> [GmState]
trace = eval . compile . parse'

run :: String -> Int
run = getResult . last . trace

getResult :: GmState -> Int
getResult GmState { gmStack = [addr], gmHeap = heap }
    = case H.lookup addr heap of
        (NNum n) -> n
        _        -> error "Program not in a terminal state"
getResult _ = error "More than one stack element remains"

eval :: GmState -> [GmState]
eval state = state : rest
    where rest | gmFinal state = []
               | otherwise     = eval (doAdmin (step state))

doAdmin :: GmState -> GmState
doAdmin s = s { gmStats = statIncSteps (gmStats s) }

gmFinal :: GmState -> Bool
gmFinal = null . gmCode

step :: GmState -> GmState
step state = dispatch i (state { gmCode = is })
    where (i:is) = gmCode state

dispatch :: Instruction -> GmState -> GmState
dispatch (PushGlobal f) = dispPushGlobal f
dispatch (PushInt n)    = dispPushInt n
dispatch (Push n)       = dispPush n
dispatch MkApp          = dispMkApp
dispatch (Update n)     = dispUpdate n
dispatch (Pop n)        = dispPop n
dispatch (Alloc n)      = dispAlloc n
dispatch (Slide n)      = dispSlide n
dispatch Eval           = dispEval
dispatch Add            = arithmetic2 (+)
dispatch Sub            = arithmetic2 (-)
dispatch Mul            = arithmetic2 (*)
dispatch Div            = arithmetic2 div
dispatch Neg            = arithmetic1 negate
dispatch Eq             = comparison (==)
dispatch Ne             = comparison (/=)
dispatch Lt             = comparison (<)
dispatch Lte            = comparison (<=)
dispatch Gt             = comparison (>)
dispatch Gte            = comparison (>=)
dispatch (Cond c1 c2)   = dispatchCond c1 c2
dispatch Unwind         = dispUnwind

dispPushGlobal :: Name -> GmState -> GmState
dispPushGlobal f state = state { gmStack = a : gmStack state }
    where a   = M.findWithDefault err f (gmGlobals state)
          err = error $ "Undeclared global: " ++ f

dispPushInt :: Int -> GmState -> GmState
dispPushInt n state
    = case M.lookup name (gmGlobals state) of
        Just a  -> state { gmStack = a : stack }
        Nothing -> let (a, heap') = H.alloc (NNum n) (gmHeap state)
                       globals'   = M.insert name a (gmGlobals state)
                   in state { gmStack = a : stack, gmHeap = heap', gmGlobals = globals' }
    where name    = show n
          stack   = gmStack state

dispPush :: Int -> GmState -> GmState
dispPush n state = state { gmStack = a : stack }
    where a = stack !! n
          stack = gmStack state

getArg :: Node -> Addr
getArg (NApp _ a) = a

dispMkApp :: GmState -> GmState
dispMkApp state = state { gmStack = a:stack', gmHeap = heap' }
    where (a, heap')     = H.alloc (NApp a1 a2) (gmHeap state)
          (a1:a2:stack') = gmStack state

dispUpdate :: Int -> GmState -> GmState
dispUpdate n state = state { gmStack = stack', gmHeap = heap' }
    where heap'  = H.update an (NIndir (head stack)) (gmHeap state)
          stack' = tail stack
          stack  = gmStack state
          an     = stack !! (n + 1)

dispPop :: Int -> GmState -> GmState
dispPop n state = state { gmStack = drop n (gmStack state) }

dispAlloc :: Int -> GmState -> GmState
dispAlloc n state
    = state { gmStack = stack'
            , gmHeap  = heap'
            }
    where (stack', heap') = foldl' go (gmStack state, gmHeap state) [1..n]
          go (stack, heap) _ = (a : stack, heap2)
                where (a, heap2) = H.alloc (NIndir H.nullAddr) heap

dispSlide :: Int -> GmState -> GmState
dispSlide n state = state { gmStack = a : drop n as }
    where (a:as) = gmStack state

dispEval :: GmState -> GmState
dispEval state
    = state { gmCode  = [Unwind]
            , gmStack = [head stack]
            , gmDump  = (gmCode state, tail stack) : gmDump state
            }
    where stack = gmStack state

dispatchCond :: GmCode -> GmCode -> GmState -> GmState
dispatchCond trueBranch falseBranch state
    = state { gmCode  = code ++ gmCode state
            , gmStack = as
            }
    where (a:as) = gmStack state
          code = case H.lookup a (gmHeap state) of
                    (NNum 1) -> trueBranch
                    (NNum 0) -> falseBranch
                    _        -> error "Boolean type error"
dispUnwind :: GmState -> GmState
dispUnwind state = newState (H.lookup a (gmHeap state))
    where (a:as) = gmStack state
          newState (NNum n)
                | null dump = state
                | otherwise
                    = state { gmCode = code
                            , gmStack = a : stack
                            , gmDump = tail dump
                            }
                where dump = gmDump state
                      (code, stack) = head dump
          newState (NApp a1 a2)  = state { gmCode = [Unwind], gmStack = a1:a:as }
          newState (NGlobal n c)
                | length as < n
                    = case gmDump state of
                        []            -> error "Unwinding with too few arguments"
                        ((i, s):rest) -> state { gmCode = i
                                               , gmStack = last (gmStack state) : s
                                               , gmDump  = rest
                                               }
                | otherwise     = state { gmCode = c
                                        , gmStack = rearrangeStack n (gmHeap state) (gmStack state)
                                        }
          newState (NIndir a1)   = state { gmCode = [Unwind], gmStack  = a1 : as }

-- | Updates the stack such that the values of application nodes for a
--   supercombinator replace the original application nodes. The root of the
--   redex is left in place so that it can be update.
rearrangeStack :: Int -> GmHeap -> GmStack -> GmStack
rearrangeStack n heap as
    = take n as' ++ drop n as
    where as' = map (getArg . (`H.lookup` heap)) (tail as)

boxInt :: Int -> GmState -> GmState
boxInt n state
    = state { gmStack = a : gmStack state
            , gmHeap  = heap'
            }
    where (a, heap') = H.alloc (NNum n) (gmHeap state)

unboxInt :: Addr -> GmState -> Int
unboxInt a state
    = case H.lookup a (gmHeap state) of
        (NNum n) -> n
        _        -> error "Unboxing a non-integer"

boxBool :: Bool -> GmState -> GmState
boxBool b state
    = state { gmStack = a : gmStack state
            , gmHeap  = heap'
            }
    where (a, heap') = H.alloc (NNum b') (gmHeap state)
          b' | b         = 1
             | otherwise = 0

primitive1 :: Box b -> Unbox a -> (a -> b) -> GmState -> GmState
primitive1 box unbox op state
    = box (op (unbox a state)) (state { gmStack = as })
    where (a:as) = gmStack state

primitive2 :: Box b -> Unbox a -> (a -> a -> b) -> GmState -> GmState
primitive2 box unbox op state
    = box (op (unbox a1 state) (unbox a2 state)) (state { gmStack = as })
    where (a1:a2:as) = gmStack state

arithmetic1 :: (Int -> Int) -> GmState -> GmState
arithmetic1 = primitive1 boxInt unboxInt

arithmetic2 :: (Int -> Int -> Int) -> GmState -> GmState
arithmetic2 = primitive2 boxInt unboxInt

comparison :: (Int -> Int -> Bool) -> GmState -> GmState
comparison = primitive2 boxBool unboxInt

-- PRETTY PRINTING RESULTS

showResults :: [GmState] -> String
showResults states
    = render $
        vcat [ text "Supercombinator definitions"
             , text "==========================="
             , M.foldrWithKey (\k v a -> a $+$ showSC s k v) empty (gmGlobals s)
             , text ""
             , text "State transitions"
             , text "================="
             , vcat (zipWith showState [0..] states)
             , text ""
             , showStats (last states)
             ]

    where s = head states

showSC :: GmState -> Name -> Addr -> Doc
showSC state n a = text "Code for" <+> doubleQuotes (text n) $+$
                        nest 4 (showInstructions code)
    where (NGlobal _ code) = H.lookup a (gmHeap state)

showInstructions :: GmCode -> Doc
showInstructions code
    = vcat [ text "Code {"
           , nest 4 (vcat (map (text . show) code))
           , text "}"
           , text ""
           ]

showState :: Int -> GmState -> Doc
showState n state = text "Step" <+> int n <+> text ":" $+$
                    nest 4
                      (vcat [ showStack state
                            , showDump state
                            , showInstructions (gmCode state)
                            ])

showStack :: GmState -> Doc
showStack s
    = vcat [ text "Stack ["
           , nest 4 $ vcat (map (showStackItem s) (reverse (gmStack s)))
           , text "]"
           ]

showStackItem :: GmState -> Addr -> Doc
showStackItem s a = text (show a) <> text ":" <+> showNode s a (H.lookup a (gmHeap s))

showDump :: GmState -> Doc
showDump s
    = vcat [ text "Dump ["
           , nest 4 $ vcat (zipWith (showDumpItem s) [0..] (reverse (gmDump s)))
           , text "]"
           ]

showDumpItem :: GmState -> Int -> GmDumpItem -> Doc
showDumpItem s n (code, stack)
    = text "Item" <+> int n <+> text ":" $+$
        nest 4
            (vcat [ showInstructions (take 3 code)
                  , showStack (s { gmStack = take 3 stack })
                  ])  $+$
        text ""

showNode :: GmState -> Addr -> Node -> Doc
showNode s a (NNum n)      = int n
showNode s a (NGlobal n g) = text "Global" <+> text v
    where v = head [ x | (x, b) <- M.assocs (gmGlobals s), a == b]
showNode s a (NApp a1 a2)  = text "Ap" <+> text (show a1) <+> text (show a2)
showNode s a (NIndir a1)   = text "NIndir" <+> text (show a1)

showStats :: GmState -> Doc
showStats s = text "Steps taken =" <+> int (statGetSteps (gmStats s))
