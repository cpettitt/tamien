module Tamien.GM.Eval where

import Tamien.Core
import Tamien.GM.Compiler
import Tamien.GM.State
import qualified Tamien.Heap as H
import Tamien.Heap (Addr, Heap)

import qualified Data.Map as M
import Text.PrettyPrint

run :: String -> String
run = showResults . eval . compile . parse'

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
dispatch (PushGlobal f) = pushGlobal f
dispatch (PushInt n)    = pushInt n
dispatch (Push n)       = push n
dispatch MkApp          = mkApp
dispatch (Slide n)      = slide n
dispatch Unwind         = unwind

pushGlobal :: Name -> GmState -> GmState
pushGlobal f state = state { gmStack = a : gmStack state }
    where a   = M.findWithDefault err f (gmGlobals state)
          err = error $ "Undeclared global: " ++ f

pushInt :: Int -> GmState -> GmState
pushInt n state = state { gmStack = stack', gmHeap = heap' }
    where (a, heap') = H.alloc (NNum n) (gmHeap state)
          stack'     = a : gmStack state

push :: Int -> GmState -> GmState
push n state = state { gmStack = a : gmStack state }
    where a = getArg (H.lookup (gmStack state !! (n + 1)) (gmHeap state))

getArg :: Node -> Addr
getArg (NApp _ a) = a

mkApp :: GmState -> GmState
mkApp state = state { gmStack = a:stack', gmHeap = heap' }
    where (a, heap')     = H.alloc (NApp a1 a2) (gmHeap state)
          (a1:a2:stack') = gmStack state

slide :: Int -> GmState -> GmState
slide n state = state { gmStack = a : drop n as }
    where (a:as) = gmStack state

unwind :: GmState -> GmState
unwind state = newState (H.lookup a (gmHeap state))
    where (a:as) = gmStack state
          newState (NNum n)      = state 
          newState (NApp a1 a2)  = state { gmCode = [Unwind], gmStack = a1:a:as }
          newState (NGlobal n c) | length as < n = error "Unwinding with too few arguments"
                                 | otherwise     = state { gmCode = c }

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
             , vcat (map showState states) 
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

showState :: GmState -> Doc
showState state = nest 4 $
                    vcat [ showStack state
                          , showInstructions (gmCode state) 
                          ]

showStack :: GmState -> Doc
showStack s
    = vcat [ text "Stack ["
           , nest 4 $ vcat (map (showStackItem s) (reverse (gmStack s)))
           , text "]"
           ]

showStackItem :: GmState -> Addr -> Doc
showStackItem s a = text (show a) <> text ":" <+> showNode s a (H.lookup a (gmHeap s))

showNode :: GmState -> Addr -> Node -> Doc
showNode s a (NNum n)      = int n
showNode s a (NGlobal n g) = text "Global" <+> text v
    where v = head [ x | (x, b) <- M.assocs (gmGlobals s), a == b]
showNode s a (NApp a1 a2)  = text "Ap" <+> text (show a1) <+> text (show a2)

showStats :: GmState -> Doc
showStats s = text "Steps taken =" <+> int (statGetSteps (gmStats s))
