module Tamien.GM.Compiler where

import Tamien.Core
import qualified Tamien.Heap as H
import Tamien.Heap (Addr)
import Tamien.GM.State

import Control.Arrow (second)
import Data.List (foldl')
import qualified Data.Map as M

type GmCompiledSc = (Name, Int, GmCode)

type GmCompiler = CoreExpr -> GmEnvironment -> GmCode

type GmEnvironment = M.Map Name Int

compile :: CoreProgram -> GmState
compile program = emptyState { gmCode    = initialCode
                             , gmHeap    = heap'
                             , gmGlobals = globals'
                             }
    where
        heap    = gmHeap emptyState
        globals = gmGlobals emptyState
        (heap', globals') = buildInitialHeap program heap globals

initialCode :: [Instruction]
initialCode = [PushGlobal "main", Unwind]

buildInitialHeap :: CoreProgram -> GmHeap -> GmGlobals -> (GmHeap, GmGlobals)
buildInitialHeap program heap globals
    = foldl' step (heap, globals) (map compileSc program)
    where step (h, g) x =  second (flip (uncurry M.insert) g) (allocSc h x)

allocSc :: GmHeap -> GmCompiledSc -> (GmHeap, (Name, Addr))
allocSc heap (name, arity, is) = (heap', (name, addr))
    where (addr, heap') = H.alloc (NGlobal arity is) heap

compileSc :: CoreScDefn -> GmCompiledSc
compileSc (ScDefn name env body)
    = (name, length env, compileR body (M.fromList (zip env [0..])))

compileR :: GmCompiler
compileR e env = compileC e env ++ [Slide (M.size env + 1), Unwind]

compileC :: GmCompiler
compileC (Num n) env = [PushInt n]
compileC (Var v) env
    = case M.lookup v env of
        Nothing -> [PushGlobal v]
        Just n  -> [Push n]
compileC (App e1 e2) env = compileC e2 env ++
                           compileC e1 (argOffset 1 env) ++
                           [MkApp]

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n = M.map (+ n)