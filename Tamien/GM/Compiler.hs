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

type GmEnvironment = [(Name, Int)]

preludeDefs :: CoreProgram
preludeDefs
    = [ ScDefn "I"       ["x"]           (Var "x")
      , ScDefn "K"       ["x", "y"]      (Var "x")
      , ScDefn "K1"      ["x", "y"]      (Var "y")
      , ScDefn "S"       ["f", "g", "x"]
            (App (App (Var "f") (Var "x"))
                 (App (Var "g") (Var "x")))

      , ScDefn "compose" ["f", "g", "x"]
            (App (Var "f")
                 (App (Var "g") (Var "x")))

      , ScDefn "twice"   ["f"]
            (App (App (Var "compose") (Var "f"))
                 (Var "f"))
      ]

compiledPrimitives :: [GmCompiledSc]
compiledPrimitives
    = [("+", 2, [Push 1, Eval, Push 1, Eval, Add, Update 2, Pop 2, Unwind])
      ,("-", 2, [Push 1, Eval, Push 1, Eval, Sub, Update 2, Pop 2, Unwind])
      ,("*", 2, [Push 1, Eval, Push 1, Eval, Mul, Update 2, Pop 2, Unwind])
      ,("/", 2, [Push 1, Eval, Push 1, Eval, Div, Update 2, Pop 2, Unwind])
      ,("negate", 1, [Push 0, Eval, Neg, Update 1, Pop 1, Unwind])
      ,("==", 2, [Push 1, Eval, Push 1, Eval, Eq, Update 2, Pop 2, Unwind])
      ,("/=", 2, [Push 1, Eval, Push 1, Eval, Ne, Update 2, Pop 2, Unwind])
      ,("<", 2, [Push 1, Eval, Push 1, Eval, Lt, Update 2, Pop 2, Unwind])
      ,("<=", 2, [Push 1, Eval, Push 1, Eval, Lte, Update 2, Pop 2, Unwind])
      ,(">", 2, [Push 1, Eval, Push 1, Eval, Gt, Update 2, Pop 2, Unwind])
      ,(">=", 2, [Push 1, Eval, Push 1, Eval, Gte, Update 2, Pop 2, Unwind])
      ,("if", 3, [Push 0, Eval, Cond [Push 1] [Push 2], Update 3, Pop 3, Unwind])
      ]

compile :: CoreProgram -> GmState
compile program = emptyState { gmCode    = initialCode
                             , gmHeap    = heap'
                             , gmGlobals = globals'
                             }
    where
        heap    = gmHeap emptyState
        globals = gmGlobals emptyState
        sc_defs = preludeDefs ++ program
        (heap', globals') = buildInitialHeap sc_defs heap globals

initialCode :: [Instruction]
initialCode = [PushGlobal "main", Eval]

-- TODO check for name collisions in the global namespace
buildInitialHeap :: CoreProgram -> GmHeap -> GmGlobals -> (GmHeap, GmGlobals)
buildInitialHeap program heap globals
    = foldl' step (heap, globals) (map compileSc program ++ compiledPrimitives)
    where step (h, g) x =  second (flip (uncurry M.insert) g) (allocSc h x)

allocSc :: GmHeap -> GmCompiledSc -> (GmHeap, (Name, Addr))
allocSc heap (name, arity, is) = (heap', (name, addr))
    where (addr, heap') = H.alloc (NGlobal arity is) heap

compileSc :: CoreScDefn -> GmCompiledSc
compileSc (ScDefn name env body)
    = (name, length env, compileR body (zip env [0..]))

compileR :: GmCompiler
compileR e env = compileC e env ++ [Update n, Pop n, Unwind]
    where n = length env

compileC :: GmCompiler
compileC (Num n) env = [PushInt n]
compileC (Var v) env
    = case lookup v env of
        Nothing -> [PushGlobal v]
        Just n  -> [Push n]
compileC (App e1 e2) env = compileC e2 env ++
                           compileC e1 (argOffset 1 env) ++
                           [MkApp]
compileC (Let rec defs e) env
    | rec       = compileLetRec compileC defs e env
    | otherwise = compileLet    compileC defs e env

compileLetRec :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLetRec comp defs expr env
    = [Alloc n] ++ compileLetRec' env' defs ++ comp expr env' ++ [Slide n]
    where n = length defs
          env' = compileArgs defs (argOffset n env)

compileLetRec' :: GmEnvironment -> [(Name, CoreExpr)] -> GmCode
compileLetRec' env defs = concat $ zipWith go defs [n-1, n-2 .. 0]
    where n = length defs
          go (_, expr) m = compileC expr env ++ [Update m]

compileLet :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLet comp defs expr env
    = compileLet' env defs ++ comp expr env' ++ [Slide (length defs)]
    where env' = compileArgs defs env

compileLet' :: GmEnvironment -> [(Name, CoreExpr)] -> GmCode
compileLet' env defs = fst $ foldl' go ([], env) defs
    where go (acc, env) (_, expr) = (acc ++ compileC expr env, argOffset 1 env)

compileArgs :: [(Name, CoreExpr)] -> GmEnvironment -> GmEnvironment
compileArgs defs env
    = env' ++ env
    where env' = zip (map fst defs) [n-1, n-2 .. 0]
          n    = length defs

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n = map (second (+ n))
