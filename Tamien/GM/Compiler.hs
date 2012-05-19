module Tamien.GM.Compiler where

import Tamien.Core
import qualified Tamien.Heap as H
import Tamien.Heap (Addr)
import Tamien.GM.State

import Control.Arrow (second)
import Data.List (foldl')
import Data.Maybe (fromJust, isJust)
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
    = [("negate", 1, [Push 0, Eval, Neg, Update 1, Pop 1, Unwind])
      ,("if", 3, [Push 0, Eval, Cond [Push 1] [Push 2], Update 3, Pop 3, Unwind])
      ] ++ map dyadic builtInDyadic
    where dyadic (op, i) = (op, 2, [Push 1, Eval, Push 1, Eval, i, Update 2, Pop 2, Unwind])

builtInDyadic :: [(Name, Instruction)]
builtInDyadic
    = [("+", Add), ("-", Sub), ("*", Mul), ("/", Div)
      ,("==", Eq), ("/=", Ne)
      ,(">", Gt), (">=", Gte), ("<", Lt), ("<=", Lte)
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
compileR e env = compileE e env ++ [Update n, Pop n, Unwind]
    where n = length env

compileE :: GmCompiler
compileE (Num n) env = [PushInt n]
compileE (Let rec defs e) env
    | rec       = compileLetRec compileE defs e env
    | otherwise = compileLet    compileE defs e env
compileE e env
    | Just (f, xs) <- findApp e = compileE' e xs f env
compileE e env = compileC e env ++ [Eval]

-- | This function supports compilation of some primitive function applications
--   in a strict context.
compileE' :: CoreExpr -> [CoreExpr] -> GmCompiler
compileE' _ [a] (Var "negate") env = compileE a env ++ [Neg]
compileE' _ [a0, a1] (Var e) env
    | Just op <- lookup e builtInDyadic
        = compileE a1 env ++ compileE a0 (argOffset 1 env) ++ [op]
compileE' _ [a0, a1, a2] (Var "if") env
    = compileE a0 env ++ [Cond (compileE a1 env) (compileE a2 env)]
compileE' orig _ _ env = compileC orig env

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
