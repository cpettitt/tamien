module Tamien.GM.EvalTest where

import Tamien.GM

import Test.HUnit

assertProgEq x prog
    = assertEqual prog x (run prog)

testId = assertProgEq 3 "main = I 3"
testId2 = assertProgEq 3 "main = S K K 3"
testId3 = assertProgEq 3 "main = twice (I I I) 3"

testLet = assertProgEq 3 "main = let id = I I I in id 3"

testLetRec = assertProgEq 4
                          "pair x y f = f x y; \
                          \fst p = p K; \
                          \snd p = p K1; \
                          \f x y = letrec \
                          \    a = pair x b; \
                          \    b = pair y a \
                          \    in fst (snd (snd (snd a))); \
                          \main = f 3 4"

testNegate = assertProgEq (-3) "main = negate 3"
testNegateIndir = assertProgEq (-3) "main = negate (I 3)"
testNegateTwice = assertProgEq 3 "main = twice negate 3"

testAdd = assertProgEq 9 "main = + 6 3"

testSub = assertProgEq 3 "main = - 6 3"

testMul = assertProgEq 18 "main = * 6 3"

testDiv = assertProgEq 2 "main = / 6 3"
testDivTruncation = assertProgEq 2 "main = / 7 3"

testPrimArithIndir = assertProgEq 26 "main = + (* 6 3) 8"
testPrimArithIndir2 = assertProgEq 26 "main = + 8 (* 6 3)"
testPrimArithIndir3 = assertProgEq 26 "main = + (* 6 3) (I 8)"

testEq1 = assertProgEq 1 "main = (== 5 5)"
testEq2 = assertProgEq 0 "main = (== 5 4)"

testNe1 = assertProgEq 0 "main = (/= 5 5)"
testNe2 = assertProgEq 1 "main = (/= 5 4)"

testGt1 = assertProgEq 1 "main = (> 5 4)"
testGt2 = assertProgEq 0 "main = (> 4 5)"
testGt3 = assertProgEq 0 "main = (> 5 5)"

testGte1 = assertProgEq 1 "main = (>= 5 4)"
testGte2 = assertProgEq 0 "main = (>= 4 5)"
testGte3 = assertProgEq 1 "main = (>= 5 5)"

testLt1 = assertProgEq 0 "main = (< 5 4)"
testLt2 = assertProgEq 1 "main = (< 4 5)"
testLt3 = assertProgEq 0 "main = (< 5 5)"

testLte1 = assertProgEq 0 "main = (<= 5 4)"
testLte2 = assertProgEq 1 "main = (<= 4 5)"
testLte3 = assertProgEq 1 "main = (<= 5 5)"

testFac = assertProgEq 120 "fac n = if (== n 0) 1 (* n (fac (- n 1))); main = fac 5"

testFib = assertProgEq 8 "fib n = if (== n 0) 0 (if (== n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))); main = fib 6"

tests = TestList $
            map TestCase
                [ testId
                , testId2
                , testId3
                , testLet
                , testLetRec
                , testNegate
                , testNegateIndir
                , testNegateTwice
                , testAdd
                , testSub
                , testMul
                , testDiv
                , testDivTruncation
                , testPrimArithIndir
                , testPrimArithIndir2
                , testPrimArithIndir3
                , testEq1
                , testEq2
                , testNe1
                , testNe2
                , testGt1
                , testGt2
                , testGt3
                , testGte1
                , testGte2
                , testGte3
                , testLt1
                , testLt2
                , testLt3
                , testLte1
                , testLte2
                , testLte3
                , testFac
                , testFib
                ]

main = runTestTT tests
