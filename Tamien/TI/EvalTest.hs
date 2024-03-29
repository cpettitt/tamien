module Tamien.TI.EvalTest where

import Tamien.TI

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
                ]

main = runTestTT tests
