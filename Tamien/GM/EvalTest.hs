module Tamien.GM.EvalTest where

import Tamien.GM

import Test.HUnit

assertProgEq x prog
    = assertEqual prog x (run prog)

testId = assertProgEq 3 "main = I 3"

testId2 = assertProgEq 3 "main = S K K 3"

testId3 = assertProgEq 3 "main = twice (I I I) 3"

tests = TestList $
            map TestCase
                [ testId
                , testId2
                , testId3
                ]

main = runTestTT tests
