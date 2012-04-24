module Tamien.TI.EvalTest where

import Tamien.TI

import Test.HUnit

assertProgEq prog
    = assertEqual prog (run prog)

testId = assertProgEq "main = I 3" 3

testId2 = assertProgEq "main = S K K 3" 3

testId3 = assertProgEq "main = twice (I I I) 3" 3

tests = TestList $
            map TestCase
                [ testId
                , testId2
                , testId3
                ]

main = runTestTT tests
