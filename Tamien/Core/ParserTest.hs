{-# LANGUAGE TypeSynonymInstances #-}

{-
 - Tests for the Core language parser
 -}

module Tamien.Core.ParserTest where

import Tamien.Core
import Tamien.TestUtil

import Control.Applicative ((<$>), (<*>))
import Test.QuickCheck

varStart = ('_':['a'..'z'])
varChar  = varStart ++ ['0'..'9']

identifier = suchThat ((:) <$> elements varStart <*> listOf (elements varChar)) (not . isKeyword)
number = suchThat arbitrary (>= 0)
definition = (,) <$> identifier <*> arbitrary

instance Arbitrary CoreScDefn where
    arbitrary = ScDefn <$> identifier <*> resize 5 (listOf identifier) <*> arbitrary

instance Arbitrary CoreExpr where
    arbitrary
        = frequency [ (28, Var <$> identifier)
                    , (28, Num <$> number)
                    , (28, Constr <$> number <*> number)
                    , (5, App <$> arbitrary <*> arbitrary)
                    , (3, Let <$> arbitrary <*> resize 10 (listOf1 definition) <*> arbitrary)
                    , (3, Case <$> arbitrary <*> resize 10 (listOf1 arbitrary))
                    , (5, Lam <$> listOf1 identifier <*> arbitrary)
                    ]

instance Arbitrary CoreAlt where
    arbitrary = Alt <$> number <*> resize 5 (listOf identifier) <*> arbitrary

prop_printParse (NonEmpty prog)
    = case parse (printProgram prog) of
        Left  err -> False
        Right x   -> x == prog

runChecks = do
    check "printParse" prop_printParse

main = runChecks
