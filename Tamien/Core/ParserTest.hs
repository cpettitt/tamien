{-# LANGUAGE TypeSynonymInstances #-}

{-
 - Tests for the Core language parser
 -}

module Tamien.Core.ParserTest where

import Tamien.Core

import Control.Applicative ((<$>), (<*>))
import Test.QuickCheck
import Text.Printf

varStart = ('_':['a'..'z'])
varChar  = varStart ++ ['0'..'9']

identifier = suchThat ((:) <$> elements varStart <*> listOf (elements varChar)) (not . isKeyword)
number = suchThat arbitrary (>= 0)
definition = (,) <$> identifier <*> arbitrary

instance Arbitrary CoreScDefn where
    arbitrary = ScDefn <$> identifier <*> listOf identifier <*> arbitrary

instance Arbitrary CoreExpr where
    arbitrary
        = frequency [ (330, Var <$> identifier)
                    , (330, Num <$> number)
                    , (330, Constr <$> number <*> number)
                    , (4, App <$> arbitrary <*> arbitrary)
                    , (2, Let <$> arbitrary <*> listOf1 definition <*> arbitrary)
                    , (2, Case <$> arbitrary <*> listOf1 arbitrary)
                    , (3, Lam <$> listOf1 identifier <*> arbitrary)
                    ]

instance Arbitrary CoreAlt where
    arbitrary = Alt <$> number <*> resize 5 (listOf identifier) <*> arbitrary

check s a = printf "%-25s: " s >> quickCheck a

prop_printParse (NonEmpty prog)
    = case parseProgram (printProgram prog) of
        Left  err -> False
        Right x   -> x == prog

runChecks = do
    check "printParse" prop_printParse
