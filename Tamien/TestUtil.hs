module Tamien.TestUtil where

import Test.QuickCheck
import Text.Printf

check s a = printf "%-25s: " s >> quickCheck a
