--Prints all of the tests for Big Step semantics
module Main where

import ImpMinusTests
import ImpExTests
import HmTests
import ImpArbNTests

main :: IO ()
main = do
         print "Tests for ImpMinus Big Step semantics:"
         print testsPt2
         print "Tests for ImpEx Big Step semantics:"
         print testsPt4
         print testsPt5
         print "Tests for ImpArbN Big Step semantics:"
         print testsPt7