module Main where

import Pt4Tests
import Pt7Tests
import ImpExtendBC

main :: IO ()
main = do
    print "Part 4, Big-Step Semantics Tests:"
    print testsPt4
    print "Part 7, Big-Step Semantics Tests:"
    print testsPt7

--Returns True if all tests for pt 4 are successful
testsPt4 :: Bool
testsPt4 =  testEx && testDiv && testFact && testFib && testGCD && testPrime

--Returns True if all tests for pt 7 are successful
testsPt7 :: Bool
testsPt7 = checkBOutside && checkBInside && checkBOuter && checkBInner 
            && checkCOutside && checkCInside && checkCOuter && checkCInner
            && checkBCLoop