module ImpArbNTests where

import ImpArbN
import Data.Map

testsPt7 :: Bool
testsPt7 =  testDiv && testGCD && testPrime && testRefError1 && testRefError2 

--Initial States for divide
stDiv1 :: State
stDiv1 =  fromList [("dividend", 10), ("divisor", 5)]

stDiv2 :: State
stDiv2 =  fromList [("dividend", 5), ("divisor", 8)]

stDiv3 :: State
stDiv3 =  fromList [("dividend", 24), ("divisor", 3)]

stDiv4 :: State
stDiv4 =  fromList [("dividend", 81), ("divisor", 16)]

stDiv5 :: State
stDiv5 =  fromList [("dividend", 124), ("divisor", 56)]

--Returns true if all tests for divide are successful
testDiv :: Bool
testDiv = (((cEval stDiv1 divide) >>= (\state -> Data.Map.lookup "quotient" state)) == Just 2)
          && (((cEval stDiv1 divide) >>= (\state -> Data.Map.lookup "remainder" state)) == Just 0)
          && (((cEval stDiv2 divide) >>= (\state -> Data.Map.lookup "quotient" state)) == Just 0)
          && (((cEval stDiv2 divide) >>= (\state -> Data.Map.lookup "remainder" state)) == Just 5)
          && (((cEval stDiv3 divide) >>= (\state -> Data.Map.lookup "quotient" state)) == Just 8)
          && (((cEval stDiv3 divide) >>= (\state -> Data.Map.lookup "remainder" state)) == Just 0)
          && (((cEval stDiv4 divide) >>= (\state -> Data.Map.lookup "quotient" state)) == Just 5)
          && (((cEval stDiv4 divide) >>= (\state -> Data.Map.lookup "remainder" state)) == Just 1)
          && (((cEval stDiv5 divide) >>= (\state -> Data.Map.lookup "quotient" state)) == Just 2)
          && (((cEval stDiv5 divide) >>= (\state -> Data.Map.lookup "remainder" state)) == Just 12)

--Initial states for gcd
stGCD1 :: State
stGCD1 =  fromList [("dividend", 2), ("divisor", 4)]

stGCD2 :: State
stGCD2 =  fromList [("dividend", 7), ("divisor", 3)]

stGCD3 :: State
stGCD3 =  fromList [("dividend", 10), ("divisor", 15)]

stGCD4 :: State
stGCD4 =  fromList [("dividend", 64), ("divisor", 28)]

stGCD5 :: State
stGCD5 =  fromList [("dividend", 32), ("divisor", 11)]

--Returns true if all tests for gcd are successful
testGCD :: Bool
testGCD = (((cEval stGCD1 gCD) >>= (\state -> Data.Map.lookup "gcd" state)) == Just 2)
          && (((cEval stGCD2 gCD) >>= (\state -> Data.Map.lookup "gcd" state)) == Just 1)
          && (((cEval stGCD3 gCD) >>= (\state -> Data.Map.lookup "gcd" state)) == Just 5)
          && (((cEval stGCD4 gCD) >>= (\state -> Data.Map.lookup "gcd" state)) == Just 4)
          && (((cEval stGCD5 gCD) >>= (\state -> Data.Map.lookup "gcd" state)) == Just 1)

--Initial states for primality
stP1 :: State
stP1 =  fromList [("dividend", 3)]

stP2 :: State
stP2 =  fromList [("dividend", 8)]

stP3 :: State
stP3 =  fromList [("dividend", 11)]

stP4 :: State
stP4 =  fromList [("dividend", 26)]

stP5 :: State
stP5 =  fromList [("dividend", 31)]

--Returns true if all tests for primality are successful
testPrime :: Bool
testPrime = (((cEval stP1 primality) >>= (\state -> Data.Map.lookup "isPrime" state)) == Just 1)
            && (((cEval stP2 primality) >>= (\state -> Data.Map.lookup "isPrime" state)) == Just 0)
            && (((cEval stP3 primality) >>= (\state -> Data.Map.lookup "isPrime" state)) == Just 1)
            && (((cEval stP4 primality) >>= (\state -> Data.Map.lookup "isPrime" state)) == Just 0)
            && (((cEval stP5 primality) >>= (\state -> Data.Map.lookup "isPrime" state)) == Just 1)

--Intitial states for refError functions
stRef1 :: State
stRef1 =  fromList [("input", 2)]

stRef2 :: State
stRef2 =  fromList [("input", 5)]

stRef3 :: State
stRef3 =  fromList [("input", 14)]

stRef4 :: State
stRef4 =  fromList [("input", 27)]

stRef5 :: State
stRef5 =  fromList [("input", 31)]

--Returns true if all tests for refError1 are successful
testRefError1 :: Bool
testRefError1 = (cEval stRef1 refError1 == Nothing) && (cEval stRef2 refError1 == Nothing)
                && (cEval stRef3 refError1 == Nothing) && (cEval stRef4 refError1 == Nothing)
                && (cEval stRef5 refError1 == Nothing)

--Returns true if all tests for refError1 are successful
testRefError2 :: Bool
testRefError2 = (cEval stRef1 refError2 == Nothing) && (cEval stRef2 refError2 == Nothing)
                && (cEval stRef3 refError2 == Nothing) && (cEval stRef4 refError2 == Nothing)
                && (cEval stRef5 refError2 == Nothing)