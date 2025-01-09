module ImpMinusTests where

import ImpMinus

--2

--Returns true if all tests for ImpMinus are successful
testsPt2 :: Bool
testsPt2 =  testAdd && testSub && testMult

--Initial states for add and subtract
st1 :: State
st1 X1 = 16
st1 X2 = 6
st1 X3 = 3
st1 X4 = 4
st1 X5 = 5
st1 X6 = 6

st2 :: State
st2 X1 = 11
st2 X2 = 15
st2 X3 = 3
st2 X4 = 4
st2 X5 = 5
st2 X6 = 6

st3 :: State
st3 X1 = -18
st3 X2 = -7
st3 X3 = 3
st3 X4 = 4
st3 X5 = 5
st3 X6 = 6

st4 :: State
st4 X1 = -22
st4 X2 = 11
st4 X3 = 3
st4 X4 = 4
st4 X5 = 5
st4 X6 = 6

st5 :: State
st5 X1 = 32
st5 X2 = -25
st5 X3 = 3
st5 X4 = 4
st5 X5 = 5
st5 X6 = 6

--Add final states
stA1 :: State
stA1 X1 = 22
stA1 X2 = 0
stA1 X3 = 1
stA1 X4 = 4
stA1 X5 = 5
stA1 X6 = 6

stA2 :: State
stA2 X1 = 26
stA2 X2 = 0
stA2 X3 = 1
stA2 X4 = 4
stA2 X5 = 5
stA2 X6 = 6

stA3 :: State
stA3 X1 = -25
stA3 X2 = 0
stA3 X3 = 0
stA3 X4 = 4
stA3 X5 = 5
stA3 X6 = 6

stA4 :: State
stA4 X1 = -11
stA4 X2 = 0
stA4 X3 = 1
stA4 X4 = 4
stA4 X5 = 5
stA4 X6 = 6

stA5 :: State
stA5 X1 = 7
stA5 X2 = 0
stA5 X3 = 0
stA5 X4 = 4
stA5 X5 = 5
stA5 X6 = 6

--Checks if all add tests are successful
testAdd :: Bool
testAdd =  cEval st1 add == stA1 && cEval st2 add == stA2 && cEval st3 add == stA3
           && cEval st4 add == stA4 && cEval st5 add == stA5

--Subtract final states
stS1 :: State
stS1 X1 = 10
stS1 X2 = 0
stS1 X3 = 1
stS1 X4 = 4
stS1 X5 = 5
stS1 X6 = 6

stS2 :: State
stS2 X1 = -4
stS2 X2 = 0
stS2 X3 = 1
stS2 X4 = 4
stS2 X5 = 5
stS2 X6 = 6

stS3 :: State
stS3 X1 = -11
stS3 X2 = 0
stS3 X3 = 0
stS3 X4 = 4
stS3 X5 = 5
stS3 X6 = 6

stS4 :: State
stS4 X1 = -33
stS4 X2 = 0
stS4 X3 = 1
stS4 X4 = 4
stS4 X5 = 5
stS4 X6 = 6

stS5 :: State
stS5 X1 = 57
stS5 X2 = 0
stS5 X3 = 0
stS5 X4 = 4
stS5 X5 = 5
stS5 X6 = 6

--Checks if all subtract tests are successful
testSub :: Bool
testSub =  cEval st1 sub == stS1 && cEval st2 sub == stS2 && cEval st3 sub == stS3
           && cEval st4 sub == stS4 && cEval st5 sub == stS5

--Mult initial states
stM1I :: State
stM1I X1 = 1
stM1I X2 = 12
stM1I X3 = 3
stM1I X4 = 0
stM1I X5 = 5
stM1I X6 = 6

stM2I :: State
stM2I X1 = 1
stM2I X2 = 8
stM2I X3 = 3
stM2I X4 = 4
stM2I X5 = 5
stM2I X6 = 6

stM3I :: State
stM3I X1 = 1
stM3I X2 = -7
stM3I X3 = 3
stM3I X4 = 11
stM3I X5 = 5
stM3I X6 = 6

stM4I :: State
stM4I X1 = 1
stM4I X2 = 20
stM4I X3 = 3
stM4I X4 = -13
stM4I X5 = 5
stM4I X6 = 6

stM5I :: State
stM5I X1 = 1
stM5I X2 = -9
stM5I X3 = 3
stM5I X4 = -36
stM5I X5 = 5
stM5I X6 = 6

--Mult final states
stM1F :: State
stM1F X1 = 0
stM1F X2 = 12
stM1F X3 = 3
stM1F X4 = 0
stM1F X5 = 1
stM1F X6 = 12

stM2F :: State
stM2F X1 = 32
stM2F X2 = 8
stM2F X3 = 1
stM2F X4 = 0
stM2F X5 = 1
stM2F X6 = 8

stM3F :: State
stM3F X1 = -77
stM3F X2 = -7
stM3F X3 = 0
stM3F X4 = 0
stM3F X5 = 1
stM3F X6 = -7

stM4F :: State
stM4F X1 = -260
stM4F X2 = 20
stM4F X3 = 1
stM4F X4 = 0
stM4F X5 = 0
stM4F X6 = 20

stM5F :: State
stM5F X1 = 324
stM5F X2 = -9
stM5F X3 = 0
stM5F X4 = 0
stM5F X5 = 0
stM5F X6 = -9

--Checks if all multiplication tests are successful
testMult :: Bool
testMult =  cEval stM1I mult == stM1F && cEval stM2I mult == stM2F && cEval stM3I mult == stM3F
           && cEval stM4I mult == stM4F && cEval stM5I mult == stM5F