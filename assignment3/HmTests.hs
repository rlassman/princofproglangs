module HmTests where

import ImpEx

--5

--Returns true if all tests for Part 5 are successful
testsPt5 :: Bool
testsPt5 = testHm && testHmC

--Initial States for Testing
st1 :: State
st1 X1 = 5
st1 X2 = 3
st1 X3 = 4
st1 X4 = 6
st1 X5 = 7
st1 X6 = 8

st2 :: State
st2 X1 = 8
st2 X2 = 3
st2 X3 = 4
st2 X4 = 6
st2 X5 = 7
st2 X6 = 8

st3 :: State
st3 X1 = 15
st3 X2 = 3
st3 X3 = 4
st3 X4 = 6
st3 X5 = 7
st3 X6 = 8

st4 :: State
st4 X1 = 23
st4 X2 = 3
st4 X3 = 4
st4 X4 = 6
st4 X5 = 7
st4 X6 = 8

st5 :: State
st5 X1 = 30
st5 X2 = 3
st5 X3 = 4
st5 X4 = 6
st5 X5 = 7
st5 X6 = 8

--Returns true if hm returns a DivByZero exception (shows hm doesn't work)
testHm :: Bool
testHm =  cEval st1 hm == Left DivBy0

--Returns True if all tests for corrected hm function (hmC) are successful
testHmC :: Bool
testHmC =  (fmap (\s -> s X2) (cEval st1 hmC) == return 2.189781) && (fmap (\s -> s X2) (cEval st2 hmC) == return 2.9434955)
           && (fmap (\s -> s X2) (cEval st3 hmC) == return 4.5204835) && (fmap (\s -> s X2) (cEval st4 hmC) == return 6.1591334)
           && (fmap (\s -> s X2) (cEval st5 hmC) == return 7.509411)