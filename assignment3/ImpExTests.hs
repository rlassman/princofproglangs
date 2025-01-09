module ImpExTests where

import ImpEx

--4

--Returns true if all tests for ImpEx big step semantics are successful
testsPt4 :: Bool
testsPt4 =  testIsInt && testFloor && testCeiling && testDiv && testPerfSq

--Initial states for testing
st1 :: State
st1 X1 = 0
st1 X2 = 6
st1 X3 = 3
st1 X4 = 4
st1 X5 = 5
st1 X6 = 7

st2 :: State
st2 X1 = -6
st2 X2 = 2
st2 X3 = 3
st2 X4 = 4
st2 X5 = 5
st2 X6 = 6

st3 :: State
st3 X1 = 17.63
st3 X2 = 1
st3 X3 = 2
st3 X4 = 4
st3 X5 = 5
st3 X6 = 6

st4 :: State
st4 X1 = -20.109
st4 X2 = 2
st4 X3 = 3
st4 X4 = 4
st4 X5 = 5
st4 X6 = 6

st5 :: State
st5 X1 = 38
st5 X2 = 2
st5 X3 = 3
st5 X4 = 4
st5 X5 = 5
st5 X6 = 6

st6 :: State
st6 X1 = 24.99
st6 X2 = 2
st6 X3 = 3
st6 X4 = 4
st6 X5 = 5
st6 X6 = 6

st7 :: State
st7 X1 = -8.33
st7 X2 = 2
st7 X3 = 3
st7 X4 = 4
st7 X5 = 5
st7 X6 = 6

st8 :: State
st8 X1 = 11
st8 X2 = 2
st8 X3 = 14
st8 X4 = 3
st8 X5 = 7
st8 X6 = 6

st9 :: State
st9 X1 = 11
st9 X2 = 2
st9 X3 = 14
st9 X4 = 10
st9 X5 = 5
st9 X6 = 6

st10 :: State
st10 X1 = 11
st10 X2 = 2
st10 X3 = 14
st10 X4 = 27
st10 X5 = 8
st10 X6 = 6

st11 :: State
st11 X1 = 3
st11 X2 = 2
st11 X3 = 14
st11 X4 = 100
st11 X5 = 22
st11 X6 = 6

st12 :: State
st12 X1 = 11
st12 X2 = 2
st12 X3 = 14
st12 X4 = 17
st12 X5 = 0
st12 X6 = 6

st13 :: State
st13 X1 = 11
st13 X2 = 2
st13 X3 = 14
st13 X4 = 36
st13 X5 = 0
st13 X6 = 6

st14 :: State
st14 X1 = 11
st14 X2 = 2
st14 X3 = 14
st14 X4 = -4
st14 X5 = 0
st14 X6 = 6

--Returns true if all tests for isInt big step semantics are successful
testIsInt :: Bool
testIsInt = (fmap (\s -> s X2) (cEval st1 isInt) == return 1) && (fmap (\s -> s X2) (cEval st2 isInt) == return 1)
            && (fmap (\s -> s X2) (cEval st3 isInt) == return 0) && (fmap (\s -> s X2) (cEval st4 isInt) == return 0)
            && (fmap (\s -> s X2) (cEval st5 isInt) == return 1)

--Returns true if all tests for floor big step semantics are successful
testFloor :: Bool
testFloor =  (fmap (\s -> s X2) (cEval st2 ImpEx.floor) == return (-6)) && (fmap (\s -> s X2) (cEval st3 ImpEx.floor) == return 17)
             && (fmap (\s -> s X2) (cEval st4 ImpEx.floor) == return (-21)) && (fmap (\s -> s X2) (cEval st5 ImpEx.floor) == return 38)
             && (fmap (\s -> s X2) (cEval st6 ImpEx.floor) == return 24)

--Returns true if all tests for ceiling big step semantics are successful
testCeiling :: Bool
testCeiling = (fmap (\s -> s X2) (cEval st7 ImpEx.ceiling) == return (-8)) && (fmap (\s -> s X2) (cEval st3 ImpEx.ceiling) == return 18)
              && (fmap (\s -> s X2) (cEval st4 ImpEx.ceiling) == return (-20)) && (fmap (\s -> s X2) (cEval st5 ImpEx.ceiling) == return 38)
              && (fmap (\s -> s X2) (cEval st6 ImpEx.ceiling) == return 25)

--Returns true if all tests for intDiv big step semantics are successful
testDiv :: Bool
testDiv = (fmap (\s -> s X2) (cEval st8 intDiv) == return 0) && (fmap (\s -> s X3) (cEval st8 intDiv) == return 3)
          && (fmap (\s -> s X2) (cEval st9 intDiv) == return 2) && (fmap (\s -> s X3) (cEval st9 intDiv) == return 0)
          && (fmap (\s -> s X2) (cEval st10 intDiv) == return 3) && (fmap (\s -> s X3) (cEval st10 intDiv) == return 3)
          && (fmap (\s -> s X2) (cEval st11 intDiv) == return 4) && (fmap (\s -> s X3) (cEval st11 intDiv) == return 12)
          && cEval st12 intDiv == Left DivBy0

--Returns true if all tests for perfSq big step semantics are successful
testPerfSq :: Bool
testPerfSq = (fmap (\s -> s X2) (cEval st8 perfSq) == return 0) && (fmap (\s -> s X2) (cEval st10 perfSq) == return 0)
             && (fmap (\s -> s X2) (cEval st11 perfSq) == return 1) && (fmap (\s -> s X2) (cEval st13 perfSq) == return 1)
             && cEval st14 perfSq == Left SqrtNeg