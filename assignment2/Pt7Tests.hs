module Pt7Tests where

import ImpExtendBC
import Imp

--7

--initial states
state1 :: State
state1 X1 = 3
state1 X2 = 60
state1 X3 = 1
state1 X4 = 11
state1 X5 = 0
state1 X6 = 99

state2 :: State
state2 X1 = 5
state2 X2 = 4
state2 X3 = 14
state2 X4 = 2
state2 X5 = 3
state2 X6 = 5

--testBOutside final state
stateBOutside :: State
stateBOutside X1 = 22
stateBOutside X2 = 60
stateBOutside X3 = 17
stateBOutside X4 = 36
stateBOutside X5 = 0
stateBOutside X6 = 99

--Returns True if Break outside loop test is successful
checkBOutside :: Bool
checkBOutside =  printCEvalBC state1 testBOutside == stateBOutside

--testBInside final states
stateBInside1 :: State
stateBInside1 X1 = 42
stateBInside1 X2 = 12
stateBInside1 X3 = 10
stateBInside1 X4 = 11
stateBInside1 X5 = 0
stateBInside1 X6 = 99

stateBInside2 :: State
stateBInside2 X1 = 42
stateBInside2 X2 = 12
stateBInside2 X3 = 14
stateBInside2 X4 = 2
stateBInside2 X5 = 3
stateBInside2 X6 = 5

--Returns True if Break inside loop test is successful
checkBInside :: Bool
checkBInside =  printCEvalBC state1 testBInside == stateBInside1 
                && printCEvalBC state2 testBInside == stateBInside2

--testBOuter final state
stateBOuter :: State
stateBOuter X1 = 1
stateBOuter X2 = 60
stateBOuter X3 = 16
stateBOuter X4 = 11
stateBOuter X5 = 0
stateBOuter X6 = 99

--Returns True if Break in outer loop test is successful
checkBOuter :: Bool
checkBOuter =  printCEvalBC state1 testBOuter == stateBOuter

--testBInner final states
stateBInner1 :: State
stateBInner1 X1 = 6
stateBInner1 X2 = 57
stateBInner1 X3 = 175
stateBInner1 X4 = 11
stateBInner1 X5 = 21
stateBInner1 X6 = 99

stateBInner2 :: State
stateBInner2 X1 = 6
stateBInner2 X2 = 3
stateBInner2 X3 = 17
stateBInner2 X4 = 2
stateBInner2 X5 = 21
stateBInner2 X6 = 5

--Returns True if Break in inner loop test is successful
checkBInner :: Bool
checkBInner=  printCEvalBC state1 testBInner == stateBInner1
              && printCEvalBC state2 testBInner == stateBInner2

--testCOutside final state
stateCOutside :: State
stateCOutside X1 = 14
stateCOutside X2 = 60
stateCOutside X3 = 21
stateCOutside X4 = 7
stateCOutside X5 = 0
stateCOutside X6 = 99

--Returns True if Continue outside loop test is successful
checkCOutside :: Bool
checkCOutside =  printCEvalBC state1 testCOutside == stateCOutside

--testCInside final state
stateCInside :: State
stateCInside X1 = -1
stateCInside X2 = 2
stateCInside X3 = 1
stateCInside X4 = 11
stateCInside X5 = 2
stateCInside X6 = -1

--Returns True if Continue inside loop test is successful
checkCInside :: Bool
checkCInside =  printCEvalBC state1 testCInside == stateCInside

--testCOuter final states
stateCOuter1 :: State
stateCOuter1 X1 = 3
stateCOuter1 X2 = 245760
stateCOuter1 X3 = 1
stateCOuter1 X4 = 11
stateCOuter1 X5 = 6
stateCOuter1 X6 = 111

stateCOuter2 :: State
stateCOuter2 X1 = 3
stateCOuter2 X2 = 64
stateCOuter2 X3 = 14
stateCOuter2 X4 = 2
stateCOuter2 X5 = 6
stateCOuter2 X6 = 111

--Returns True if Continue in outer loop test is successful
checkCOuter :: Bool
checkCOuter =  printCEvalBC state1 testCOuter == stateCOuter1
               && printCEvalBC state2 testCOuter == stateCOuter2

--testCInner final state
stateCInner :: State
stateCInner X1 = 12
stateCInner X2 = 15
stateCInner X3 = 1
stateCInner X4 = 9
stateCInner X5 = 0
stateCInner X6 = 99

--Returns True if Continue in inner loop test is successful
checkCInner :: Bool
checkCInner=  printCEvalBC state1 testCInner == stateCInner

--testBCLoop final state
stateBCLoop :: State
stateBCLoop X1 = 2
stateBCLoop X2 = 60
stateBCLoop X3 = 7
stateBCLoop X4 = 66
stateBCLoop X5 = 0
stateBCLoop X6 = 99

--Returns True if Break and Continue in nested loop test is successful
checkBCLoop :: Bool
checkBCLoop =  printCEvalBC state1 testBCLoop == stateBCLoop