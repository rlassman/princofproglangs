module Pt4Tests where

import Imp

--4

--Tests for exponentiate 
stateEx1I :: State --test 1 intial state
stateEx1I X1 = 8
stateEx1I X2 = 0
stateEx1I X3 = 2
stateEx1I X4 = 2
stateEx1I X5 = 2
stateEx1I X6 = 2

stateEx1F :: State --test 1 final state
stateEx1F X1 = 8
stateEx1F X2 = 0
stateEx1F X3 = 1
stateEx1F X4 = 2
stateEx1F X5 = 2
stateEx1F X6 = 2

stateEx2I :: State --test 2 intial state
stateEx2I X1 = 2
stateEx2I X2 = 5
stateEx2I X3 = 0
stateEx2I X4 = 1
stateEx2I X5 = 2
stateEx2I X6 = 3

stateEx2F :: State --test 2 final state
stateEx2F X1 = 2
stateEx2F X2 = 0
stateEx2F X3 = 32
stateEx2F X4 = 1
stateEx2F X5 = 2
stateEx2F X6 = 3

stateEx3I :: State --test 3 initial state
stateEx3I X1 = 4
stateEx3I X2 = 10
stateEx3I X3 = 9
stateEx3I X4 = 10
stateEx3I X5 = 21
stateEx3I X6 = 5

stateEx3F :: State --test 3 final state
stateEx3F X1 = 4
stateEx3F X2 = 0
stateEx3F X3 = 1048576
stateEx3F X4 = 10
stateEx3F X5 = 21
stateEx3F X6 = 5

stateEx4I :: State ----test 4 intial state
stateEx4I X1 = 14
stateEx4I X2 = 7
stateEx4I X3 = 1
stateEx4I X4 = 123
stateEx4I X5 = 0
stateEx4I X6 = 120

stateEx4F :: State --test 4 final state
stateEx4F X1 = 14
stateEx4F X2 = 0
stateEx4F X3 = 105413504
stateEx4F X4 = 123
stateEx4F X5 = 0
stateEx4F X6 = 120

stateEx5I :: State --test 5 intial state
stateEx5I X1 = 27
stateEx5I X2 = 8
stateEx5I X3 = 66
stateEx5I X4 = 81
stateEx5I X5 = 2335
stateEx5I X6 = 12890

stateEx5F :: State --test 5 final state
stateEx5F X1 = 27
stateEx5F X2 = 0
stateEx5F X3 = 282429536481
stateEx5F X4 = 81
stateEx5F X5 = 2335
stateEx5F X6 = 12890

stateEx6I :: State --test 6 intial state
stateEx6I X1 = 3
stateEx6I X2 = 25
stateEx6I X3 = 66
stateEx6I X4 = 8
stateEx6I X5 = 2
stateEx6I X6 = 1

stateEx6F :: State --test 6 final state
stateEx6F X1 = 3
stateEx6F X2 = 0
stateEx6F X3 = 847288609443
stateEx6F X4 = 8
stateEx6F X5 = 2
stateEx6F X6 = 1

--Returns True if all exponentiate tests are successful
testEx :: Bool
testEx =  (cEval stateEx1I exponentiate == stateEx1F) && (cEval stateEx2I exponentiate == stateEx2F)
          && (cEval stateEx3I exponentiate == stateEx3F) && (cEval stateEx4I exponentiate == stateEx4F)
          && (cEval stateEx5I exponentiate == stateEx5F) && (cEval stateEx6I exponentiate == stateEx6F)

--Tests for divide
stateDiv1I :: State --test 1 intial state
stateDiv1I X1 = 3
stateDiv1I X2 = 7
stateDiv1I X3 = 1
stateDiv1I X4 = 2
stateDiv1I X5 = 3
stateDiv1I X6 = 4

stateDiv1F :: State --test 1 final state
stateDiv1F X1 = 3
stateDiv1F X2 = 7
stateDiv1F X3 = 0
stateDiv1F X4 = 3
stateDiv1F X5 = 3
stateDiv1F X6 = 4

stateDiv2I :: State --test 2 intial state
stateDiv2I X1 = 10
stateDiv2I X2 = 5
stateDiv2I X3 = 12
stateDiv2I X4 = 45
stateDiv2I X5 = 30
stateDiv2I X6 = 2

stateDiv2F :: State --test 2 final state
stateDiv2F X1 = 0
stateDiv2F X2 = 5
stateDiv2F X3 = 2
stateDiv2F X4 = 0
stateDiv2F X5 = 30
stateDiv2F X6 = 2

stateDiv3I :: State --test 3 intial state
stateDiv3I X1 = 64
stateDiv3I X2 = 4
stateDiv3I X3 = 0
stateDiv3I X4 = 111
stateDiv3I X5 = 467
stateDiv3I X6 = 52

stateDiv3F :: State --test 3 final state
stateDiv3F X1 = 0
stateDiv3F X2 = 4
stateDiv3F X3 = 16
stateDiv3F X4 = 0
stateDiv3F X5 = 467
stateDiv3F X6 = 52

stateDiv4I :: State --test 4 intial state
stateDiv4I X1 = 167
stateDiv4I X2 = 31
stateDiv4I X3 = 17
stateDiv4I X4 = 0
stateDiv4I X5 = 47
stateDiv4I X6 = 15

stateDiv4F :: State --test 4 final state
stateDiv4F X1 = 12
stateDiv4F X2 = 31
stateDiv4F X3 = 5
stateDiv4F X4 = 12
stateDiv4F X5 = 47
stateDiv4F X6 = 15

stateDiv5I :: State --test 5 intial state
stateDiv5I X1 = 945
stateDiv5I X2 = 192
stateDiv5I X3 = 24
stateDiv5I X4 = 25
stateDiv5I X5 = 233
stateDiv5I X6 = 12918

stateDiv5F :: State --test 5 final state
stateDiv5F X1 = 177
stateDiv5F X2 = 192
stateDiv5F X3 = 4
stateDiv5F X4 = 177
stateDiv5F X5 = 233
stateDiv5F X6 = 12918

--Returns True if all divide tests are successful
testDiv :: Bool
testDiv = (cEval stateDiv1I divide == stateDiv1F) && (cEval stateDiv2I divide == stateDiv2F)
          && (cEval stateDiv3I divide == stateDiv3F) && (cEval stateDiv4I divide == stateDiv4F)
          && (cEval stateDiv5I divide == stateDiv5F)

--Tests for factorial
stateFact1I :: State --test 1 intial state
stateFact1I X1 = 0
stateFact1I X2 = 7
stateFact1I X3 = 55
stateFact1I X4 = 23
stateFact1I X5 = 3
stateFact1I X6 = 4

stateFact1F :: State --test 1 final state
stateFact1F X1 = 1
stateFact1F X2 = 1
stateFact1F X3 = 55
stateFact1F X4 = 23
stateFact1F X5 = 3
stateFact1F X6 = 4

stateFact2I :: State --test 2 intial state
stateFact2I X1 = 3
stateFact2I X2 = 192
stateFact2I X3 = 4
stateFact2I X4 = 25
stateFact2I X5 = 2
stateFact2I X6 = 1

stateFact2F :: State --test 2 final state
stateFact2F X1 = 6
stateFact2F X2 = 6
stateFact2F X3 = 4
stateFact2F X4 = 25
stateFact2F X5 = 2
stateFact2F X6 = 1

stateFact3I :: State --test 3 intial state
stateFact3I X1 = 6
stateFact3I X2 = 5
stateFact3I X3 = 132
stateFact3I X4 = 67
stateFact3I X5 = 3
stateFact3I X6 = 28

stateFact3F :: State --test 3 final state
stateFact3F X1 = 720
stateFact3F X2 = 720
stateFact3F X3 = 132
stateFact3F X4 = 67
stateFact3F X5 = 3
stateFact3F X6 = 28

stateFact4I :: State --test 4 intial state
stateFact4I X1 = 10
stateFact4I X2 = 34
stateFact4I X3 = 0
stateFact4I X4 = 11
stateFact4I X5 = 55
stateFact4I X6 = 55

stateFact4F :: State --test 4 final state
stateFact4F X1 = 3628800
stateFact4F X2 = 3628800
stateFact4F X3 = 0
stateFact4F X4 = 11
stateFact4F X5 = 55
stateFact4F X6 = 55

stateFact5I :: State --test 5 intial state
stateFact5I X1 = 12
stateFact5I X2 = 0
stateFact5I X3 = 54
stateFact5I X4 = 333
stateFact5I X5 = 4
stateFact5I X6 = 5

stateFact5F :: State --test 5 final state
stateFact5F X1 = 479001600
stateFact5F X2 = 479001600
stateFact5F X3 = 54
stateFact5F X4 = 333
stateFact5F X5 = 4
stateFact5F X6 = 5

--Returns True if all factorial tests are successful
testFact :: Bool
testFact = (cEval stateFact1I factorial == stateFact1F) && (cEval stateFact2I factorial == stateFact2F)
          && (cEval stateFact3I factorial == stateFact3F) && (cEval stateFact4I factorial == stateFact4F)
          && (cEval stateFact5I factorial == stateFact5F)

--Tests for fib
stateFib1I :: State --test 1 intial state
stateFib1I X1 = 1
stateFib1I X2 = 2
stateFib1I X3 = 0
stateFib1I X4 = 4
stateFib1I X5 = 8
stateFib1I X6 = 12

stateFib1F :: State --test 1 final state
stateFib1F X1 = 1
stateFib1F X2 = 2
stateFib1F X3 = 1
stateFib1F X4 = 1
stateFib1F X5 = 8
stateFib1F X6 = 12

stateFib2I :: State --test 2 intial state
stateFib2I X1 = 6
stateFib2I X2 = 5
stateFib2I X3 = 82
stateFib2I X4 = 6
stateFib2I X5 = 2
stateFib2I X6 = 1

stateFib2F :: State --test 2 final state
stateFib2F X1 = 13
stateFib2F X2 = 21
stateFib2F X3 = 13
stateFib2F X4 = 13
stateFib2F X5 = 2
stateFib2F X6 = 1

stateFib3I :: State --test 3 intial state
stateFib3I X1 = 14
stateFib3I X2 = 34
stateFib3I X3 = 0
stateFib3I X4 = 36
stateFib3I X5 = 55
stateFib3I X6 = 54

stateFib3F :: State --test 3 final state
stateFib3F X1 = 610
stateFib3F X2 = 987
stateFib3F X3 = 610
stateFib3F X4 = 610
stateFib3F X5 = 55
stateFib3F X6 = 54

stateFib4I :: State --test 4 intial state
stateFib4I X1 = 21
stateFib4I X2 = 1
stateFib4I X3 = 2
stateFib4I X4 = 3
stateFib4I X5 = 4
stateFib4I X6 = 5

stateFib4F :: State --test 4 final state
stateFib4F X1 = 17711
stateFib4F X2 = 28657
stateFib4F X3 = 17711
stateFib4F X4 = 17711
stateFib4F X5 = 4
stateFib4F X6 = 5

stateFib5I :: State --test 5 intial state
stateFib5I X1 = 25
stateFib5I X2 = 192
stateFib5I X3 = 457
stateFib5I X4 = 25
stateFib5I X5 = 0
stateFib5I X6 = 1

stateFib5F :: State --test 5 final state
stateFib5F X1 = 121393
stateFib5F X2 = 196418
stateFib5F X3 = 121393
stateFib5F X4 = 121393
stateFib5F X5 = 0
stateFib5F X6 = 1

--Returns True if all fib tests are successful
testFib :: Bool
testFib = (cEval stateFib1I fib == stateFib1F) && (cEval stateFib2I fib == stateFib2F)
          && (cEval stateFib3I fib == stateFib3F) && (cEval stateFib4I fib == stateFib4F)
          && (cEval stateFib5I fib == stateFib5F)

--Tests for gCD
stateGCD1I :: State --test 1 intial state
stateGCD1I X1 = 2
stateGCD1I X2 = 4
stateGCD1I X3 = 1
stateGCD1I X4 = 3
stateGCD1I X5 = 8
stateGCD1I X6 = 7

stateGCD1F :: State --test 1 final state
stateGCD1F X1 = 2
stateGCD1F X2 = 0
stateGCD1F X3 = 2
stateGCD1F X4 = 0
stateGCD1F X5 = 8
stateGCD1F X6 = 7

stateGCD2I :: State --test 2 intial state
stateGCD2I X1 = 7
stateGCD2I X2 = 3
stateGCD2I X3 = 8
stateGCD2I X4 = 6
stateGCD2I X5 = 23
stateGCD2I X6 = 11

stateGCD2F :: State --test 2 final state
stateGCD2F X1 = 1
stateGCD2F X2 = 0
stateGCD2F X3 = 1
stateGCD2F X4 = 0
stateGCD2F X5 = 23
stateGCD2F X6 = 11

stateGCD3I :: State --test 3 intial state
stateGCD3I X1 = 10
stateGCD3I X2 = 15
stateGCD3I X3 = 2
stateGCD3I X4 = 10
stateGCD3I X5 = 5
stateGCD3I X6 = 6

stateGCD3F :: State --test 3 final state
stateGCD3F X1 = 5
stateGCD3F X2 = 0
stateGCD3F X3 = 5
stateGCD3F X4 = 0
stateGCD3F X5 = 5
stateGCD3F X6 = 6

stateGCD4I :: State --test 4 intial state
stateGCD4I X1 = 64
stateGCD4I X2 = 28
stateGCD4I X3 = 23
stateGCD4I X4 = 7
stateGCD4I X5 = 2
stateGCD4I X6 = 120

stateGCD4F :: State --test 4 final state
stateGCD4F X1 = 4
stateGCD4F X2 = 0
stateGCD4F X3 = 4
stateGCD4F X4 = 0
stateGCD4F X5 = 2
stateGCD4F X6 = 120

stateGCD5I :: State --test 5 intial state
stateGCD5I X1 = 32
stateGCD5I X2 = 11
stateGCD5I X3 = 47
stateGCD5I X4 = 25
stateGCD5I X5 = 0
stateGCD5I X6 = 1

stateGCD5F :: State --test 5 final state
stateGCD5F X1 = 1
stateGCD5F X2 = 0
stateGCD5F X3 = 1
stateGCD5F X4 = 0
stateGCD5F X5 = 0
stateGCD5F X6 = 1

--Returns True if all gcd tests are successful
testGCD :: Bool
testGCD = (cEval stateGCD1I gCD == stateGCD1F) && (cEval stateGCD2I gCD == stateGCD2F)
          && (cEval stateGCD3I gCD == stateGCD3F) && (cEval stateGCD4I gCD == stateGCD4F)
          && (cEval stateGCD5I gCD == stateGCD5F)

--Tests for primality
statePrime0I :: State --test 1 intial state
statePrime0I X1 = 0
statePrime0I X2 = 1
statePrime0I X3 = 5
statePrime0I X4 = 6
statePrime0I X5 = 8
statePrime0I X6 = 9

statePrime0F :: State --test 1 final state
statePrime0F X1 = 0
statePrime0F X2 = -1
statePrime0F X3 = 5
statePrime0F X4 = 6
statePrime0F X5 = 0
statePrime0F X6 = 0


statePrime1I :: State --test 1 intial state
statePrime1I X1 = 3
statePrime1I X2 = 14
statePrime1I X3 = 5
statePrime1I X4 = 6
statePrime1I X5 = 8
statePrime1I X6 = 9

statePrime1F :: State --test 1 final state
statePrime1F X1 = 3
statePrime1F X2 = 1
statePrime1F X3 = 1
statePrime1F X4 = 1
statePrime1F X5 = 1
statePrime1F X6 = 3

statePrime2I :: State --test 2 intial state
statePrime2I X1 = 8
statePrime2I X2 = 0
statePrime2I X3 = 19
statePrime2I X4 = 6
statePrime2I X5 = 23
statePrime2I X6 = 11

statePrime2F :: State --test 2 final state
statePrime2F X1 = 8
statePrime2F X2 = 3
statePrime2F X3 = 2
statePrime2F X4 = 0
statePrime2F X5 = 0
statePrime2F X6 = 8

statePrime3I :: State --test 3 intial state
statePrime3I X1 = 11
statePrime3I X2 = 1
statePrime3I X3 = 23
statePrime3I X4 = 10
statePrime3I X5 = 5
statePrime3I X6 = 6

statePrime3F :: State --test 3 final state
statePrime3F X1 = 11
statePrime3F X2 = 1
statePrime3F X3 = 5
statePrime3F X4 = 1
statePrime3F X5 = 1
statePrime3F X6 = 11

statePrime4I :: State --test 4 intial state
statePrime4I X1 = 26
statePrime4I X2 = 6
statePrime4I X3 = 23
statePrime4I X4 = 7
statePrime4I X5 = 22
statePrime4I X6 = 20

statePrime4F :: State --test 4 final state
statePrime4F X1 = 26
statePrime4F X2 = 12
statePrime4F X3 = 2
statePrime4F X4 = 0
statePrime4F X5 = 0
statePrime4F X6 = 26

statePrime5I :: State --test 5 intial state
statePrime5I X1 = 31
statePrime5I X2 = 11
statePrime5I X3 = 47
statePrime5I X4 = 25
statePrime5I X5 = 3
statePrime5I X6 = 1

statePrime5F :: State --test 5 final state
statePrime5F X1 = 31
statePrime5F X2 = 1
statePrime5F X3 = 15
statePrime5F X4 = 1
statePrime5F X5 = 1
statePrime5F X6 = 31

--Returns True if all gcd tests are successful
testPrime :: Bool
testPrime = (cEval statePrime0I primality == statePrime0F) && (cEval statePrime1I primality == statePrime1F) 
          && (cEval statePrime2I primality == statePrime2F) && (cEval statePrime3I primality == statePrime3F) 
          && (cEval statePrime4I primality == statePrime4F) && (cEval statePrime5I primality == statePrime5F)
