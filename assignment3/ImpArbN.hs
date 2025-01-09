module ImpArbN where

import Data.Map

--Defines State
type State = Map String Int

--Formats printing for state
instance {-# OVERLAPPING #-} Show State where
    show :: State -> String
    show    s     =  show (toList s)

--Defines a natural number
data Nat where
    Z :: Nat
    S :: Nat -> Nat
    deriving Show

--6

--Inductive definition for arithmetic expressions
data AExp where
    AInt  :: Int -> AExp
    AVar  :: String -> AExp
    AAdd  :: AExp -> AExp -> AExp
    ASub  :: AExp -> AExp -> AExp
    AMult :: AExp -> AExp -> AExp

--Inductive definition for boolean expressions
data BExp where
    BTrue  :: Bool -> BExp
    BFalse :: Bool -> BExp
    BLeq   :: AExp -> AExp -> BExp
    BNot   :: BExp -> BExp
    BAnd   :: BExp -> BExp -> BExp

--Inductive definition for commands
data Com where
    Skip   :: Com
    Assign :: String -> AExp -> Com
    Seq    :: Com -> Com -> Com
    If     :: BExp -> Com -> Com -> Com
    While  :: BExp -> Com -> Com

--Evaluates arithmatic expressions
aEval :: State -> AExp -> Maybe Int
aEval    st     (AInt n)     =  return n 
aEval    st     (AVar x)     =  Data.Map.lookup x st 
aEval    st     (AAdd a1 a2) =  do
                                  n1 <- aEval st a1
                                  n2 <- aEval st a2
                                  return (n1 + n2)
aEval    st     (ASub a1 a2) =  do
                                  n1 <- aEval st a1
                                  n2 <- aEval st a2
                                  return (n1 - n2)
aEval    st     (AMult a1 a2) =  do
                                  n1 <- aEval st a1
                                  n2 <- aEval st a2
                                  return (n1 * n2)

--Evaluates boolean expressions
bEval :: State -> BExp -> Maybe Bool
bEval    st     (BTrue True)   =  return True
bEval    st     (BFalse False) =  return False
bEval    st     (BLeq a1 a2)   =  do
                                    n1 <- aEval st a1
                                    n2 <- aEval st a2
                                    if (n1 <= n2) then return True else return False
bEval    st     (BAnd b1 b2)   =  do
                                    bv1 <- bEval st b1
                                    bv2 <- bEval st b2
                                    return (bv1 && bv2)
bEval    st     (BNot b)       =  do
                                    bv <- bEval st b
                                    return (not bv)

--Big Step Semantics
cEval :: State -> Com -> Maybe State
cEval    st    Skip        =  return st
cEval    st   (Assign x a) =  do
                                n <- aEval st a
                                return (insert x n st)
cEval    st   (Seq c1 c2)  =  do
                                st1 <- cEval st c1
                                cEval st1 c2
cEval    st   (If b c1 c2) =  do
                                bv <- bEval st b
                                if bv then cEval st c1 else cEval st c2
cEval    st   (While b c)  =  auxWhile st st b c where
        auxWhile :: State -> State -> BExp -> Com -> Maybe State
        auxWhile    gst      lst      b       c   =  do
                                                    bv <- bEval gst b
                                                    if bv then 
                                                        (do
                                                            st' <- cEval lst c 
                                                            let gst' = intersection st' gst in auxWhile gst' st' b c)
                                                    else return gst
                                            
--Small Step Semantics
cEvalMulti :: State -> Com -> Nat -> Maybe (State, Maybe Com, Nat)
cEvalMulti    st    c            Z    =  return (st, Just c, Z)
cEvalMulti    st    Skip        (S n) =  return (st, Nothing, n)
cEvalMulti    st   (Assign x a) (S n) =  do
                                           n' <- aEval st a
                                           return (insert x n' st, Nothing, n)
cEvalMulti    st   (Seq c1 c2)  (S n) =  do
                                           scn <- cEvalMulti st c1 (S n)
                                           case scn of
                                              (st', Nothing, m)  -> cEvalMulti st' c2 m 
                                              (st', Just c1', m) -> return (st', Just (Seq c1' c2), m)
cEvalMulti    st   (If b c1 c2) (S n) =  do
                                           bv <- bEval st b 
                                           if bv then cEvalMulti st c1 (S n) else cEvalMulti st c2 (S n)
cEvalMulti    st   (While b c)  (S n) =  auxWhile st st b c (S n) where
        auxWhile :: State -> State -> BExp -> Com -> Nat -> Maybe (State, Maybe Com, Nat)
        auxWhile    gst      lst      b       c     Z    =  return (gst, Just (While b c), Z)
        auxWhile    gst      lst      b       c    (S m) =  do
                                                            bv <- bEval gst b 
                                                            if bv then
                                                                do
                                                                    scn <- cEvalMulti lst c (S m)
                                                                    case scn of
                                                                        (st', Nothing, m') -> let gst' = intersection st' gst in auxWhile gst' st' b c m'
                                                                        (st', Just c', m') -> return (st', Just (Seq c' (While b c)), m')
                                                            else return (gst, Nothing, m)

--Prints Small Step Semantics Trace
printTrace :: State -> Com -> String
printTrace    st       c    = printTraceAux st c Z

--Helper function
printTraceAux :: State -> Com -> Nat -> String
printTraceAux    st       c      n   =  case cEvalMulti st c n of
                                                Nothing                -> "Error encountered."
                                                Just (st', Nothing, _) -> show st'
                                                Just (st', Just c', _) -> show st' ++ "\n" ++ printTraceAux st c (S n)

--Input: dividend, divisor
--Output: quotient, remainder
--Operation: quotient = dividend/divisor, remainder = dividend % divisor
--Assumptions: dividend >= 0, divisor > 0
{-
quotient := 0;
while (divisor <= dividend)
    quotient := quotient + 1;
    dividend := dividend - divisor;
remainder := dividend;
-}
divide :: Com
divide  = Seq (Assign "quotient" (AInt 0))
         (Seq (While (BLeq (AVar "divisor") (AVar "dividend"))
                  (Seq (Assign "quotient" (AAdd (AVar "quotient") (AInt 1)))
                  (Assign "dividend" (ASub (AVar "dividend") (AVar "divisor")))))
         (Assign "remainder" (AVar "dividend")))

--Input: dividend, divisor
--Output: gcd
--Operation: gcd = gcd(dividend, divisor)
--Assumptions: dividend & divisor > 0
{-
remainder := 1;
while (remainder != 0)
    divide;
    dividend := divisor;
    divisor := remainder;
gcd := dividend;
-}
gCD :: Com
gCD  = Seq (Assign "remainder" (AInt 1))
      (Seq (While (BNot (BAnd (BLeq (AVar "remainder") (AInt 0)) (BLeq (AInt 0) (AVar "remainder"))))
                (Seq (divide)
                (Seq (Assign "dividend" (AVar "divisor"))
                (Assign "divisor" (AVar "remainder")))))
      (Assign "gcd" (AVar "dividend")))

--Input: dividend
--Output: isPrime
--Operation: isPrime = 1 if dividend is prime and 0 otherwise
--Assumptions: input >= 0
{-
holdInput := dividend;
if (dividend <= 1) then isPrime := 0 else isPrime := 1;
divisor := dividend - 1;
while (2 <= divisor && 1 <= isPrime)
    divide;
    if (remainder == 0) then isPrime := 0 else skip;
    divisor := divisor - 1;
    dividend := holdInput
-}
primality :: Com
primality  = Seq (Assign "holdInput" (AVar "dividend"))
            (Seq (Assign "isPrime" (AInt 1))
            (Seq (If (BLeq (AVar "dividend") (AInt 1)) (Assign "isPrime" (AInt 0)) (Skip))
            (Seq (Assign "divisor" (ASub (AVar "dividend") (AInt 1)))
            (While (BAnd (BLeq (AInt 2) (AVar "divisor")) (BLeq (AInt 1) (AVar "isPrime")))
                (Seq divide
                (Seq (If (BAnd (BLeq (AInt 0) (AVar "remainder")) (BLeq (AVar "remainder") (AInt 0))) (Assign "isPrime" (AInt 0)) Skip)
                (Seq (Assign "divisor" (ASub (AVar "divisor") (AInt 1)))
                (Assign "dividend" (AVar "holdInput")))))))))

--Functions that have reference errors

--Input: input
{-
square := input * input;
if square <= 10 then num := 6 else num := 3;
num := input + imAFakeVariable; 
-}
refError1 :: Com
refError1 = Seq (Assign "square" (AMult (AVar "input") (AVar "input")))
            (Seq (If (BLeq (AVar "square") (AInt 10)) (Assign "num" (AInt 6)) (Assign "num" (AInt 3)))
            (Assign "num" (AAdd (AVar "input") (AVar "imAFakeVariable"))))

--Input: input
{-
double := input + input
while (0 <= double)
    counter := input + 1;
    double := double - 1;
output := counter;
-}
refError2 :: Com
refError2 =  Seq (Assign "double" (AAdd (AVar "input") (AVar "input")))
             (Seq (While (BLeq (AInt 0) (AVar "double"))
                (Seq (Assign "counter" (AAdd (AVar "input") (AInt 1)))
                (Assign "double" (ASub (AVar "double") (AInt 1)))))
            (Assign "output" (AVar "counter")))