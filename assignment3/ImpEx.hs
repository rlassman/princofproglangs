module ImpEx where 

--Memory
data Var where
    X1 :: Var
    X2 :: Var
    X3 :: Var
    X4 :: Var
    X5 :: Var
    X6 :: Var
    deriving (Eq, Show)

--Defines a State
type State = Var -> Float

--3

--Inductive definition for arithmetic expressions
data AExp where
    AFloat  :: Float -> AExp
    AVar    :: Var -> AExp
    AAdd    :: AExp -> AExp -> AExp
    ASub    :: AExp -> AExp -> AExp
    AMult   :: AExp -> AExp -> AExp
    ADiv    :: AExp -> AExp -> AExp
    ASqrt   :: AExp -> AExp

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
    Assign :: Var -> AExp -> Com
    Seq    :: Com -> Com -> Com
    If     :: BExp -> Com -> Com -> Com
    While  :: BExp -> Com -> Com

--Definition for Exception type
data Ex where
    DivBy0  :: Ex
    SqrtNeg :: Ex
    deriving (Eq, Show)

--Formats printing States
instance Show State where
    show :: State -> String
    show st = "{X1 -> " ++ show (st X1) ++ "} {X2 -> " ++ show (st X2)
                ++ "} {X3 -> " ++ show (st X3) ++ "} {X4 -> " ++ show (st X4)
                ++ "} {X5 -> " ++ show (st X5) ++ "} {X6 -> " ++ show (st X6) ++ "}" ++ "\n"

--Equality Test for States
instance Eq State where
    (==) :: State -> State -> Bool
    st1    ==   st2   =  ((st1 X1) == (st2 X1)) && ((st1 X2) == (st2 X2)) &&
                         ((st1 X3) == (st2 X3)) && ((st1 X4) == (st2 X4)) &&
                         ((st1 X5) == (st1 X5)) && ((st1 X6) == (st2 X6))

--Evaluates arithmatic expressions
aEval :: State -> AExp -> Either Ex Float
aEval    st   (AFloat n)    =  return n
aEval    st   (AVar v)      =  return (st v)
aEval    st   (AAdd a1 a2)  =  do
                                 n1 <- aEval st a1
                                 n2 <- aEval st a2
                                 return (n1 + n2)
aEval    st   (ASub a1 a2)  =  do
                                 n1 <- aEval st a1
                                 n2 <- aEval st a2
                                 return (n1 - n2)
aEval    st   (AMult a1 a2) =  do
                                 n1 <- aEval st a1
                                 n2 <- aEval st a2
                                 return (n1 * n2)
aEval    st   (ADiv a1 a2)  =  do
                                 n1 <- aEval st a1
                                 n2 <- aEval st a2
                                 if (n2 == 0) then Left DivBy0 else return (n1/n2)
aEval    st   (ASqrt a)     =  do
                                 n <- aEval st a
                                 if (n < 0) then Left SqrtNeg else return (sqrt n)

--Evaluates boolean expressions
bEval :: State -> BExp -> Either Ex Bool
bEval    st     (BTrue True)   =  Right True
bEval    st     (BFalse False) =  Right False
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
cEval :: State -> Com -> Either Ex State
cEval    st    Skip        =  return st
cEval    st   (Assign x a) = do
                              n <- aEval st a
                              return (\y -> if y == x then n else st y)
cEval    st   (Seq c1 c2)  =  do
                                st1 <- cEval st c1
                                cEval st1 c2
cEval    st   (If b c1 c2) =  do
                                bv <- bEval st b
                                if bv then cEval st c1 else cEval st c2
cEval    st   (While b c)  =  do
                                bv <- bEval st b
                                if bv then cEval st (Seq c (While b c)) else return st

--Small Step Semantics
cEvalOneStep :: State -> Com -> Either Ex (State, Maybe Com)
cEvalOneStep    st    Skip        =  return (st, Nothing)
cEvalOneStep    st   (Assign x a) =  do
                                       n <- aEval st a
                                       return (\y -> if y == x then n else st y, Nothing)
cEvalOneStep    st   (Seq c1 c2)  =  do
                                       (st', xc1') <- cEvalOneStep st c1
                                       case xc1' of
                                            Nothing  -> return (st', Just c2)
                                            Just c1' -> return (st', Just (Seq c1' c2))
cEvalOneStep    st   (If b c1 c2) =  do
                                       bv <- bEval st b
                                       if bv then cEvalOneStep st c1 else cEvalOneStep st c2
cEvalOneStep    st   (While b c)  =  do
                                       bv <- bEval st b
                                       if bv then cEvalOneStep st (Seq c (While b c)) else return (st, Nothing)

--Returns list of all states after all commands are executed
cEvalSteps :: State -> Com -> [Either Ex State]
cEvalSteps    st       c    = case (cEvalOneStep st c) of
                                Left e               -> [Left e] --terminates if exception
                                Right (st', Nothing) -> [Right st']
                                Right (st', Just c') ->  Right st':(cEvalSteps st' c')

--Adds the intial state to cEvalSteps so the trace is complete
printTrace :: State -> Com -> [Either Ex State]
printTrace    st       c    = Right st : cEvalSteps st c

--4

--Input: X1
--Output: X2
--Operation: X2 := 1 if X1 is an integer and 0 otherwise
{-
if (X1 <= 0) then X3 := 0 else X3 := 1;
while (not(not(X1 <= -1) && not(1 <= X1)))
    if X3 <= 0 then X1 := X1 + 1 else X1 := X1 - 1;
if (X1 == 0) then X2 := 1 else X2 := 0;
if (X1 == 1) then X2 := 1 else Skip; --case where X1 is 0
-}
isInt :: Com
isInt =  Seq (If (BLeq (AVar X1) (AFloat 0)) (Assign X3 (AFloat 0)) (Assign X3 (AFloat 1)))
         (Seq (While (BNot(BAnd (BNot (BLeq (AVar X1) (AFloat (-1)))) (BNot (BLeq (AFloat 1) (AVar X1))))) 
            (If (BLeq (AVar X3) (AFloat 0)) (Assign X1 (AAdd (AVar X1) (AFloat 1))) 
            (Assign X1 (ASub (AVar X1) (AFloat 1)))))
         (If (BAnd (BLeq (AVar X1) (AFloat 0)) (BLeq (AFloat 0) (AVar X1))) (Assign X2 (AFloat 1)) (Assign X2 (AFloat 0))))

--Input: X1
--Output: X2
--Operation: X2 = floor(X1)
{-
X2 := 0;
if (X1 <= 0) then X3 := 0 else X3 := 1;
while (not((0 <= X1) && not(1 <= X1)))
    if (X3 <= 0) then X2 := X2 - 1; X1 = X1 + 1 else X2 := X2 + 1; X1 = X1 - 1
-}
floor :: Com
floor =  Seq (Assign X2 (AFloat 0))
         (Seq (If (BLeq (AVar X1) (AFloat 0)) (Assign X3 (AFloat 0)) (Assign X3 (AFloat 1)))
         (While (BNot (BAnd (BLeq (AFloat 0) (AVar X1)) (BNot (BLeq (AFloat 1) (AVar X1))))) 
            (If (BLeq (AVar X3) (AFloat 0)) (Seq (Assign X2 (ASub (AVar X2) (AFloat 1))) (Assign X1 (AAdd (AVar X1) (AFloat 1)))) 
            (Seq (Assign X2 (AAdd (AVar X2) (AFloat 1))) (Assign X1 (ASub (AVar X1) (AFloat 1)))))))

--Input: X1
--Output: X2
--Operation: X2 = ceiling(X1)
{-
X2 := 0;
if (X1 <= 0) then X3 := 0 else X3 := 1;
while (not(not(X1 <= -1) && (X1 <= 0)))
    if (X3 <= 0) then X2 := X2 - 1; X1 = X1 + 1 else X2 := X2 + 1; X1 = X1 - 1
-}
ceiling :: Com
ceiling =  Seq (Assign X2 (AFloat 0))
         (Seq (If (BLeq (AVar X1) (AFloat 0)) (Assign X3 (AFloat 0)) (Assign X3 (AFloat 1)))
         (While (BNot (BAnd (BNot (BLeq (AVar X1) (AFloat (-1)))) (BLeq (AVar X1) (AFloat 0)))) 
            (If (BLeq (AVar X3) (AFloat 0)) (Seq (Assign X2 (ASub (AVar X2) (AFloat 1))) (Assign X1 (AAdd (AVar X1) (AFloat 1)))) 
            (Seq (Assign X2 (AAdd (AVar X2) (AFloat 1))) (Assign X1 (ASub (AVar X1) (AFloat 1)))))))

--Input: X4 (dividend), X5 (divisor)
--Output: X2 (quotient), X3 (remainder)
--Operation: X2 = X4/X5, X3 = X4 % X5
--Assumptions: X4 is an integer and X5 is a positive integer
{-
X1 := X4/X5;
floor;
X3 := X4 - (X5 * X2)
-}
intDiv :: Com
intDiv =  Seq (Assign X1 (ADiv (AVar X4) (AVar X5)))
          (Seq ImpEx.floor
          (Assign X3 (ASub (AVar X4) (AMult (AVar X5) (AVar X2)))))

--Input: X4
--Output: X2
--Operation: X2 = sqrt(X4)
--Assumptions: X4 is a positive integer
perfSq :: Com
perfSq =  Seq (Assign X1 (ASqrt (AVar X4)))
          (isInt)

--5

--Original Function
--Input: X1
--Output: X2
--Operation: X2 = harmonic mean of first X1 positive integers
--Assumption: X1 is a positive integer
{-
X2 = 0;
X3 = X1;
while (0 <= X3)
    X2 = X2 + (1 / X3);
    X3 = X3 - 1
X2 = X1 / X2
-}
hm :: Com
hm =  Seq (Assign X2 (AFloat 0))
      (Seq (Assign X3 (AVar X1))
      (Seq (While (BLeq (AFloat 0) (AVar X3))
          (Seq (Assign X2 (AAdd (AVar X2) (ADiv (AFloat 1) (AVar X3))))
          (Assign X3 (ASub (AVar X3) (AFloat 1)))))
      (Assign X2 (ADiv (AVar X1) (AVar X2)))))
--This function throws a DivBy0 error because the while loop iterates one too many times,
--causing 1 to be divided by 0 in the 4th line. Below is the corrected version:

--Corrected Function
--Input: X1
--Output: X2
--Operation: X2 = harmonic mean of first X1 positive integers
--Assumption: X1 is a positive integer
{-
X2 = 0;
X3 = X1;
while (1 <= X3) 
    X2 = X2 + (1 / X3);
    X3 = X3 - 1
X2 = X1 / X2
-}
hmC :: Com
hmC =  Seq (Assign X2 (AFloat 0))
       (Seq (Assign X3 (AVar X1))
       (Seq (While (BLeq (AFloat 1) (AVar X3))
           (Seq (Assign X2 (AAdd (AVar X2) (ADiv (AFloat 1) (AVar X3))))
           (Assign X3 (ASub (AVar X3) (AFloat 1)))))
       (Assign X2 (ADiv (AVar X1) (AVar X2)))))