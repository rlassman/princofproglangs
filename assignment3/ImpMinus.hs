module ImpMinus where

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
type State = Var -> Int

--1

--Inductive definition for arithmetic expressions
data AExp where
    AInt :: Int -> AExp
    AVar :: Var -> AExp
    AInc :: AExp -> AExp
    ADec :: AExp -> AExp

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
aEval :: State -> AExp -> Int
aEval     f     (AInt n)  =  n
aEval     f     (AVar v)  =  f v
aEval     f     (AInc a)  =  (aEval f a) + 1
aEval     f     (ADec a)  =  (aEval f a) - 1

--Evaluates boolean expressions
bEval :: State -> BExp -> Bool
bEval     f      (BTrue True)   = True
bEval     f      (BFalse False) = False
bEval     f      (BLeq a1 a2)   = (aEval f a1) <= (aEval f a2)
bEval     f      (BNot b)       = not (bEval f b)
bEval     f      (BAnd b1 b2)   = (bEval f b1) && (bEval f b2)

--Big-step semantics for Commands
cEval :: State -> Com -> State
cEval    st    Skip        = st
cEval    st   (Assign x a) = \y -> if y == x then aEval st a else st y
cEval    st   (Seq c1 c2)  = cEval (cEval st c1) c2
cEval    st   (If b c1 c2) = if bEval st b then cEval st c1 else cEval st c2
cEval    st   (While b c)  = if bEval st b then cEval (cEval st c) (While b c) else st

--Small-Step Semantics for Commands

--Returns state after one command is executed
cEvalOneStep :: State -> Com -> (State, Maybe Com)
cEvalOneStep    st  Skip        = (st, Nothing)
cEvalOneStep    st (Assign x a) = (\y -> if y == x then aEval st a else st y, Nothing)
cEvalOneStep    st (Seq c1 c2)  = case (cEvalOneStep st c1) of
                                    (st', Nothing) -> (st', Just c2)
                                    (st', Just c1')     -> (st', Just (Seq c1' c2))
cEvalOneStep    st (If b c1 c2) = if (bEval st b) then cEvalOneStep st c1 else cEvalOneStep st c2
cEvalOneStep    st (While b c)  = if (bEval st b) then cEvalOneStep st (Seq c (While b c)) else (st, Nothing)

--Returns list of all states after all commands are executed
cEvalSteps :: State -> Com -> [State]
cEvalSteps    st       c    = case (cEvalOneStep st c) of
                                (st', Nothing) -> [st']
                                (st', Just c') -> st':(cEvalSteps st' c')

--Adds the intial state to cEvalSteps so the trace is complete
printTrace :: State -> Com -> [State]
printTrace    st       c    = st : cEvalSteps st c

--2

--Input: X1, X2
--Output: X1
--Operation: X1 = X1 + X2
{-
if (X2 <= -1) then X3 := 0 else X3 := 1;
while (X2 != 0)
    if (X3 == 0) then X1--; X2++ else X1++; X2--;
-}
add :: Com
add =  Seq (If (BLeq (AVar X2) (AInt (-1))) (Assign X3 (AInt 0)) (Assign X3 (AInt 1)))
       (While (BNot (BAnd (BLeq (AVar X2) (AInt 0)) (BLeq (AInt 0) (AVar X2))))
            (If (BAnd (BLeq (AVar X3) (AInt 0)) (BLeq (AInt 0) (AVar X3))) 
                (Seq (Assign X1 (ADec (AVar X1))) 
                (Assign X2 (AInc (AVar X2))))
            (Seq (Assign X1 (AInc (AVar X1))) 
                (Assign X2 (ADec (AVar X2))))))

--Input: X1, X2
--Output: X1
--Operation: X1 = X1 - X2
{-
if (X2 <= -1) then X3 := 0 else X3 := 1;
while (X2 != 0)
    if (X3 == 0) then X1++; X2++ else X1--; X2--;
-}
sub :: Com
sub =  Seq (If (BLeq (AVar X2) (AInt (-1))) (Assign X3 (AInt 0)) (Assign X3 (AInt 1)))
            (While (BNot (BAnd (BLeq (AVar X2) (AInt 0)) (BLeq (AInt 0) (AVar X2))))
                    (If (BAnd (BLeq (AVar X3) (AInt 0)) (BLeq (AInt 0) (AVar X3))) 
                        (Seq (Assign X1 (AInc (AVar X1))) 
                        (Assign X2 (AInc (AVar X2))))
                    (Seq (Assign X1 (ADec (AVar X1))) 
                        (Assign X2 (ADec (AVar X2))))))

--Input: X2, X4
--Output: X1
--Operation: X1 = X2 * X4
{-
if (X4 <= -1) then X5 := 0 else X5 := 1;
X1 := 0;
X6 := X2;
while (X4 != 0)
    if (X5 == 0) then sub; X4++ else add; X4--;
    X2 = X6;
-}
mult :: Com
mult = Seq (If (BLeq (AVar X4) (AInt (-1))) (Assign X5 (AInt 0)) (Assign X5 (AInt 1)))
       (Seq (Assign X1 (AInt 0))
       (Seq (Assign X6 (AVar X2))
       (While (BNot (BAnd (BLeq (AVar X4) (AInt 0)) (BLeq (AInt 0) (AVar X4))))
            (Seq (If (BAnd (BLeq (AVar X5) (AInt 0)) (BLeq (AInt 0) (AVar X5)))
                (Seq sub 
                (Assign X4 (AInc (AVar X4))))
            (Seq add (Assign X4 (ADec (AVar X4)))))
            (Assign X2 (AVar X6))))))