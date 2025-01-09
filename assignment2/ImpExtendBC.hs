module ImpExtendBC where

import Imp

--Defines a natural number
data Nat where
    Z :: Nat
    S :: Nat -> Nat
    deriving Show

--6

--Added Break and Continue constructs to Imp
data ComBC where
    SkipBC   :: ComBC
    AssignBC :: Var -> AExp -> ComBC
    SeqBC    :: ComBC -> ComBC -> ComBC
    IfBC     :: BExp -> ComBC -> ComBC -> ComBC
    WhileBC  :: BExp -> ComBC -> ComBC
    Break    :: ComBC
    Continue :: ComBC

--Marks if there was a Break or Continue command or neither
data Flags where
    None  :: Flags
    BFlag :: Flags
    CFlag :: Flags
    deriving Show

--Big-Step Semantics with Break and Continue
cEvalBC :: State -> ComBC -> (Flags, State)
cEvalBC    st   SkipBC        = (None, st)
cEvalBC    st  (AssignBC x a) = (None, \y -> if y == x then aEval st a else st y)
cEvalBC    st  (SeqBC c1 c2)  = case (cEvalBC st c1) of
                                  (None, st') -> cEvalBC st' c2
                                  (f, st')    -> (f, st')
cEvalBC    st  (IfBC b c1 c2) = if bEval st b then cEvalBC st c1 else cEvalBC st c2
cEvalBC    st  (WhileBC b c)  = if (bEval st b) then
                                  case (cEvalBC st c) of
                                    (None, st')  -> cEvalBC st' (WhileBC b c)
                                    (BFlag, st') -> (None, st')
                                    (CFlag, st') -> cEvalBC st' (WhileBC b c)
                                else (None, st)
cEvalBC    st   Break         = (BFlag, st)
cEvalBC    st   Continue      = (CFlag, st)

--Returns just the State from cEvalBC
printCEvalBC :: State -> ComBC -> State
printCEvalBC    st       c     =  snd (cEvalBC st c)

--Small-Step Semantics with Break and Continue
cEvalMultiBC :: State -> ComBC -> Nat -> (Flags, State, Maybe ComBC, Nat)
cEvalMultiBC    st   c              Z     =  (None, st, Just c, Z)
cEvalMultiBC    st   SkipBC        (S n)  =  (None, st, Nothing, n)
cEvalMultiBC    st  (AssignBC x a) (S n)  =  (None, (\y -> if y == x then aEval st a else st y), Nothing, n)
cEvalMultiBC    st  (SeqBC c1 c2)  (S n)  =  case (cEvalMultiBC st c1 (S n)) of
                                                (None, st', Just c1', Z) -> (None, st', Just (SeqBC c1' c2), Z)
                                                (None, st', Nothing, m)  -> cEvalMultiBC st' c2 m
                                                (f, st', _, m)           -> (f, st', Nothing, m)
cEvalMultiBC    st  (IfBC b c1 c2) (S n)  =  if (bEval st b) then cEvalMultiBC st c1 (S n) else cEvalMultiBC st c2 (S n)
cEvalMultiBC    st  (WhileBC b c)  (S n)  =  if (bEval st b) then 
                                                case (cEvalMultiBC st c (S n)) of
                                                    (None, st', Just c', Z) -> (None, st', Just (SeqBC c' (WhileBC b c)), Z)
                                                    (None, st', Nothing, m) -> cEvalMultiBC st' (WhileBC b c) m
                                                    (BFlag, st', _, m)      -> (None, st', Nothing, m)
                                                    (CFlag, st', _, m)      -> cEvalMultiBC st' (WhileBC b c) m
                                             else (None, st, Nothing, n)
cEvalMultiBC    st   Break         (S n)  =  (BFlag, st, Nothing, n)
cEvalMultiBC    st   Continue      (S n)  =  (CFlag, st, Nothing, n)

--Returns a list of all states after running all commands
cEvalMultiStepsBC :: State -> ComBC -> [State]
cEvalMultiStepsBC    st       c     =  cEvalMultiStepsBCAux st c Z

cEvalMultiStepsBCAux :: State -> ComBC -> Nat -> [State]
cEvalMultiStepsBCAux    st       c        n   =  case cEvalMultiBC st c n of
                                                    (_, st', Nothing, _) -> [st']
                                                    (_, st', Just c', _) -> st':cEvalMultiStepsBCAux st c (S n)


--7 (extended Imp programs)

{-
X1 := 22;
X4 := 36;
X3 := 17;
if (X1 <= X2) then Break else X2 := 50;
X4 := 1;
-}
testBOutside :: ComBC
testBOutside =  SeqBC (AssignBC X1 (AInt 22))
                (SeqBC (AssignBC X4 (AInt 36))
                (SeqBC (AssignBC X3 (AInt 17))
                (SeqBC (IfBC (BLeq (AVar X1) (AVar X2)) Break (AssignBC X2 (AInt 50)))
                (AssignBC X4 (AInt 1)))))

{-
while (True)
    X1 := 42;
    X2 := 12;
    if (X3 <= 5) then X3 := 10 else skip;
    Break;
    X1 := 3;
    X4 := 20;
-}
testBInside :: ComBC
testBInside =  WhileBC (BTrue True)
                   (SeqBC (AssignBC X1 (AInt 42))
                   (SeqBC (AssignBC X2 (AInt 12))
                   (SeqBC (IfBC (BLeq (AVar X3) (AInt 5)) (AssignBC X3 (AInt 10)) (SkipBC))
                   (SeqBC Break
                   (SeqBC (AssignBC X1 (AInt 3))
                   (AssignBC X4 (AInt 20)))))))

{-
while (0 <= X1) 
    X1 := X1 - 2;
    X3 := 4;
    Break;
    while (X3 <= 6)
        X3 := X3 + 1
    X2 := X1;
X3 := 16;
-}
testBOuter :: ComBC
testBOuter =  SeqBC (WhileBC (BLeq (AInt 0) (AVar X1))
                    (SeqBC (AssignBC X1 (ASub (AVar X1) (AInt 2)))
                    (SeqBC (AssignBC X3 (AInt 4))
                    (SeqBC Break
                    (SeqBC (WhileBC (BLeq (AVar X3) (AInt 6))
                          (AssignBC X3 (AAdd (AVar X3) (AInt 1))))
                    (AssignBC X2 (AVar X1)))))))
                (AssignBC X3 (AInt 16))

{-
while (X1 <= 5)
    X2 := X2 - 1;
    while (0 <= X2)
        X3 := X3 + X2;
        Break;
        X1 := X1 + X1;
        X4 := 2;
    X5 := X1;
    X1 := X1 + 1;
X5 := 21;
-}
testBInner :: ComBC
testBInner =  SeqBC (WhileBC (BLeq (AVar X1) (AInt 5))
                  (SeqBC (AssignBC X2 (ASub (AVar X2) (AInt 1)))
                  (SeqBC (WhileBC (BLeq (AInt 0) (AVar X2))
                        (SeqBC (AssignBC X3 (AAdd (AVar X3) (AVar X2)))
                        (SeqBC Break
                        (SeqBC (AssignBC X1 (AAdd (AVar X1) (AVar X1)))
                        (AssignBC X4 (AInt 2))))))
                  (SeqBC (AssignBC X5 (AVar X1))
                  (AssignBC X1 (AAdd (AVar X1) (AInt 1)))))))
              (AssignBC X5 (AInt 21))

{-
X4 := 7;
if (X1 <= 2) then X1 := 0 else X1 := 14;
X3 := X1 + X4;
Continue;
X1 := 24;
X5 := 88;
-}
testCOutside :: ComBC
testCOutside =  SeqBC (AssignBC X4 (AInt 7))
                (SeqBC (IfBC (BLeq (AVar X1) (AInt 2)) (AssignBC X1 (AInt 0)) (AssignBC X1 (AInt 14)))
                (SeqBC (AssignBC X3 (AAdd (AVar X1) (AVar X4)))
                (SeqBC Continue
                (SeqBC (AssignBC X1 (AInt 24))
                (AssignBC X5 (AInt 88))))))

{-
while (0 <= X1)
    X1 := X1 - 1;
    X2 := X2 - 15;
    if (1 <= X2) then Continue else Skip;
    X2 := 2;
    X6 := X1;
X5 := X2;
-}
testCInside :: ComBC
testCInside =  SeqBC (WhileBC (BLeq (AInt 0) (AVar X1)) 
                   (SeqBC (AssignBC X1 (ASub (AVar X1) (AInt 1)))
                   (SeqBC (AssignBC X2 (ASub (AVar X2) (AInt 15)))
                   (SeqBC (IfBC (BLeq (AInt 1) (AVar X2)) Continue SkipBC)
                   (SeqBC (AssignBC X2 (AInt 2))
                   (AssignBC X6 (AVar X1)))))))
                (AssignBC X5 (AVar X2))

{-
while (X5 <= 5)
    while (X1 <= 4)
        X2 := X2 * 2;
        X1 := X1 + 1;
    X5 := X5 + 1;
    X1 := 3;
    Continue;
    X4 := 25;
    while (10 <= X4)
        X4 := X4 - 5;
X6 := 111;
-}
testCOuter :: ComBC
testCOuter =  SeqBC (WhileBC (BLeq (AVar X5) (AInt 5))
                  (SeqBC (WhileBC (BLeq (AVar X1) (AInt 4))
                        (SeqBC (AssignBC X2 (AMult (AVar X2) (AInt 2)))
                        (AssignBC X1 (AAdd (AVar X1) (AInt 1)))))
                  (SeqBC (AssignBC X5 (AAdd (AVar X5) (AInt 1)))
                  (SeqBC (AssignBC X1 (AInt 3))
                  (SeqBC Continue
                  (WhileBC (BLeq (AInt 10) (AVar X4))
                        (AssignBC X4 (ASub (AVar X4) (AInt 5)))))))))
              (AssignBC X6 (AInt 111))

{-
X4 := 6;
while (16 <= X2)
    X1 := X4;
    while (X1 <= 10)
        X1 := X1 + 2;
        Continue;
        X3 := X6;
    X2 := X2 - 15;
    X4 := X4 + 1;
-}
testCInner :: ComBC
testCInner =  SeqBC (AssignBC X4 (AInt 6))
              (WhileBC (BLeq (AInt 16) (AVar X2))
                  (SeqBC (AssignBC X1 (AVar X4))
                  (SeqBC (WhileBC (BLeq (AVar X1) (AInt 10))
                       (SeqBC (AssignBC X1 (AAdd (AVar X1) (AInt 2)))
                       (SeqBC Continue
                       (AssignBC X3 (AVar X6)))))
                  (SeqBC (AssignBC X2 (ASub (AVar X2) (AInt 15)))
                  (AssignBC X4 (AAdd (AVar X4) (AInt 1)))))))


{-
while (0 <= X1) 
    X1 := X1 - 1;
    while (X3 <= 6)
        X3 := X3 + 1
        Continue;
        X1 := X5;
    Break;
    X2 := X1;
if (5 <= X3) then X4 := 66 else Skip;
-}
testBCLoop :: ComBC
testBCLoop = SeqBC (WhileBC (BLeq (AInt 0) (AVar X1))
                (SeqBC (AssignBC X1 (ASub (AVar X1) (AInt 1)))
                (SeqBC (WhileBC (BLeq (AVar X3) (AInt 6))
                    (SeqBC (AssignBC X3 (AAdd (AVar X3) (AInt 1)))
                    (SeqBC Continue
                    (AssignBC X1 (AVar X5)))))
                (SeqBC Break
                (AssignBC X2 (AVar X1)))))) 
             (IfBC (BLeq (AInt 5) (AVar X3)) (AssignBC X4 (AInt 66)) SkipBC)