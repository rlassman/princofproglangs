module ImpExtendB where
import Imp

--Defines a natural number
data Nat where
    Z :: Nat
    S :: Nat -> Nat
    deriving Show

--5

--Added Break construct to Imp
data ComB where --delete B if states work, do same for BC
    SkipB   :: ComB
    AssignB :: Var -> AExp -> ComB
    SeqB    :: ComB -> ComB -> ComB
    IfB     :: BExp -> ComB -> ComB -> ComB
    WhileB  :: BExp -> ComB -> ComB
    Break   :: ComB

--Big-Step Semantics with Break
cEvalB :: State -> ComB -> (Bool, State)
cEvalB    st   SkipB         = (False, st)
cEvalB    st  (AssignB x a)  = (False, \y -> if y == x then aEval st a else st y)
cEvalB    st  (SeqB c1 c2)   = case (cEvalB st c1) of
                                (True, st')  -> (True, st')
                                (False, st') -> cEvalB st' c2 
cEvalB    st  (IfB b c1 c2)  = if bEval st b then cEvalB st c1 else cEvalB st c2
cEvalB    st  (WhileB b c)   = if bEval st b then 
                                 case (cEvalB st c) of
                                    (True, st')  -> (False, st')
                                    (False, st') -> cEvalB st' (WhileB b c)
                               else (False, st)
cEvalB    st   Break         = (True, st)

--Returns just the State from cEvalB
printCEvalB :: State -> ComB -> State
printCEvalB    st       c     =  snd (cEvalB st c)

--Small-Step Semantics with Break
cEvalMultiB :: State -> ComB -> Nat -> (Bool, State, Maybe ComB, Nat)
cEvalMultiB    st    c             Z    =  (False, st, Just c, Z)
cEvalMultiB    st    SkipB        (S n) =  (False, st, Nothing, n)
cEvalMultiB    st   (AssignB x a) (S n) =  (False, (\y -> if y == x then aEval st a else st y), Nothing, n)
cEvalMultiB    st   (SeqB c1 c2)  (S n) =  case (cEvalMultiB st c1 (S n)) of
                                                (False, st', Just c1', Z) -> (False, st', Just (SeqB c1' c2), Z)
                                                (False, st', Nothing, m)  -> cEvalMultiB st' c2 m
                                                (True, st', _, m)         -> (True, st', Nothing, m)
cEvalMultiB    st   (IfB b c1 c2) (S n) =  if (bEval st b) then cEvalMultiB st c1 (S n) else cEvalMultiB st c2 (S n)
cEvalMultiB    st   (WhileB b c)  (S n) =  if (bEval st b) then 
                                             case (cEvalMultiB st c (S n)) of
                                                (False, st', Just c', Z) -> (False, st', Just (SeqB c' (WhileB b c)), Z)
                                                (False, st', Nothing, m) -> cEvalMultiB st' (WhileB b c) m 
                                                (True, st', _, m)        -> (False, st', Nothing, m)
                                           else (False, st, Nothing, n)
cEvalMultiB    st    Break        (S n) =  (True, st, Nothing, n)

--Returns a list of all states after running all commands
cEvalMultiStepsB :: State -> ComB -> [State]
cEvalMultiStepsB    st       c    =  cEvalMultiStepsBAux st c Z

--Helper function
cEvalMultiStepsBAux :: State -> ComB -> Nat -> [State]
cEvalMultiStepsBAux    st       c      n   =  case cEvalMultiB st c n of
                                                (_, st', Nothing, _) -> [st']
                                                (_, st', Just c', _) -> st':cEvalMultiStepsBAux st c (S n)