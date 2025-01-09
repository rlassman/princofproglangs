{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module NatNumFunctions where

--1

--defines a natural number
data Nat where
    Z :: Nat
    S :: Nat -> Nat
    deriving Show

--defines a list
data List a where
    Empty :: List a
    Cons  :: a -> List a -> List a
    deriving Show

--defines a sum type
data a + b where
    Inl :: a -> a + b
    Inr :: b -> a + b
    deriving Show

--defines a product type
data a ** b where
    Pair :: a -> b -> a ** b

--defines a Unit type
data Unit where
    Unit :: Unit
    deriving Show

--defines a boolean type
data Boolean where
    T :: Boolean --represents True
    F :: Boolean --represents False
    deriving Show

--2

--adds two natural numbers
add :: Nat -> Nat -> Nat
add     Z      n   =  n
add    (S m)   n   =  S (add m n)

--Tests for add
    --input: add (S Z) Z  |  output: S Z
    --input: add Z (S (S Z))  |  output: S (S Z)
    --input: add (S (S Z)) (S (S Z))  |  output: S (S (S (S Z)))
    --input: add (S (S (S Z))) (S (S Z))  |  output: S (S (S (S (S Z))))

--multiplies two natural numbers
multiply :: Nat -> Nat -> Nat
multiply    Z     n     =  Z
multiply   (S m)  n     =  add n (multiply m n)

--Tests for multiply
    --input: multiply (S Z) Z  |  ouput: Z
    --input: multiply Z (S (S Z))  |  output: Z
    --input: multiply (S (S Z)) (S (S Z))  |  output: S (S (S (S Z)))
    --input: multiply (S (S (S Z))) (S (S Z))  |  output: S (S (S (S (S (S Z)))))

--raises a natural number to the power of another natural number
exponentiate :: Nat -> Nat -> Nat
exponentiate    n      Z    =  S Z
exponentiate    m     (S n) =  multiply m (exponentiate m n)

--Tests for exponentiate
    --input: exponentiate (S Z) Z  |  output: S Z
    --input: exponentiate Z (S (S Z))  |  output: Z
    --input: exponentiate (S (S Z)) (S (S Z))  |  output: S (S (S (S Z)))
    --input: exponentiate (S (S (S Z))) (S (S Z))  |  output: S (S (S (S (S (S (S (S (S Z))))))))

--subtracts two natural numbers
subtractNat :: Nat -> Nat -> Unit + Nat
subtractNat    m      Z    =  Inr m
subtractNat    Z    (S n)  =  Inl Unit
subtractNat   (S m) (S n)  =  subtractNat m n

--Tests for subtractNat
    --input: subtractNat (S Z) Z  |  output: Inr (S Z)
    --input: subtractNat Z (S (S Z))  |  ouput: Inl Unit
    --input: subtractNat (S (S Z)) (S (S Z))  |  ouput: Inr Z
    --input: subtractNat (S (S (S Z))) (S (S Z))  |  output: Inr (S Z)

--divides two natural numbers and returns the quotient
quotient :: Nat -> Nat -> Unit + Nat
quotient    m      Z    =  Inl Unit
quotient    m     (S n) =  case (subtractNat m (S n)) of
                              Inl Unit -> Inr Z
                              Inr m'   -> let Inr q = quotient m' (S n) in Inr (S q)

--Tests for quotient
    --input: quotient (S Z) Z  |  output: Inl Unit
    --input: quotient Z (S (S Z))  |  output: Inr Z
    --input: quotient (S (S Z)) (S (S Z))  |  output: Inr (S Z)
    --input: quotient (S (S (S Z))) (S (S Z))  |  output: Inr (S Z)

--divides two natural numbers and returns the remainder
remainder :: Nat -> Nat -> Unit + Nat
remainder    m      Z     =  Inl Unit
remainder    m     (S n)  =  case (subtractNat m (S n)) of
                                Inl Unit -> Inr m
                                Inr m'   -> remainder m' (S n)

--Tests for remainder
    --input: remainder (S Z) Z  |  output: Inl Unit
    --input: remainder Z (S (S Z))  |  output: Inr Z
    --input: remainder (S (S Z)) (S (S Z))  |  output: Inr Z
    --input: remainder (S (S (S Z))) (S (S Z))  |  output: Inr (S Z)

--3

--checks whether two natural numbers are equal
equal :: Nat -> Nat -> Bool --I used Haskell's Bool throughout so that I could use if-then, not, etc.
equal    Z      Z    =  True
equal    Z     (S n) =  False
equal   (S n)   Z    =  False
equal   (S n)  (S m) =  equal n m

--Tests for equal
    --input: equal (S Z) Z  |  output: False
    --input: equal Z (S (S Z))  |  output: False
    --input: equal (S (S Z)) (S (S Z))  |  output: True
    --input: equal (S (S (S Z))) (S (S Z))  |  output: False

--checks whether a natural number is less than or equal to another natural number
lessThan :: Nat -> Nat -> Bool
lessThan    Z      n    =  True
lessThan   (S m)   Z    =  False
lessThan   (S m)  (S n) =  lessThan m n

--Tests for lessThan
    --input: lessThan (S Z) Z  |  ouput: False
    --input: lessThan Z (S (S Z))  |  output: True
    --input: lessThan (S Z) (S (S Z))  |  output: True
    --input: lessThan (S (S (S Z))) (S (S Z))  |  output: False

--4

--finds the gcd of two natural numbers
gcdNat :: Nat -> Nat -> Nat
gcdNat    Z      n   =  n 
gcdNat   (S m)   n   =  case (remainder n (S m)) of
                            Inr Z      -> (S m)
                            Inr (S m') -> gcdNat (S m') (S m)

--Tests for gcdNat
    --input: gcdNat (S (S Z)) Z  |  output: S (S Z)
    --input: gcdNat (S Z) (S (S Z))  |  output: S Z
    --input: gcdNat (S (S (S Z))) (S (S Z))  |  output: S Z
    --input: gcdNat (S (S Z)) (S (S (S (S Z))))  |  output: S (S Z)

--checks whether a natural number is divisible by another natural number
divisible :: Nat -> Nat -> Bool
divisible    m      n    = case (remainder n m) of
                              Inl Unit   -> False
                              Inr Z      -> True
                              Inr (S m') -> False

--Tests for divisible
    --input: divisible Z (S Z)  |  output: False
    --input: divisible (S (S Z)) Z  |  output: True
    --input: divisible (S (S Z)) (S (S (S Z)))  |  output: False
    --input: divisible (S (S Z)) (S (S (S (S Z))))  |  output: True

--checks whether a natural number is prime
isPrime :: Nat -> Bool
isPrime    Z          =  False
isPrime (S Z)         =  False
isPrime (S (S Z))     =  True
isPrime (S (S (S m))) =  case filterList (\x -> x == True) (mapList (\x -> divisible x (S (S (S m)))) (fin (S (S (S m))))) of
                            Empty -> True
                            (Cons n ns) -> False

--Tests for isPrime
    --input: isPrime Z  |  output: False
    --input: isPrime S (S Z)  |  output: True
    --input: isPrime S (S (S (S Z))))  |  output: False
    --input: isPrime S (S (S (S (S Z)))))  |  output: True

--helper function that returns a list of natural numbers 2-given natural number
fin :: Nat -> List Nat
fin    Z        =  Empty
fin (S Z)       =  Empty
fin (S(S Z))    =  Empty
fin (S(S(S m))) =  Cons (S (S m)) (fin (S (S m)))

--returns a list containing the prime factorization of a natural number
primeFactorization :: Nat -> List Nat
primeFactorization     Z     =  Empty
primeFactorization    (S Z)  =  Empty
primeFactorization (S (S n)) =  factorize (S (S n)) (S (S Z))

--Tests for primeFactorization
    --input: primeFactorization Z  |  output: Empty
    --input: primeFactorization S (S (S Z))  |  output: Cons (S (S (S Z))) Empty
    --input: primeFactorization S (S (S (S Z)))  |  output: Cons (S (S Z)) (Cons (S (S Z)) Empty)
    --input: primeFactorization S (S (S (S (S (S (S (S (S (S (S (S Z)))))))))))  |  output: Cons (S (S Z)) (Cons (S (S Z)) (Cons (S (S (S Z))) Empty))

--helper function that finds prime factors of natural number m starting from natural number n
factorize :: Nat -> Nat -> List Nat
factorize   (S Z)    n   =  Empty
factorize    m       n   =  if divisible n m then Cons n (factorize (quotientNat m n) n)
                               else factorize m (S n)

--helper function that divides two natural numbers and returns a natural number rather than Unit + Nat
quotientNat :: Nat -> Nat -> Nat
quotientNat    m      Z    =  Z --garbage value
quotientNat    m     (S n) =  case (subtractNat m (S n)) of
                                Inl Unit -> Z
                                Inr m'   -> S (quotientNat m' (S n))

--5

--returns a list with the elements of the given list in reversed order
reverseList :: List a -> List a
reverseList   Empty      =  Empty
reverseList  (Cons n ns) =  concatenate (reverseList ns) (Cons n Empty)

--Tests for reverseList
    --input: reverseList Cons Z Empty  |  output: Cons Z Empty
    --input: reverseList Cons (S Z) (Cons Z Empty)  |  output: Cons Z (Cons (S Z) Empty)
    --input: reverseList Cons (S Z) (Cons Z (Cons (S (S Z)) Empty))  |  output: Cons (S (S Z)) (Cons Z (Cons (S Z) Empty))
    --input: reverseList Cons (S Z) (Cons (S Z) (Cons Z (Cons (S (S Z)) Empty)))  |  output: Cons (S (S Z)) (Cons Z (Cons (S Z) (Cons (S Z) Empty)))

--concatenates two lists into one list
concatenate :: List a -> List a -> List a
concatenate    Empty      l2     =  l2
concatenate   (Cons a as) l2     =  Cons a (concatenate as l2)

--Tests for concatenate
    --input: concatenate Empty (Cons (S Z) Empty)  |  output: Cons (S Z) Empty
    --input: concatenate (Cons Z Empty) (Cons (S Z) Empty)  |  output: Cons Z (Cons (S Z) Empty)
    --input: concatenate (Cons Z (Cons (S Z) Empty)) (Cons (S Z) Empty)  |  output: Cons Z (Cons (S Z) (Cons (S Z) Empty))
    --input: concatenate (Cons Z (Cons (S Z) Empty)) (Cons Z (Cons (S Z) Empty))  |  output: Cons Z (Cons (S Z) (Cons Z (Cons (S Z) Empty)))

--returns a list containing elements from the given list that meet the given predicate
filterList :: (a -> Bool) -> List a -> List a
filterList     pred        Empty      =  Empty
filterList     pred       (Cons a as) =  if (pred a) then Cons a (filterList pred as)
                                            else filterList pred as

--Tests for filterList
    --input: filterList (\x -> equal x (S Z)) Empty  |  output: Empty
    --input: filterList (\x -> equal x (S Z)) (Cons (S Z) (Cons Z (Cons (S Z) Empty)))  |  output: Cons (S Z) (Cons (S Z) Empty)
    --input: filterList (\x -> lessThan x (S Z)) (Cons (S Z) (Cons Z (Cons (S (S Z)) Empty)))  |  output: Cons (S Z) (Cons Z Empty)
    --input: filterList (\x -> lessThan x Z) (Cons (S Z) (Cons (S Z) (Cons (S (S Z)) Empty)))  |  output: Empty

--returns a list with the given predicate applied to every element in the given list
mapList :: (a -> b) -> List a -> List b
mapList    f       Empty       =  Empty
mapList    f      (Cons a as)  =  Cons (f a) (mapList f as)

--Tests for mapList
    --input; mapList (\x -> lessThan x (S Z)) Empty  |  output: Empty
    --input: mapList (\x -> lessThan x (S Z)) (Cons (S Z) (Cons Z (Cons (S (S Z)) Empty)))  |  output: Cons True (Cons True (Cons False Empty))
    --input: mapList (\x -> equal x (S Z)) (Cons Z (Cons (S Z) Empty))  |  output: Cons False (Cons True Empty)
    --input: mapList (\x -> S x) (Cons Z (Cons (S Z) Empty))  |  output: Cons (S Z) (Cons (S (S Z)) Empty)