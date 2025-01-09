{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module FoldFunctions where

import NatNumFunctions
import ListFunctions

--8

--a factorization of recursion on natural numbers
foldNat :: (a -> a) -> a -> Nat -> a
foldNat     f          a   Z     =  a
foldNat     f          a  (S n)  =  f (foldNat f a n)

--adds two natural numbers together
addFold :: Nat -> Nat -> Nat
addFold    m      n    =  foldNat S n m

--Tests for addFold
    --input: addFold Z (S (S Z))  |  output: S (S Z)
    --input: addFold (S Z) Z  |  output: S Z
    --input: addFold (S Z) (S (S Z))  |  output: S (S (S Z))
    --input: addFold (S (S (S Z))) (S (S Z))  |  output: S (S (S (S (S Z))))

--multiplies two natural numbers
multFold :: Nat -> Nat -> Nat
multFold    m      n   =  foldNat (add n) Z m

--Tests for multFold
    --input: multFold (S Z) Z  |  ouput: Z
    --input: multFold Z (S (S Z))  |  output: Z
    --input: multFold (S (S Z)) (S (S Z))  |  output: S (S (S (S Z)))
    --input: multFold (S (S (S Z))) (S (S Z))  |  output: S (S (S (S (S (S Z)))))

--raises a natural number to the power of another natural number
expFold :: Nat -> Nat -> Nat
expFold    m      n   =  foldNat (multiply m) (S Z) n

--Tests for expFold
    --input: expFold (S Z) Z  |  ouput: S Z
    --input: expFold Z (S (S Z))  |  output: Z
    --input: expFold (S (S Z)) (S Z)  |  output: S (S Z)
    --input: expFold (S (S (S Z))) (S (S Z))  |  output: S (S (S (S (S (S (S (S (S Z))))))))

--subtracts two natural numbers
subtractFold :: Nat -> Nat -> Unit + Nat
subtractFold    m      Z     =  Inr m
subtractFold    m      n     =  foldNat (\x -> case subtractNat m n of
                                            Inl Unit -> Inl Unit
                                            Inr Z -> Inl Unit
                                            Inr d -> Inr d)
                                        (Inr m) n

--Tests for subtractFold
    --input: subtractFold (S Z) Z  |  output: Inr (S Z)
    --input: subtractFold Z (S (S Z))  |  output: Inl Unit
    --input: subtractFold (S (S Z)) (S Z)  |  output: Inr (S Z)
    --input: subtractFold (S (S (S (S Z)))) (S (S Z)))  |  output: Inr (S (S Z))

--9

--a factorization of recursion on lists
foldList :: (a -> b -> b) -> b -> List a -> b
foldList     f             b   Empty      =  b
foldList     f             b  (Cons a as) =  f a (foldList f b as)

--concatenates two lists into one list
concatFold :: List a -> List a -> List a
concatFold    l1        l2     =  foldList Cons l2 l1

--Tests for concatFold
    --input: concatFold Empty (Cons (S Z) Empty)  |  output: Cons (S Z) Empty
    --input: concatFold (Cons Z Empty) (Cons (S Z) Empty)  |  output: Cons Z (Cons (S Z) Empty)
    --input: concatFold (Cons Z (Cons (S Z) Empty)) (Cons (S Z) Empty)  |  output: Cons Z (Cons (S Z) (Cons (S Z) Empty))
    --input: concatFold (Cons Z (Cons (S Z) Empty)) (Cons Z (Cons (S Z) Empty))  |  output: Cons Z (Cons (S Z) (Cons Z (Cons (S Z) Empty)))

--returns a list containing elements from the given list that meet the given condition
filterFold :: (a -> Bool) -> List a -> List a
filterFold     pred        l      =  foldList (\ x y -> if pred x then Cons x y else y) Empty l

--Tests for filterFold
    --input: filterFold (\x -> equal x (S Z)) Empty  |  output: Empty
    --input: filterFold (\x -> equal x (S Z)) (Cons (S Z) (Cons Z (Cons (S Z) Empty)))  |  output: Cons (S Z) (Cons (S Z) Empty)
    --input: filterFold (\x -> lessThan x (S Z)) (Cons (S Z) (Cons Z (Cons (S (S Z)) Empty)))  |  output: Cons (S Z) (Cons Z Empty)
    --input: filterFold (\x -> lessThan x Z) (Cons (S Z) (Cons (S Z) (Cons (S (S Z)) Empty)))  |  output: Empty

--returns a list with the given predicate applied to every element in the given list
mapFold :: (a -> b) -> List a -> List b
mapFold     g          l      =  foldList (\x y -> Cons (g x) y) Empty l

--Tests for mapFold
    --input; mapFold (\x -> lessThan x (S Z)) Empty  |  output: Empty
    --input: mapFold (\x -> lessThan x (S Z)) (Cons (S Z) (Cons Z (Cons (S (S Z)) Empty)))  |  output: Cons True (Cons True (Cons False Empty))
    --input: mapFold (\x -> equal x (S Z)) (Cons Z (Cons (S Z) Empty))  |  output: Cons False (Cons True Empty)
    --input: mapFold (\x -> S x) (Cons Z (Cons (S Z) Empty))  |  output: Cons (S Z) (Cons (S (S Z)) Empty)