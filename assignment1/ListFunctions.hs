{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module ListFunctions where

import NatNumFunctions

--6

--returns a list with duplicate natural numbers removed from the given list
remDup :: List Nat -> List Nat
remDup    Empty     =  Empty
remDup (Cons n ns)  =  let l = (Cons n Empty) in concatenate l (remDup (filterList (\ m -> not (equal m n)) ns))

--Tests for remDup
    --input: remDup Empty  |  output: Empty
    --input: remDup (Cons (S Z) (Cons Z Empty))  |  output: Cons (S Z) (Cons Z Empty)
    --input: remDup (Cons Z (Cons (S Z) (Cons Z Empty)))  |  output: Cons Z (Cons (S Z) Empty)
    --input: remDup (Cons Z (Cons Z (Cons Z Empty)))  |  output: Cons Z Empty

--returns a list with the natural numbers from the given list sorted in ascending order
quickSort :: List Nat -> List Nat
quickSort    Empty     =  Empty
quickSort (Cons n ns)  =  let l = (Cons n Empty) in
                             concatenate (quickSort (filterList (\m -> lessThan m n) ns)) 
                             (concatenate l ((quickSort (filterList (\m -> not (lessThan m n)) ns))))

--Tests for quickSort
    --input: quickSort Empty  |  output: Empty
    --input: quickSort (Cons (S Z) (Cons Z Empty))  |  output: Cons Z (Cons (S Z) Empty)
    --input: quickSort (Cons (S Z) (Cons (S (S Z)) (Cons (S Z) (Cons Z Empty)))) 
        -- |  output: Cons Z (Cons (S Z) (Cons (S Z) (Cons (S (S Z)) Empty)))
    --input: quickSort (Cons Z (Cons (S (S (S Z))) (Cons (S Z) (Cons (S (S Z)) Empty)))) 
        -- |  output: Cons Z (Cons (S Z) (Cons (S (S Z)) (Cons (S (S (S Z))) Empty))) 

--returns a list with the natural numbers from the given list sorted in ascending order
insertionSort :: List Nat -> List Nat
insertionSort    Empty    =  Empty
insertionSort (Cons n ns) =  insert n (insertionSort ns)

--Tests for insertionSort
    --input: insertionSort Empty  |  output: Empty
    --input: insertionSort (Cons (S Z) (Cons Z Empty))  |  output: Cons Z (Cons (S Z) Empty)
    --input: insertionSort (Cons (S Z) (Cons (S Z) (Cons Z Empty)))  |  output: Cons Z (Cons (S Z) (Cons (S Z) Empty))
    --input: insertionSort (Cons (S (S Z)) (Cons Z (Cons (S (S (S Z))) (Cons (S Z) Empty))))  
        -- |  output: Cons Z (Cons (S Z) (Cons (S (S Z)) (Cons (S (S (S Z))) Empty)))


--helper function that inserts a natural number into its sorted place in the given list (assumes given list is sorted)
insert :: Nat -> List Nat -> List Nat
insert    m      Empty     =  (Cons m Empty)
insert    m   (Cons n ns)  =  if not (lessThan m n) then (Cons n (insert m ns))
                                 else Cons m (Cons n ns)
--7

--splits a list of Tris wherever there is an Sp symbol, returning a list of contiguous blocks of information received, as separate lists
splitTri :: List Tri -> List (List Tri)
splitTri    Empty     =  Empty
splitTri (Cons a as)  =  splitBy (\x -> not (triEqualSp x)) (Cons a as)

--Tests for splitTri
    --input: splitTri Empty  |  output: Empty
    --input: splitTri (Cons Zero (Cons Sp Empty))  |  output: Cons (Cons Zero Empty) Empty
    --input: splitTri (Cons One (Cons Sp (Cons Zero Empty)))  |  output: Cons (Cons One Empty) (Cons (Cons Zero Empty) Empty)
    --input: splitTri (Cons Zero (Cons Sp (Cons One (Cons One (Cons Sp (Cons Zero Empty)))))) 
        -- |  output: Cons (Cons Zero Empty) (Cons (Cons One (Cons One Empty)) (Cons (Cons Zero Empty) Empty))

--data type that defines bits of information recieved by the reciever
data Tri where
    Zero :: Tri --represents 0 bit
    One  :: Tri --represents 1 bit
    Sp   :: Tri --special symbol representing missing bits
    deriving Show

--helper function that checks if a Tri equals Sp
triEqualSp :: Tri -> Bool
triEqualSp  Sp    =  True
triEqualSp  Zero  =  False
triEqualSp  One   =  False

--helper function that collects elements until the predicate is not satisfied
takeWhileL :: (a -> Bool) -> List a -> List a
takeWhileL     pred     Empty       =  Empty
takeWhileL     pred    (Cons a as)  =  if (pred a) then Cons a (takeWhileL pred as)
                                          else Empty

--helper function that drops elements until the predicate is not satisfied then returns the remaining list
dropWhileL :: (a -> Bool) -> List a -> List a
dropWhileL    pred      Empty       =  Empty
dropWhileL    pred     (Cons a as)  =  if (pred a) then dropWhileL pred as
                                          else Cons a as

--helper funcion that returns a list of lists of elements that satisfy the predicate
splitBy :: (a -> Bool) -> List a -> List (List a)
splitBy    pred       Empty      =  Empty
splitBy    pred      (Cons a as) =  let l1 = takeWhileL pred (Cons a as) in
                                    let l2 = dropWhileL pred (Cons a as) in
                                    let l3 = dropWhileL (\x -> not (pred x)) l2 in
                                        Cons l1 (splitBy pred l3)