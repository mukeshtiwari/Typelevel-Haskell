{-# LANGUAGE DataKinds, TypeFamilies, GADTs, RankNTypes, TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

import Data.Kind (Type)

data Nat = Z | Succ Nat

type family Add (a :: Nat) (b :: Nat) :: Nat where
   Add Z b = b
   Add (Succ a) b = Succ (Add a b)

type family Mult (a :: Nat) (b :: Nat) :: Nat where
   Mult Z b = Z
   Mult (Succ a) b = Add b (Mult a b)


data Vector a n where
  Nil :: Vector a Z
  Cons :: a -> Vector a n -> Vector a (Succ n)

deriving instance Eq a => Eq (Vector a n)
deriving instance Show a => Show (Vector a n)

toList :: Vector a n -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

heaD :: Vector a (Succ n) -> a
heaD (Cons x _) = x

taiL :: Vector a (Succ n) -> Vector a n
taiL (Cons x xs) = xs


main :: IO ()
main = do
  print $ heaD (Cons 1 (Cons 2 Nil))
  print $ taiL (Cons 1 (Cons 2 Nil))
  -- | Uncommenting the line below causes type error
  -- print $ head Nil
-- /show

append :: Vector a n -> Vector a m -> Vector a (Add n m)
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

{- This implimentation is buggy, and won't compile, but the thing to remember here is 
   n can be Z or (Succ n) appearing in type level, and there is no constructor to 
   construct element of type Z, and element of type (Succ n). 
   If you look at the implementation of SNat, then we have two constructors 
   SZ : SNat Z
   SSuc  to construct SNat (Succ n).
   Now we can inspect n by pattern matching on SNat n.  
fromList :: n  -> [a] -> Vector a n
fromList Z [] = Nil
fromList (Succ n) (x : xs) = Cons x (fromList xs) -}



data SNat :: Nat -> Type where
  SZero :: SNat Z
  SSuc  :: SNat n -> SNat (Succ n)

{- This code still does not give perfect Sigma types. You can pass two different parameters -}
fromList :: forall (n :: Nat). SNat n -> forall (a :: Type). [a] -> Vector a n
fromList SZero [] = Nil
fromList (SSuc n) (x : xs) = Cons x (fromList n xs)

{- 

*Main> fromList SZero [1]
*** Exception: /Users/keep_learning/Mukesh/Github/Typelevel-Haskell/Dephaskell.hs:(63,1)-(64,51): Non-exhaustive patterns in function fromList

*Main> fromList SZero []
Nil

*Main> fromList (SSuc (SSuc SZero)) [1,2,3]
Cons 1 (Cons 2 *** Exception: /Users/keep_learning/Mukesh/Github/Typelevel-Haskell/Dephaskell.hs:(63,1)-(64,51): Non-exhaustive patterns in function fromList

*Main> fromList (SSuc (SSuc SZero)) [1,2]
Cons 1 (Cons 2 Nil) -}


replicateVec :: forall (n :: Nat). SNat n -> forall (a :: Type). a -> Vector a n
replicateVec SZero _ = Nil
replicateVec (SSuc n) a = Cons a (replicateVec n a)

zipWithSame :: forall (n :: Nat) a b c . (a -> b -> c) -> Vector a n -> Vector b n -> Vector c n
zipWithSame f Nil Nil = Nil
zipWithSame f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWithSame f xs ys)


type family Min (a :: Nat) (b :: Nat) :: Nat where
   Min Z Z = Z
   Min Z (Succ b) = Z
   Min (Succ a) Z = Z
   Min (Succ a) (Succ b) = Succ (Min a b)


zipWithDiff :: forall n m a b c. (a -> b -> c) -> Vector a n -> Vector b m -> Vector c (Min n m)
zipWithDiff f Nil Nil = Nil
zipWithDiff f Nil (Cons _ _) = Nil
zipWithDiff f (Cons _ _) Nil = Nil
zipWithDiff f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWithDiff f xs ys)












