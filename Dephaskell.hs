{-# LANGUAGE DataKinds, TypeFamilies, GADTs #-}
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

head :: Vector a (Succ n) -> a
head (Cons x _) = x

tail :: Vector a (Succ n) -> Vector a n
tail (Cons x xs) = xs

