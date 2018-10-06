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
   n is assummed as type Variable whose type is Type
    • Expected kind ‘Nat’, but ‘n’ has kind ‘*’ 
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
















