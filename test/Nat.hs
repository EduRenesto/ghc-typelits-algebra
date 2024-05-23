{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Nat where

import Data.Kind (Type)

data Nat where
  Z    :: Nat
  Succ :: Nat -> Nat

type (:+:) :: Nat -> Nat -> Nat
type family x :+: y where
  x :+: Z = x
  x :+: (Succ y)  = Succ (x :+: y)

-- Ideia de annotation
-- Isso vai marcar o kind Nat como um Monoid, com :+: como operacao e
-- Z como elemento neutro
{-# ANN type Nat ("Monoid", ":+:", "Z") #-}

type N1 = Succ Z
type N2 = Succ N1
type N3 = Succ N2
type N4 = Succ N3
type N5 = Succ N4
type N6 = Succ N5
type N7 = Succ N6
type N8 = Succ N7
type N9 = Succ N8

type SNat :: Nat -> Type
data SNat n where
  SZ    :: SNat Z
  SSucc :: SNat n -> SNat (Succ n)

s1 :: SNat N1
s1 = SSucc SZ

s2 :: SNat N2
s2 = SSucc s1
