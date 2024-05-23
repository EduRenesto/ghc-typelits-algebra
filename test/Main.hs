{-# OPTIONS_GHC -ddump-to-file -ddump-tc-trace -fplugin=MyLib -fplugin-opt=MyLib:foo=bar #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Main (main) where

import Data.Kind (Type)
import Data.Type.Equality

import Nat

--foo :: Monoid m => m -> m -> m
--foo x y = mappend x y

type Proxy :: Nat -> Type
data Proxy a where
  Proxy :: Proxy a

--thing :: Proxy (N1 :+: N1)
--thing = Proxy
--
--bar0 :: Proxy N2 -> ()
--bar0 _ = ()
--
--bar1 :: Proxy N3 -> ()
--bar1 _ = ()

addComm
  :: SNat a
  -> SNat b
  -> SNat (a :+: b) :~: SNat (b :+: a)
addComm _ _ = Refl

main :: IO ()
main = do
  --let _ = bar0 thing
  --let _ = bar1 thing
  let _ = addComm s1 s2
  putStrLn "Hello World"
