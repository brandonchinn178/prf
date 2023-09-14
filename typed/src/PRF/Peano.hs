{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module PRF.Peano (
  PN (..),
  nat,
  fromNat,
  SomePN (..),
  asNat,
) where

import GHC.TypeLits
import Language.Haskell.TH qualified as TH

{----- PN -----}

class AsNat a where
  asNat :: a -> Nat

data PN (n :: Nat) where
  Z :: PN 0
  S :: PN n -> PN (n + 1)

deriving instance Show (PN n)

instance Eq (PN n) where
  -- if it typechecks, the numbers are the same
  _ == _ = True

instance AsNat (PN n) where
  asNat Z = 0
  asNat (S n) = 1 + asNat n

nat :: Nat -> TH.ExpQ
nat 0 = [| Z |]
nat n = [| S $(nat $ n - 1) |]

fromNat :: Nat -> SomePN
fromNat 0 = SomePN Z
fromNat n = withPN (SomePN . S) $ fromNat (n - 1)

data SomePN = forall n. SomePN (PN n)

instance Show SomePN where
  show (SomePN n) = show n
instance Eq SomePN where
  SomePN x == SomePN y = eq x y
    where
      eq :: PN x -> PN y -> Bool
      eq Z Z = True
      eq (S x) (S y) = eq x y
      eq _ _ = False

instance AsNat SomePN where
  asNat (SomePN n) = asNat n

withPN :: (forall x. PN x -> a) -> SomePN -> a
withPN f (SomePN n) = f n