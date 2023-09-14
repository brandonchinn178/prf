{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module PRF.Axioms (
  -- * Peano numbers
  PN (..),
  fromNat,

  -- * Primitive recursive functions
  Func,
  runFunc,
  c,
  s,
  p,
  (•),
  rho,
) where

import Data.Maybe (listToMaybe)
import GHC.Stack (HasCallStack)
import Numeric.Natural (Natural)

data PN = Z | S PN
  deriving (Show, Eq)

fromNat :: Natural -> PN
fromNat 0 = Z
fromNat n = S (fromNat (n - 1))

data Func = Func
  { arity :: Natural
  , func :: [PN] -> PN
  }

instance Show Func where
  show Func{..} = "Func { arity = " ++ show arity ++ ", func = _ }"

mkFunc :: Natural -> ([PN] -> PN) -> Func
mkFunc n f =
  Func
    { arity = n
    , func = \xs ->
        if len xs == n
          then f xs
          else error $ "Expected arity " ++ show n ++ ", got: " ++ show xs
    }

runFunc :: Func -> [PN] -> PN
runFunc = func

c :: Natural -> Natural -> Func
c k n = mkFunc k $ \_ -> fromNat n

s :: Func
s = mkFunc 1 (S . head)

p :: Natural -> Natural -> Func
p k i
  | not $ 1 <= i && i <= k = error $ "Expected 1 <= " ++ show i ++ " <= " ++ show k
  | otherwise = mkFunc k (ix (i - 1))

(•) :: Func -> [Func] -> Func
h • gs
  | len gs /= arity h = error $ "Expected arity " ++ show (arity h) ++ ", got: " ++ show gs
  | not $ all ((== k) . arity) gs = error $ "Expected arity " ++ show k ++ ", got: " ++ show gs
  | otherwise = f
  where
    k = maybe 0 arity $ listToMaybe gs
    f = mkFunc k $ \xs -> runFunc h $ map (\g -> runFunc g xs) gs

rho :: Func -> Func -> Func
rho g h
  | arity h /= k + 2 = error $ "Expected arity " ++ show (k + 2) ++ ", got: " ++ show (arity h)
  | otherwise = f
  where
    k = arity g
    f = mkFunc (k + 1) $ \case
        [] -> impossible
        Z : xs -> runFunc g xs
        S y : xs -> runFunc h $ y : runFunc f (y : xs) : xs

{----- Utilities -----}

len :: [a] -> Natural
len = fromIntegral . length

ix :: Natural -> [a] -> a
ix n = (!! fromIntegral n)

impossible :: HasCallStack => a
impossible = error "The impossible happened!"
