{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module PRF.Axioms (
  -- * Peano numbers
  module X,

  -- * Primitive recursive functions
  Func,
  runFunc,
  c,
  s,
  -- p,
  -- (•),
  -- rho,

  -- * Common PRFs
  c_0_0,
  c_0_1,
  c_1_0,
  c_1_1,
  c_2_0,
  c_2_1,
  p_1_1,
  p_2_1,
  p_2_2,
  p_3_1,
  p_3_2,
  p_3_3,
  p_4_1,
  p_4_2,
  p_4_3,
  p_4_4,
) where

import Data.Maybe (listToMaybe)
import Data.Vector.Fixed qualified as Vec
import Data.Vector.Fixed.Boxed (Vec)
import GHC.Stack (HasCallStack)
import GHC.TypeLits
import PRF.Peano as X

{----- Func -----}

data Func n = Func
  { arity :: PN n
  , func :: Vec n SomePN -> SomePN
  }

instance Show (Func n) where
  show Func{..} = "Func { arity = " ++ show (asNat arity) ++ ", func = _ }"

runFunc :: Func n -> Vec n SomePN -> SomePN
runFunc = func

{----- Axioms -----}

c :: PN k -> PN n -> Func k
c k n = Func k $ \_ -> SomePN n

s :: Func 1
s = Func (S Z) $ \v ->
  case Vec.convert v of
    Vec.Only (SomePN n) -> SomePN (S n)

p :: (KnownNat k, i <= k, i ~ i0 + 1, i0 + 1 <= k) => PN k -> PN i -> Func k
p k (S n) = Func k $ (`Vec.index` n)

-- (•) :: Func -> [Func] -> Func
-- h • gs
--   | len gs /= arity h = error $ "Expected arity " ++ show (arity h) ++ ", got: " ++ show gs
--   | not $ all ((== k) . arity) gs = error $ "Expected arity " ++ show k ++ ", got: " ++ show gs
--   | otherwise = f
--   where
--     k = maybe 0 arity $ listToMaybe gs
--     f = Func k $ \xs -> runFunc h $ map (\g -> runFunc g xs) gs

-- rho :: Func -> Func -> Func
-- rho g h
--   | arity h /= k + 2 = error $ "Expected arity " ++ show (k + 2) ++ ", got: " ++ show (arity h)
--   | otherwise = f
--   where
--     k = arity g
--     f = Func (k + 1) $ \case
--         [] -> impossible
--         Z : xs -> runFunc g xs
--         S y : xs -> runFunc h $ y : runFunc f (y : xs) : xs

{----- Common PRFs -----}

c_0_0 :: Func 0
c_0_0 = c $(nat 0) $(nat 0)

c_0_1 :: Func 0
c_0_1 = c $(nat 0) $(nat 1)

c_1_0 :: Func 1
c_1_0 = c $(nat 1) $(nat 0)

c_1_1 :: Func 1
c_1_1 = c $(nat 1) $(nat 1)

c_2_0 :: Func 2
c_2_0 = c $(nat 2) $(nat 0)

c_2_1 :: Func 2
c_2_1 = c $(nat 2) $(nat 1)

p_1_1 :: Func 1
p_1_1 = p $(nat 1) $(nat 1)

p_2_1 :: Func 2
p_2_1 = p $(nat 2) $(nat 1)

p_2_2 :: Func 2
p_2_2 = p $(nat 2) $(nat 2)

p_3_1 :: Func 3
p_3_1 = p $(nat 3) $(nat 1)

p_3_2 :: Func 3
p_3_2 = p $(nat 3) $(nat 2)

p_3_3 :: Func 3
p_3_3 = p $(nat 3) $(nat 3)

p_4_1 :: Func 4
p_4_1 = p $(nat 4) $(nat 1)

p_4_2 :: Func 4
p_4_2 = p $(nat 4) $(nat 2)

p_4_3 :: Func 4
p_4_3 = p $(nat 4) $(nat 3)

p_4_4 :: Func 4
p_4_4 = p $(nat 4) $(nat 4)


-- {----- Utilities -----}

-- len :: [a] -> Nat
-- len = fromIntegral . length

-- ix :: Nat -> [a] -> a
-- ix n = (!! fromIntegral n)

-- impossible :: HasCallStack => a
-- impossible = error "The impossible happened!"
