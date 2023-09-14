{-# LANGUAGE DataKinds #-}

module PRF (
  module X,

  -- * Examples
  add,
  double,
  mul,
  pred,
  sub,

  -- ** Predicates
  true,
  false,
  isZero,
  leq,
  geq,
  if_,
  and,
  or,
  not,
  eq,
  lt,
  gt,

  -- ** Other examples
  pow,
) where

import PRF.Axioms as X
import Prelude hiding (and, not, or, pred)

add :: Func 2
add = rho (p_1_1) (s • [p_3_2])

double :: Func 2
double = add • [p_1_1, p_1_1]

mul :: Func 2
mul = rho (c_1_0) (add • [p_3_2, p_3_3])

pred :: Func 1
pred = rho (c_0_0) (p_2_1)

sub :: Func 2
sub = rsub • [p_2_2, p_2_1]
  where
    rsub = rho (p_1_1) (pred • [p_3_2])

true :: PN 1
true = S Z

false :: PN 0
false = Z

isZero :: Func 1
isZero = rho (c_0_1) (c_2_0)

leq :: Func 2
leq = isZero • [sub]

geq :: Func 2
geq = leq • [p_2_2, p_2_1]

if_ :: Func 3
if_ = rho (p_2_2) (p_4_3)

and :: Func 2
and = if_ • [p_2_1, p_2_2, c_2_0]

or :: Func 2
or = if_ • [p_2_1, c_2_1, p_2_2]

not :: Func 1
not = if_ • [p_1_1, c_1_0, c_1_1]

eq :: Func 2
eq = and • [leq, geq]

lt :: Func 2
lt = not • [geq]

gt :: Func 2
gt = not • [leq]

pow :: Func 2
pow = rho (c_1_1) (mul • [p_3_2, p_3_3]) • [p_2_2, p_2_1]
