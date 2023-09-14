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

add :: Func
add = rho (p 1 1) (s • [p 3 2])

double :: Func
double = add • [p 1 1, p 1 1]

mul :: Func
mul = rho (c 1 0) (add • [p 3 2, p 3 3])

pred :: Func
pred = rho (c 0 0) (p 2 1)

sub :: Func
sub = rsub • [p 2 2, p 2 1]
  where
    rsub = rho (p 1 1) (pred • [p 3 2])

true :: PN
true = S Z

false :: PN
false = Z

isZero :: Func
isZero = rho (c 0 1) (c 2 0)

leq :: Func
leq = isZero • [sub]

geq :: Func
geq = leq • [p 2 2, p 2 1]

if_ :: Func
if_ = rho (p 2 2) (p 4 3)

and :: Func
and = if_ • [p 2 1, p 2 2, c 2 0]

or :: Func
or = if_ • [p 2 1, c 2 1, p 2 2]

not :: Func
not = if_ • [p 1 1, c 1 0, c 1 1]

eq :: Func
eq = and • [leq, geq]

lt :: Func
lt = not • [geq]

gt :: Func
gt = not • [leq]

pow :: Func
pow = rho (c 1 1) (mul • [p 3 2, p 3 3]) • [p 2 2, p 2 1]