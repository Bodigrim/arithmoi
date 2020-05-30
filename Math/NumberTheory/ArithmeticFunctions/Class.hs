-- |
-- Module:      Math.NumberTheory.ArithmeticFunctions.Class
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Generic type for arithmetic functions over arbitrary unique
-- factorisation domains.
--

module Math.NumberTheory.ArithmeticFunctions.Class
  ( ArithmeticFunction(..)
  , (&^&)
  , ($^)
  , foldFactorsWith
  ) where

import Math.NumberTheory.Primes

-- | A typical arithmetic function operates on the canonical factorisation of
-- a number into prime's powers and consists of two rules. The first one
-- determines the values of the function on the powers of primes. The second
-- one determines how to combine these values into final result.
--
-- For example, here is the totient function:
--
-- > ArithmeticFunction
-- >   { afEmpty                = 1
-- >   , afFunctionOnPrimePower = \p a -> (unPrime p - 1) * unPrime p ^ (a - 1)
-- >   , afAppend               = (*)
-- >   }
data ArithmeticFunction n a = ArithmeticFunction
  { afEmpty                :: a
    -- ^ value of a function on 1
  , afFunctionOnPrimePower :: Prime n -> Word -> a
    -- ^ how to evaluate a function on prime powers
  , afAppend               :: a -> a -> a
    -- ^ how to combine values of a function on coprime arguments
  }

(&^&) :: ArithmeticFunction n a -> ArithmeticFunction n b -> ArithmeticFunction n (a, b)
ArithmeticFunction e1 f1 a1 &^& ArithmeticFunction e2 f2 a2 = ArithmeticFunction
  { afEmpty = (e1, e2)
  , afFunctionOnPrimePower = \p k -> (f1 p k, f2 p k)
  , afAppend = \(x1, x2) (y1, y2) -> (x1 `a1` y1, x2 `a2` y2)
  }

infixr 0 $^

-- | Convert to a function. The value on 0 is undefined.
($^) :: UniqueFactorisation n => ArithmeticFunction n a -> n -> a
($^) f = foldFactorsWith f . factorise

-- | Convert to a function on prime factorisation.
foldFactorsWith :: ArithmeticFunction n a -> [(Prime n, Word)] -> a
foldFactorsWith (ArithmeticFunction empty f append)
  = foldl append empty . map (uncurry f)
