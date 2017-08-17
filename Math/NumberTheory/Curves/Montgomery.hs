-- |
-- Module:      Math.NumberTheory.Curves.Montgomery
-- Copyright:   (c) 2017 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Arithmetic on Montgomery elliptic curves.
-- This is an internal module, exposed only for purposes of testing.
--

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_HADDOCK hide #-}

module Math.NumberTheory.Curves.Montgomery
  ( Point
  , pointX
  , pointZ
  , pointN
  , pointA24
  , SomePoint(..)
  , newPoint
  , add
  , double
  , multiply
  , multiplyNew
  ) where

import Data.Proxy
import GHC.Exts
import GHC.Integer.Logarithms
import GHC.TypeNats.Compat

import Math.NumberTheory.Utils (recipMod)

-- | We use the Montgomery form of elliptic curve:
-- b Y² = X³ + a X² + X (mod n).
-- See Eq. (10.3.1.1) at p. 260 of <http://www.ams.org/journals/mcom/1987-48-177/S0025-5718-1987-0866113-7/S0025-5718-1987-0866113-7.pdf Speeding the Pollard and Elliptic Curve Methods of Factorization> by P. L. Montgomery.
--
-- Switching to projective space by substitutions Y = y \/ z, X = x \/ z,
-- we get b y² z = x³ + a x² z + x z² (mod n).
-- The point on projective elliptic curve is characterized by three coordinates,
-- but it appears that only x- and z-components matter for computations.
-- By the same reason there is no need to store coefficient b.
--
-- That said, the chosen curve is represented by a24 = (a + 2) \/ 4
-- and modulo n at type level, making points on different curves
-- incompatible.
data Point (a24 :: Nat) (n :: Nat) = Point
  { pointX :: !Integer -- ^ Extract x-coordinate.
  , pointZ :: !Integer -- ^ Extract z-coordinate.
  }

-- | Extract (a + 2) \/ 4, where a is a coefficient in curve's equation.
pointA24 :: forall a24 n. KnownNat a24 => Point a24 n -> Integer
pointA24 _ = toInteger $ natVal (Proxy :: Proxy a24)

-- | Extract modulo of the curve.
pointN :: forall a24 n. KnownNat n => Point a24 n -> Integer
pointN _ = toInteger $ natVal (Proxy :: Proxy n)

-- | In projective space 'Point's are equal, if they are both at infinity
-- or if respective ratios 'pointX' \/ 'pointZ' are equal.
instance KnownNat n => Eq (Point a24 n) where
  Point _ 0 == Point _ 0 = True
  Point _ 0 == _         = False
  _         == Point _ 0 = False
  p@(Point x1 z1) == Point x2 z2 = let n = pointN p in (x1 * z2 - x2 * z1) `rem` n == 0

-- | For debugging.
instance (KnownNat a24, KnownNat n) => Show (Point a24 n) where
  show p = "(" ++ show (pointX p) ++ ", " ++ show (pointZ p) ++ ") (a24 "
    ++ show (pointA24 p) ++ ", mod "
    ++ show (pointN p) ++ ")"

-- | Point on unknown curve.
data SomePoint where
  SomePoint :: (KnownNat a24, KnownNat n) => Point a24 n -> SomePoint

instance Show SomePoint where
  show (SomePoint p) = show p

-- | 'newPoint' @s@ @n@ creates a point on an elliptic curve modulo @n@, uniquely determined by seed @s@.
-- Some choices of @s@ and @n@ produce ill-parametrized curves, which is reflected by return value 'Nothing'.
--
-- We choose a curve by Suyama's parametrization. See Eq. (3)-(4) at p. 4
-- of <http://www.hyperelliptic.org/tanja/SHARCS/talks06/Gaj.pdf Implementing the Elliptic Curve Method of Factoring in Reconfigurable Hardware>
-- by K. Gaj, S. Kwon et al.
newPoint :: Integer -> Integer -> Maybe SomePoint
newPoint s n = do
  _ <- if n > 1 then Just () else Nothing
  a24denRecip <- recipMod a24den n
  a24 <- case a24num * a24denRecip `rem` n of
    -- (a+2)/4 = 0 corresponds to singular curve with A = -2
    0 -> Nothing
    -- (a+2)/4 = 1 corresponds to singular curve with A = 2
    1 -> Nothing
    t -> Just t
  SomeNat (_ :: Proxy a24Ty) <- if a24 < 0
                                then Nothing
                                else Just $ someNatVal $ fromInteger a24
  SomeNat (_ :: Proxy nTy)   <- if n < 0
                                then Nothing
                                else Just $ someNatVal $ fromInteger n
  return $ SomePoint (Point x z :: Point a24Ty nTy)
  where
    u = s * s `rem` n - 5
    v = 4 * s
    d = v - u
    x = u * u * u `mod` n
    z = v * v * v `mod` n
    a24num = d * d * d * (3 * u + v) `mod` n
    a24den = 16 * x * v `rem` n

infinitePoint :: Point a24 n
infinitePoint = Point 0 0

-- | If @p0@ + @p1@ = @p2@, then 'add' @p0@ @p1@ @p2@ equals to @p1@ + @p2@.
-- It is also required that z-coordinates of @p0@, @p1@ and @p2@ are coprime with modulo
-- of elliptic curve; and x-coordinate of @p0@ is non-zero.
-- If preconditions do not hold, return value is undefined.
--
-- Remarkably such addition does not require 'KnownNat' @a24@ constraint.
--
-- Computations follow Algorithm 3 at p. 4
-- of <http://www.hyperelliptic.org/tanja/SHARCS/talks06/Gaj.pdf Implementing the Elliptic Curve Method of Factoring in Reconfigurable Hardware>
-- by K. Gaj, S. Kwon et al.
add :: (KnownNat a24, KnownNat n) => Point a24 n -> Point a24 n -> Point a24 n -> Point a24 n
add (Point 0 0) _ p2 = double p2
add _ (Point 0 0) p2 = p2
add p0@(Point x0 z0) (Point x1 z1) (Point x2 z2) = Point x3 z3
  where
    n = pointN p0
    a = (x1 - z1) * (x2 + z2) `rem` n
    b = (x1 + z1) * (x2 - z2) `rem` n
    apb = a + b
    amb = a - b
    c = apb * apb `rem` n
    d = amb * amb `rem` n
    x3 = c * z0 `mod` n
    z3 = d * x0 `mod` n

-- | Multiply by 2.
--
-- Computations follow Algorithm 3 at p. 4
-- of <http://www.hyperelliptic.org/tanja/SHARCS/talks06/Gaj.pdf Implementing the Elliptic Curve Method of Factoring in Reconfigurable Hardware>
-- by K. Gaj, S. Kwon et al.
double :: (KnownNat a24, KnownNat n) => Point a24 n -> Point a24 n
double p@(Point x z) = Point x' z'
  where
    n = pointN p
    a24 = pointA24 p
    r = x + z
    s = x - z
    u = r * r `rem` n
    v = s * s `rem` n
    t = u - v
    x' = u * v `mod` n
    z' = (v + a24 * t `rem` n) * t `mod` n

-- | Multiply by 3.
triple :: (KnownNat a24, KnownNat n) => Point a24 n -> Point a24 n
triple p = add p p (double p)

-- | Multiply by given number, using PRAC algorithm from
-- <https://cr.yp.to/bib/1992/montgomery-lucas.pdf Evaluating recurrences of form X_{m+n} = f(X_m, X_n, X_{m-n}) via Lucas chains>
-- by P. L. Montgomery.
multiplyNew :: (KnownNat a24, KnownNat n) => Word -> Point a24 n -> Point a24 n
multiplyNew 0 _ = infinitePoint
multiplyNew 1 a = a
multiplyNew d a = uncurry multiplyNew $ go (r, d - r, a, a, infinitePoint)
  where
    phi :: Double
    phi = (1 + sqrt 5) / 2
    r :: Word
    r = round (fromIntegral d / phi)


go
  :: (KnownNat a24, KnownNat n)
  => (Word, Word, Point a24 n, Point a24 n, Point a24 n)
  -> (Word, Point a24 n)
go (d, e, a, b, c)
  | d == e    = (d, add c b a)
  | otherwise = go $ table4 $
                if d < e
                  then (e, d, b, a, applyx_1 c)
                  else (d, e, a, b, c)

table4
  :: (KnownNat a24, KnownNat n)
  => (Word, Word, Point a24 n, Point a24 n, Point a24 n)
  -> (Word, Word, Point a24 n, Point a24 n, Point a24 n)
table4 (d, e, a, b, c)
  -- | 4 * d <= 5 * e && (d + e) `rem` 3 == 0
  --   = let t = add c b a in
  --     ((2 * d - e) `quot` 3, (2 * e - d) `quot` 3, add b a t, add a b t, c)

  -- | 4 * d <= 5 * e && d `rem` 6 == e `rem` 6
  --   = ((d - e) `quot` 2, e, double a, add c b a, c)

  -- | d <= 4 * e
  --   = (d - e, e, a, add c b a, applyx_1 b)

  | d `rem` 2 == e `rem` 2
    = ((d - e) `quot` 2, e, double a, add c b a, c)

  | d `rem` 2 == 0
    = (d `quot` 2, e, double a, b, add b c a)

  -- | d `rem` 3 == 0
  --   = let t1 = double a in
  --     let t2 = add c b a in
  --     (d `quot` 3 - e, e, add a a t1, add c t2 t1, applyx_1 b)

  -- | (d + e) `rem` 3 == 0
  --   = let t1 = add c b a in
  --     ((d - 2 * e) `quot` 3, e, triple a, add b a t1, c)

  -- | d `rem` 3 == e `rem` 3
  --   = let t1 = add c b a in
  --     let t2 = add b c a in
  --     ((d - e) `quot` 3, e, triple a, t1, t2)

  | e `rem` 2 == 0
    = (d, e `quot` 2, a, double b, add a (applyx_1 b) c)

applyx_1 :: Point a24 n -> Point a24 n
applyx_1 (Point x z) = Point x z

multiply :: (KnownNat a24, KnownNat n) => Word -> Point a24 n -> Point a24 n
multiply 0 _ = infinitePoint
multiply 1 p = p
multiply (W# w##) p =
    case wordLog2# w## of
      l# -> go (l# -# 1#) p (double p)
  where
    go 0# !p0 !p1 = case w## `and#` 1## of
                      0## -> double p0
                      _   -> add p p0 p1
    go i# p0 p1 = case uncheckedShiftRL# w## i# `and#` 1## of
                    0## -> go (i# -# 1#) (double p0) (add p p0 p1)
                    _   -> go (i# -# 1#) (add p p0 p1) (double p1)
