{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}

module Math.NumberTheory.QuadraticIntegers (
  QuadraticInteger(..),
  Sign(..),
  discriminant,
  conjugate,
  norm,
  trace,
  quotRemQ,
  quotQ,
  remQ,
  divModQ,
  divQ,
  modQ,
) where

import GHC.TypeNats.Compat
import Data.Proxy

infix 5 :+

--------------------------------------------------------------
-- Type level representation of sign
  
data Sign = Pos | Neg

class KnownSign (x :: Sign) where
  signVal :: proxy x -> Sign

instance KnownSign 'Pos where
  signVal _ = Pos

instance KnownSign 'Neg where
  signVal _ = Neg

-- | Takes a quadratic integer and returns its sign as an integer
signToInt :: forall s d . KnownSign s => QuadraticInteger s d -> Integer
signToInt _ =
  case signVal (Proxy :: Proxy s) of
    Pos -> 1
    Neg -> -1
  
---------------------------------------------------------------
-- Data definitions for quadratic integers

data QuadraticInteger (sign :: Sign) (d :: Nat) =
  (:+) { rationalComp :: !Integer, irrationalComp :: !Integer } deriving (Eq)


{- If the discriminant of a field is equivalent to 1 mod 4
   then the basis for the ring of integers is strictly
   larger than a + bx. Need to represent this other basis -}

data BasisType = Integral | Fractional


-- | Takes a quadratic int and returns its basis type
basisType :: forall s d . (KnownNat d, KnownSign s)
          => QuadraticInteger s d
          -> BasisType
basisType z
  | disc `mod` 4 == 1 = Fractional
  | otherwise         = Integral
  where disc = discriminant z


instance Show (QuadraticInteger s d) where
    show (a :+ b)
        | b == 0     = show a
        | a == 0     = s ++ b'
        | otherwise  = show a ++ op ++ b'
        where
            b' = if abs b == 1 then "x" else show (abs b) ++ "*x"
            op = if b > 0 then "+" else "-"
            s  = if b > 0 then "" else "-"

instance forall s d . (KnownNat d, KnownSign s) => Num (QuadraticInteger s d) where
  (+) (a :+ b) (c :+ d) = (a+c) :+ (b+d)

  (*) x@(a :+ b) (c :+ d) =
    case basisType x of
      Integral -> (a*c) + (b*d)*(discriminant x) :+ (a*d + c*b)
      Fractional -> (a*c + b*d*((discriminant x)-1) `div` 4) :+ (a*d + b*c + b*d)

  abs = undefined
  negate (a :+ b) = (-a) :+ (-b)
  fromInteger n =
    n :+ 0
  signum _ = undefined


---------------------------------------------------------------
-- Standard geometric/algebraic functions on quadratic integers


-- | Extracts the discriminant information from a quadratic integer
discriminant :: forall s d . (KnownNat d, KnownSign s)
             => QuadraticInteger s d
             -> Integer
discriminant x = sign * disc
  where disc = toInteger $ natVal (Proxy :: Proxy d)
        sign = signToInt x


-- | Returns the conjugate of a quadratic integer
conjugate :: forall s d . (KnownSign s, KnownNat d)
          => QuadraticInteger s d
          -> QuadraticInteger s d
conjugate x@(a :+ b) =
  case basisType x of
    Integral -> a :+ -b
    Fractional -> (a+b) :+ -b

-- | Computes the norm of a quadratic integer
norm :: forall s d . (KnownSign s, KnownNat d)
     => QuadraticInteger s d
     -> Integer
norm x = let (a :+ _) = x * (conjugate x) in a

trace :: forall s d . (KnownSign s, KnownNat d)
      => QuadraticInteger s d
      -> Integer
trace x = let (a :+ _) = x + (conjugate x) in a      


--------------------------------------------------------------
-- Division functionality for quadratic integers

-- |Simultaneous 'quot' and 'rem'.
quotRemQ :: forall s d . (KnownSign s, KnownNat d)
         => QuadraticInteger s d
         -> QuadraticInteger s d
         -> (QuadraticInteger s d, QuadraticInteger s d)
quotRemQ = divHelper quot

-- |Quadratic integer division, truncating toward zero.
quotQ :: forall s d . (KnownSign s, KnownNat d)
      => QuadraticInteger s d
      -> QuadraticInteger s d
      -> QuadraticInteger s d
n `quotQ` d = q where (q,_) = quotRemQ n d

-- |Quadratic integer remainder, satisfying
--
-- > (x `quotQ` y)*y + (x `remQ` y) == x
remQ :: forall s d . (KnownSign s, KnownNat d)
     => QuadraticInteger s d
     -> QuadraticInteger s d
     -> QuadraticInteger s d
n `remQ`  d = r where (_,r) = quotRemQ n d

-- |Simultaneous 'div' and 'mod'.
divModQ :: forall s d . (KnownSign s, KnownNat d)
        => QuadraticInteger s d
        -> QuadraticInteger s d
        -> (QuadraticInteger s d, QuadraticInteger s d)
divModQ = divHelper div

-- |Quadratic integer division, truncating toward negative infinity.
divQ :: forall s d . (KnownSign s, KnownNat d)
     => QuadraticInteger s d
     -> QuadraticInteger s d
     -> QuadraticInteger s d
n `divQ` d = q where (q,_) = divModQ n d

-- |Quadratic integer remainder, satisfying
--
-- > (x `divG` y)*y + (x `modG` y) == x
modQ :: forall s d . (KnownSign s, KnownNat d)
     => QuadraticInteger s d
     -> QuadraticInteger s d
     -> QuadraticInteger s d
n `modQ` d = r where (_,r) = divModQ n d

divHelper :: forall s d . (KnownSign s, KnownNat d)
          => (Integer -> Integer -> Integer)
          -> QuadraticInteger s d
          -> QuadraticInteger s d
          -> (QuadraticInteger s d, QuadraticInteger s d)
divHelper divide g h =
    let nr :+ ni = g * conjugate h
        denom = norm h
        q = divide nr denom :+ divide ni denom
        p = h * q
    in (q, g - p)
