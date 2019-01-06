-- |
-- Module:      Math.NumberTheory.DirichletCharacters
-- Copyright:   (c) 2018 Bhavik Mehta
-- Licence:     MIT
-- Maintainer:  Bhavik Mehta <bhavikmehta8@gmail.com>
-- Stability:   Provisional
-- Portability: Non-portable (GHC extensions)
--
-- Implementation and enumeration of Dirichlet characters.
--

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module Math.NumberTheory.DirichletCharacters
  (
  -- * Roots of unity
    RootOfUnity
  -- ** Conversions
  , toRootOfUnity
  , fromRootOfUnity
  , toComplex
  -- * Dirichlet characters
  , DirichletCharacter
  , evaluate
  , generalEval
  , toFunction
  , indexToChar
  , indicesToChars
  , characterNumber
  , allChars
  -- ** Special Dirichlet characters
  , principalChar
  , isPrincipal
  , induced
  -- ** Real Dirichlet characters
  , RealCharacter
  , isRealCharacter
  , getRealChar
  , toRealFunction
  , jacobiCharacter
  -- * Debugging
  , validChar
  ) where

import Data.Bits                                           (Bits(..))
import Data.Complex                                        (Complex, cis)
import Data.Functor.Identity                               (Identity(..))
import Data.List                                           (mapAccumL, foldl')
import Data.Proxy                                          (Proxy(..))
import Data.Ratio                                          (Rational, Ratio, (%), numerator, denominator)
import Data.Semigroup                                      (Semigroup(..), Product(..))
import GHC.TypeNats.Compat                                 (Nat, natVal)
import Numeric.Natural                                     (Natural)

import Math.NumberTheory.ArithmeticFunctions               (totient)
import Math.NumberTheory.Moduli.Class                      (KnownNat, MultMod(..), getVal, Mod, isMultElement)
import Math.NumberTheory.Moduli.DiscreteLogarithm.Internal (discreteLogarithmPP)
import Math.NumberTheory.Moduli.PrimitiveRoot              (isPrimitiveRoot', CyclicGroup(..))
import Math.NumberTheory.Powers                            (powMod)
import Math.NumberTheory.Primes                            (primes)
import Math.NumberTheory.UniqueFactorisation               (UniqueFactorisation, unPrime, Prime, factorise)
import Math.NumberTheory.Utils.FromIntegral                (wordToInt)

-- | A Dirichlet character mod \(n\) is a group homomorphism from \((\mathbb{Z}/n\mathbb{Z})^*\)
-- to \(\mathbb{C}^*\), represented abstractly by `DirichletCharacter`. In particular, they take
-- values at roots of unity and can be evaluated using `evaluate`.
-- A Dirichlet character can be extended to a completely multiplicative function on \(\mathbb{Z}\)
-- by assigning the value 0 for \(a\) sharing a common factor with \(n\), using `generalEval`.
--
-- There are finitely many possible Dirichlet characters for a given modulus, in particular there
-- are \(\phi(n)\) characters modulo \(n\), where \(\phi\) refers to Euler's `totient` function.
-- This gives rise to `Enum` and `Bounded` instances.
newtype DirichletCharacter (n :: Nat) = Generated [DirichletFactor]

-- | The group (Z/nZ)^* decomposes to a product (Z/2^k0 Z)^* x (Z/p1^k1 Z)^* x ... x (Z/pi^ki Z)^*
-- where n = 2^k0 p1^k1 ... pi^ki, and the pj are odd primes, k0 possibly 0. Thus, a group
-- homomorphism from (Z/nZ)^* is characterised by group homomorphisms from each of these factor
-- groups. Furthermore, for odd p, we have (Z/p^k Z)^* isomorphic to Z / p^(k-1)*(p-1) Z, an
-- additive group, where an isomorphism is specified by a choice of primitive root.
-- Similarly, for k >= 2, (Z/2^k Z)^* is isomorphic to Z/2Z * (Z / 2^(k-2) Z) (and for k < 2
-- it is trivial).  (See `lambda` for this isomorphism).
-- Thus, to specify a Dirichlet character, it suffices to specify the value of generators
-- of each of these cyclic groups, when primitive roots are given. This data is given by a
-- DirichletFactor.
-- We have the invariant that the factors must be given in strictly increasing order, and the
-- generator is as given by `generator`, and are each non-trivial. These conditions are verified
-- using `validChar`.
data DirichletFactor = OddPrime { _getPrime :: Prime Natural
                                , _getPower :: Word
                                , _getGenerator :: Natural
                                , _getValue :: RootOfUnity
                                }
                     | TwoPower { _getPower2 :: Int -- this ought to be Word, but many applications
                                                    -- needed to use wordToInt, so Int is cleaner
                                , _getFirstValue :: RootOfUnity
                                , _getSecondValue :: RootOfUnity
                                }

instance Eq (DirichletCharacter n) where
  Generated a == Generated b = a == b

instance Eq DirichletFactor where
  TwoPower _ x1 x2 == TwoPower _ y1 y2 = x1 == y1 && x2 == y2
  OddPrime _ _ _ x == OddPrime _ _ _ y = x == y
  _ == _ = False

-- | A representation of <https://en.wikipedia.org/wiki/Root_of_unity roots of unity>: complex
-- numbers \(z\) for which there is \(n\) such that \(z^n=1\).
newtype RootOfUnity =
  RootOfUnity { -- | Every root of unity can be expressed as \(e^{2 \pi i q}\) for some
                -- rational \(q\) satisfying \(0 \leq q < 1\), this function extracts it.
                fromRootOfUnity :: Rational }
  deriving (Eq)

instance Show RootOfUnity where
  show (RootOfUnity q)
    | n == 0    = "e^0"
    | d == 1    = "e^(πi)"
    | n == 1    = "e^(πi/" ++ show d ++ ")"
    | otherwise = "e^(" ++ show n ++ "πi/" ++ show d ++ ")"
    where n = numerator (2*q)
          d = denominator (2*q)

-- | Given a rational \(q\), produce the root of unity \(e^{2 \pi i q}\).
toRootOfUnity :: Integral a => Ratio a -> RootOfUnity
toRootOfUnity q = RootOfUnity ((n `rem` d) % d)
  where n = toInteger $ numerator q
        d = toInteger $ denominator q
        -- effectively q `mod` 1
  -- This smart constructor ensures that the rational is always in the range 0 <= q < 1.

-- | This Semigroup is in fact a group, so @stimes@ can be called with a negative first argument.
instance Semigroup RootOfUnity where
  RootOfUnity q1 <> RootOfUnity q2 = toRootOfUnity (q1 + q2)
  stimes k (RootOfUnity q) = toRootOfUnity (q * fromIntegral k)

instance Monoid RootOfUnity where
  mappend = (<>)
  mempty = RootOfUnity 0

-- | Convert a root of unity into an inexact complex number. Due to floating point inaccuracies,
-- it is recommended to avoid use of this until the end of a calculation. Alternatively, with
-- the [cyclotomic](http://hackage.haskell.org/package/cyclotomic-0.5.1) package, one can use
-- @[polarRat](https://hackage.haskell.org/package/cyclotomic-0.5.1/docs/Data-Complex-Cyclotomic.html#v:polarRat)
-- 1 . @'fromRootOfUnity' to convert to a cyclotomic number.
toComplex :: Floating a => RootOfUnity -> Complex a
toComplex = cis . (2*pi*) . fromRational . fromRootOfUnity

-- | For primes, define the canonical primitive root as the smallest such. For prime powers \(p^k\),
-- either the smallest primitive root \(g\) mod \(p\) works, or \(g+p\) works.
generator :: (Integral a, UniqueFactorisation a) => Prime a -> Word -> a
generator p k
  | k == 1 = modP
  | otherwise = if powMod modP (p'-1) (p'*p') == 1 then modP + p' else modP
  where p' = unPrime p
        modP = head $ filter (isPrimitiveRoot' (CGOddPrimePower p 1)) [2..p'-1]

-- | Implement the function \(\lambda\) from page 5 of
-- https://www2.eecs.berkeley.edu/Pubs/TechRpts/1984/CSD-84-186.pdf
lambda :: Integer -> Int -> Integer
lambda x e = ((powMod x (2*modulus) largeMod - 1) `shiftR` (e+1)) .&. (modulus - 1)
  where modulus = bit (e-2)
        largeMod = bit (2*e - 1)

-- | For elements of the multiplicative group \((\mathbb{Z}/n\mathbb{Z})^*\), a Dirichlet
-- character evaluates to a root of unity.
evaluate :: DirichletCharacter n -> MultMod n -> RootOfUnity
evaluate (Generated ds) m = foldMap (evalFactor m') ds
  where m' = getVal $ multElement m

-- | Evaluate each factor of the Dirichlet character.
evalFactor :: Integer -> DirichletFactor -> RootOfUnity
evalFactor m =
  \case
    OddPrime (toInteger . unPrime -> p) k (toInteger -> a) b ->
      discreteLogarithmPP p k a (m `rem` p^k) `stimes` b
    TwoPower k s b -> (if testBit m 1 then s else mempty)
                   <> lambda m'' k `stimes` b
                                       where m' = m .&. kBits
                                             m'' = if testBit m 1
                                                      then bit k - m'
                                                      else m'
                                             kBits = bit k - 1

-- | A character can evaluate to a root of unity or zero: represented by @Nothing@.
generalEval :: KnownNat n => DirichletCharacter n -> Mod n -> Maybe RootOfUnity
generalEval chi = fmap (evaluate chi) . isMultElement

-- | Convert a Dirichlet character to a complex-valued function. As in `toComplex`, the result is
-- inexact due to floating-point inaccuracies. See `toComplex` for more.
toFunction :: (Integral a, RealFloat b, KnownNat n) => DirichletCharacter n -> a -> Complex b
toFunction chi = maybe 0 toComplex . generalEval chi . fromIntegral

-- | Give the principal character for this modulus: a principal character mod \(n\) is 1 for
-- \(a\) coprime to \(n\), and 0 otherwise.
principalChar :: KnownNat n => DirichletCharacter n
principalChar = minBound

mulChars :: DirichletCharacter n -> DirichletCharacter n -> DirichletCharacter n
mulChars (Generated x) (Generated y) = Generated (zipWith combine x y)
  where combine :: DirichletFactor -> DirichletFactor -> DirichletFactor
        combine (OddPrime p k g n) (OddPrime _ _ _ m) =
          OddPrime p k g (n <> m)
        combine (TwoPower k a n) (TwoPower _ b m) =
          TwoPower k (a <> b) (n <> m)
        combine _ _ = error "internal error: malformed DirichletCharacter"

-- TODO: this semigroup is also a group, allow `stimes` to work for non-positives too
instance Semigroup (DirichletCharacter n) where
  (<>) = mulChars

instance KnownNat n => Monoid (DirichletCharacter n) where
  mempty = principalChar
  mappend = (<>)

-- | We define `succ` and `pred` with more efficient implementations than
-- @`toEnum` . (+1) . `fromEnum`@.
instance KnownNat n => Enum (DirichletCharacter n) where
  toEnum = indexToChar
  fromEnum = fromIntegral . characterNumber
  succ x = makeChar x (characterNumber x + 1)
  pred x = makeChar x (characterNumber x - 1)

  enumFromTo x y       = bulkMakeChars x [fromEnum x..fromEnum y]
  enumFrom x           = bulkMakeChars x [fromEnum x..]
  enumFromThenTo x y z = bulkMakeChars x [fromEnum x, fromEnum y..fromEnum z]
  enumFromThen x y     = bulkMakeChars x [fromEnum x, fromEnum y..]

instance KnownNat n => Bounded (DirichletCharacter n) where
  minBound = indexToChar (0 :: Int)
  maxBound = indexToChar (totient n - 1)
    where n = natVal (Proxy :: Proxy n)

-- | We have a (non-canonical) enumeration of dirichlet characters.
characterNumber :: DirichletCharacter n -> Integer
characterNumber (Generated y) = foldl' go 0 y
  where go x (OddPrime p k _ a) = x * m + numerator (fromRootOfUnity a * (m % 1))
          where p' = fromIntegral (unPrime p)
                m = p'^(k-1)*(p'-1)
        go x (TwoPower k a b)   = x' * 2 + numerator (fromRootOfUnity a * (2 % 1))
          where m = bit (k-2) :: Integer
                x' = x `shiftL` (k-2) + numerator (fromRootOfUnity b * fromIntegral m)

-- | Give the dirichlet character from its number.
-- Inverse of `characterNumber`.
indexToChar :: forall n a. (KnownNat n, Integral a) => a -> DirichletCharacter n
indexToChar = runIdentity . indicesToChars . Identity

-- | Give a collection of dirichlet characters from their numbers. This may be more efficient than
-- `indexToChar` for multiple characters, as it prevents some internal recalculations.
indicesToChars :: forall n a f. (KnownNat n, Integral a, Functor f) => f a -> f (DirichletCharacter n)
indicesToChars = fmap (Generated . unroll t . (`mod` m) . fromIntegral)
  where n = natVal (Proxy :: Proxy n)
        (Product m, t) = mkTemplate n

-- | List all characters for the modulus. This is preferred to using @[minBound..maxBound]@.
allChars :: forall n. (KnownNat n) => [DirichletCharacter n]
allChars = indicesToChars [0..m-1]
  where m = totient $ natVal (Proxy :: Proxy n)

makeChar :: (Integral a) => DirichletCharacter n -> a -> DirichletCharacter n
makeChar x = runIdentity . bulkMakeChars x . Identity

-- use one character to make many more: better than indicestochars since it avoids recalculating
-- some primitive roots
bulkMakeChars :: (Integral a, Functor f) => DirichletCharacter n -> f a -> f (DirichletCharacter n)
bulkMakeChars x = fmap (Generated . unroll t . (`mod` m) . fromIntegral)
  where (Product m, t) = templateFromCharacter x

-- We assign each natural a unique Template, which can be decorated (eg in `unroll`) to
-- form a DirichletCharacter. A Template effectively holds the information carried around
-- in a DirichletFactor which depends only on the modulus of the character.
data Template = OddTemplate { _getPrime'     :: Prime Natural
                            , _getPower'     :: Word
                            , _getGenerator' :: !Natural
                            , _getModulus'   :: !Natural
                            }
              | TwoTemplate { _getPower2'    :: Int
                            , _getModulus'   :: !Natural
                            } -- the modulus is derivable from the other values, but calculation
                              -- may be expensive, so we pre-calculate it
                              -- morally getModulus should be a prefactored but seems to be
                              -- pointless here

templateFromCharacter :: DirichletCharacter n -> (Product Natural, [Template])
templateFromCharacter (Generated t) = mapM go t
  where go (OddPrime p k g _) = (Product m, OddTemplate p k g m)
          where p' = unPrime p
                m = p'^(k-1)*(p'-1)
        go (TwoPower k _ _) = (Product (2*m), TwoTemplate k m)
          where m = bit (k-2)

-- TODO (idea): Template is effectively a CyclicFactor of a generalised CyclicGroup...
-- see issue #154

mkTemplate :: Natural -> (Product Natural, [Template])
mkTemplate = go . factorise
  where go :: [(Prime Natural, Word)] -> (Product Natural, [Template])
        go ((unPrime -> 2, 1):xs) = mapM odds xs
        go ((unPrime -> 2, wordToInt -> k):xs) = (Product (2*m), [TwoTemplate k m]) <> mapM odds xs
          where m = bit (k-2)
        go xs = mapM odds xs
        odds :: (Prime Natural, Word) -> (Product Natural, Template)
        odds (p, k) = (Product m, OddTemplate p k (generator p k) m)
          where p' = unPrime p
                m = p'^(k-1)*(p'-1)

-- the validity of the producted dirichletfactor list here requires the template to be valid
unroll :: [Template] -> Natural -> [DirichletFactor]
unroll t m = snd (mapAccumL func m t)
  where func :: Natural -> Template -> (Natural, DirichletFactor)
        func a (OddTemplate p k g n) = (a1, OddPrime p k g (toRootOfUnity $ a2 % n))
          where (a1,a2) = quotRem a n
        func a (TwoTemplate k n) = (b1, TwoPower k (toRootOfUnity $ a2 % 2) (toRootOfUnity $ b2 % n))
          where (a1,a2) = quotRem a 2
                (b1,b2) = quotRem a1 n
                -- TODO: consider tidying

-- | Test if a given Dirichlet character is prinicpal for its modulus: a principal character mod
-- \(n\) is 1 for \(a\) coprime to \(n\), and 0 otherwise.
isPrincipal :: DirichletCharacter n -> Bool
isPrincipal chi = characterNumber chi == 0

-- | Induce a Dirichlet character to a higher modulus. If \(d \mid n\), then \(a \bmod{n}\) can be
-- reduced to \(a \bmod{d}\). Thus, a multiplicative function on \(\mathbb{Z}/d\mathbb{Z}\)
-- induces a multiplicative function on \(\mathbb{Z}/n\mathbb{Z}\).
--
-- >>> :set -XTypeApplications
-- >>> chi = fromIndex 5 :: DirichletCharacter 45
-- >>> chi2 = induced @135 chi
-- >>> :t chi2
-- Maybe (DirichletCharacter 135)
--
induced :: forall n d. (KnownNat d, KnownNat n) => DirichletCharacter d -> Maybe (DirichletCharacter n)
induced (Generated start) = if n `rem` d == 0
                            then Just (Generated (combine n' start))
                            else Nothing
  where n = natVal (Proxy :: Proxy n)
        d = natVal (Proxy :: Proxy d)
        n' = factorise n
        combine :: [(Prime Natural, Word)] -> [DirichletFactor] -> [DirichletFactor]
        combine [] _ = []
        combine t [] = plain t
        combine ((p1,k1):xs) (y:ys)
          -- TODO: consider tidying
          | unPrime p1 == 2, TwoPower _ a b <- y = TwoPower (wordToInt k1) a b: combine xs ys
          | OddPrime p2 1 _g a <- y, p1 == p2 =
                             OddPrime p2 k1 (generator p2 k1) a: combine xs ys
            -- TODO: generator p2 k1 will be g or g + p2, and we already know g is a primroot mod p
            -- so should be able to save work instead of running generator
          | OddPrime p2 _ g a <- y, p1 == p2 =
                             OddPrime p2 k1 g a: combine xs ys
          | unPrime p1 == 2, k1 >= 2 = TwoPower (wordToInt k1) mempty mempty: combine xs (y:ys)
          | unPrime p1 == 2 = combine xs (y:ys)
          | otherwise = OddPrime p1 k1 (generator p1 k1) mempty: combine xs (y:ys)
        plain :: [(Prime Natural, Word)] -> [DirichletFactor]
        plain [] = []
        plain f@((p,k):xs) = case (unPrime p, k) of
                               (2,1) -> map rest xs
                               (2,_) -> TwoPower (wordToInt k) mempty mempty: map rest xs
                               _ -> map rest f
        rest :: (Prime Natural, Word) -> DirichletFactor
        rest (p,k) = OddPrime p k (generator p k) mempty

-- | The <https://en.wikipedia.org/wiki/Jacobi_symbol Jacobi symbol> gives a real Dirichlet
-- character for odd moduli.
jacobiCharacter :: forall n. KnownNat n => Maybe (RealCharacter n)
jacobiCharacter = if odd n
                     then Just $ RealChar $ Generated $ map go $ snd $ mkTemplate n
                     else Nothing
  where n = natVal (Proxy :: Proxy n)
        go :: Template -> DirichletFactor
        go TwoTemplate{} = error "internal error in jacobiCharacter: please report this as a bug"
          -- every factor of n should be odd
        go (OddTemplate p k g _) = OddPrime p k g $ toRootOfUnity (k % 2)
          -- jacobi symbol of a primitive root mod p over p is always -1

-- | A Dirichlet character is real if it is real-valued.
newtype RealCharacter n = RealChar { -- | Extract the character itself from a `RealCharacter`.
                                     getRealChar :: DirichletCharacter n
                                   }

-- | Test if a given `DirichletCharacter` is real, and if so give a `RealCharacter`.
isRealCharacter :: DirichletCharacter n -> Maybe (RealCharacter n)
isRealCharacter t@(Generated xs) = if all real xs then Just (RealChar t) else Nothing
  where real :: DirichletFactor -> Bool
        real (OddPrime _ _ _ a) = a <> a == mempty
        real (TwoPower _ _ b) = b <> b == mempty

-- TODO: it should be possible to calculate this without evaluate/generalEval
-- and thus avoid using discrete log calculations: consider the order of m
-- inside each of the factor groups?
-- | Evaluate a real Dirichlet character, which can only take values \(-1,0,1\).
toRealFunction :: KnownNat n => RealCharacter n -> Natural -> Int
toRealFunction (RealChar chi) m = case generalEval chi (fromIntegral m) of
                                    Nothing -> 0
                                    Just t | t == mempty -> 1
                                    Just t | t == RootOfUnity (1 % 2) -> -1
                                    _ -> error "internal error in toRealFunction: please report this as a bug"
                                      -- A real character should not be able to evaluate to
                                      -- anything other than {-1,0,1}, so should not reach this branch

-- | Test if the internal DirichletCharacter structure is valid.
validChar :: forall n. KnownNat n => DirichletCharacter n -> Bool
validChar (Generated xs) = correctDecomposition && all correctPrimitiveRoot xs
  where correctDecomposition = removeTwo (factorise n) == map getPP xs
        getPP (TwoPower k _ _) = (two, fromIntegral k)
        getPP (OddPrime p k _ _) = (p, k)
        removeTwo ((unPrime -> 2,1):ys) = ys
        removeTwo ys = ys
        correctPrimitiveRoot TwoPower{} = True
        correctPrimitiveRoot (OddPrime p k g _) = g == generator p k
        n = natVal (Proxy :: Proxy n)
        two = head primes -- lazy way to get Prime 2
