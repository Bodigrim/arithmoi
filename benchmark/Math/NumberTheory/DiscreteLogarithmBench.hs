{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.DiscreteLogarithmBench
  ( benchSuite
  , rangeCases
  , discreteLogarithm'
  ) where

import Gauge.Main
import Data.Maybe
import GHC.TypeNats.Compat
import Data.Proxy
import Numeric.Natural

import Math.NumberTheory.Moduli.Class (isMultElement, KnownNat, MultMod, multElement, getVal, getMod, Mod)
import Math.NumberTheory.Moduli.DiscreteLogarithm (discreteLogarithm)
import Math.NumberTheory.Moduli.PrimitiveRoot (PrimitiveRoot, isPrimitiveRoot, unPrimitiveRoot, cyclicGroupFromModulo)

data Case = forall m. KnownNat m => Case (PrimitiveRoot m) (MultMod m) String

instance Show Case where
  show (Case a b s) = concat [show (getVal a'), "ⁿ == ", show b', " mod ", s]
    where a' = multElement $ unPrimitiveRoot a
          b' = getVal $ multElement b

makeCase :: (Integer, Integer, Natural, String) -> Maybe Case
makeCase (a,b,n,s) =
  case someNatVal n of
    SomeNat (_ :: Proxy m) ->
      Case <$> isPrimitiveRoot a' <*> isMultElement b' <*> pure s
        where a' = fromInteger a :: Mod m
              b' = fromInteger b

cases :: [Case]
cases = mapMaybe makeCase [ (5,  8,  10^9 + 7,  "10^9 + 7")
                          , (2,  7,    3^1000,    "3^1000")
                          , (2,  3, 10^11 + 3, "10^11 + 3")
                          , (3, 17,     5^700,     "5^700")
                          ]

rangeCases :: Natural -> Int -> [Case]
rangeCases start num = take num $ do
  n <- [start..]
  _cg <- maybeToList $ cyclicGroupFromModulo n
  case someNatVal n of
    SomeNat (_ :: Proxy m) -> do
      a <- take 1 $ mapMaybe isPrimitiveRoot [2 :: Mod m .. maxBound]
      b <- take 1 $ filter (/= unPrimitiveRoot a) $ mapMaybe isMultElement [2 .. maxBound]
      return $ Case a b (show n)

discreteLogarithm' :: Case -> Natural
discreteLogarithm' (Case a b _) = discreteLogarithm a b

benchSuite :: Benchmark
benchSuite = bgroup "Discrete logarithm"
  [ bgroup "individual case"
          [ bench (show c) $ nf discreteLogarithm' c | c <- cases]
  , bgroup "range"
          [ bench (show num ++ " cases near " ++ show n) $ nf (map discreteLogarithm') $ rangeCases n num
            | (n, num) <- [(10000, 100), (1000000, 100), (100000000, 100), (10000000000, 100)]
          ]
  ]
