{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.DiscreteLogarithmBench
  ( benchSuite
  , rangeCases
  , discreteLogarithm'
  ) where

import Gauge.Main
import Control.Monad
import Data.Maybe
import Data.Mod
import GHC.TypeNats.Compat
import Data.Proxy
import Numeric.Natural

import Math.NumberTheory.Moduli.Multiplicative
import Math.NumberTheory.Moduli.Singleton

data Case = forall m. KnownNat m => Case (PrimitiveRoot m) (MultMod m) String

instance Show Case where
  show (Case a b s) = concat [show (unMod a'), "ⁿ == ", show b', " mod ", s]
    where a' = multElement $ unPrimitiveRoot a
          b' = unMod $ multElement b

makeCase :: (Integer, Integer, Natural, String) -> Maybe Case
makeCase (a,b,n,s) =
  case someNatVal n of
    SomeNat (_ :: Proxy m) ->
      Case <$> join (isPrimitiveRoot @Integer <$> cyclicGroup <*> pure a') <*> isMultElement b' <*> pure s
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
  case someNatVal n of
    SomeNat (_ :: Proxy m) -> case cyclicGroup :: Maybe (CyclicGroup Integer m) of
      Nothing -> []
      Just cg -> do
        a <- take 1 $ mapMaybe (isPrimitiveRoot cg) [2 :: Mod m .. maxBound]
        b <- take 1 $ filter (/= unPrimitiveRoot a) $ mapMaybe isMultElement [2 .. maxBound]
        return $ Case a b (show n)

discreteLogarithm' :: Case -> Natural
discreteLogarithm' (Case a b _) = discreteLogarithm (fromJust cyclicGroup) a b

benchSuite :: Benchmark
benchSuite = bgroup "Discrete logarithm"
  [ bgroup "individual case"
          [ bench (show c) $ nf discreteLogarithm' c | c <- cases]
  , bgroup "range"
          [ bench (show num ++ " cases near " ++ show n) $ nf (map discreteLogarithm') $ rangeCases n num
            | (n, num) <- [(10000, 100), (1000000, 100), (100000000, 100), (10000000000, 100)]
          ]
  ]
