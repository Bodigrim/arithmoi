-- |
-- Module:      Math.NumberTheory.ArithmeticFunctionsTests
-- Copyright:   (c) 2016 Andrew Lelechenko
-- Licence:     MIT
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Tests for Math.NumberTheory.ArithmeticFunctions
--

{-# LANGUAGE CPP       #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.NumberTheory.ArithmeticFunctionsTests
  ( testSuite
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.List (sort)
import qualified Data.Set as S
import qualified Data.IntSet as IS

import Math.NumberTheory.ArithmeticFunctions
import Math.NumberTheory.Primes (UniqueFactorisation (factorise))
import Math.NumberTheory.TestUtils
import Math.NumberTheory.Zeta (zetas)

import Numeric.Natural

oeisAssertion :: (Eq a, Show a) => String -> ArithmeticFunction Natural a -> [a] -> Assertion
oeisAssertion name f baseline = assertEqual name baseline (map (runFunction f) [1 .. fromIntegral (length baseline)])

wolframAlphaAssertion :: (Eq a, Show a) => String -> ArithmeticFunction Integer a -> [Integer] -> [a] -> Assertion
wolframAlphaAssertion name f domain baseline = assertEqual name baseline (map (runFunction f) domain)

-- | tau(n) equals to a number of divisors.
divisorsProperty1 :: NonZero Natural -> Bool
divisorsProperty1 (NonZero n) = S.size (runFunction divisorsA n) == runFunction tauA n

-- | sigma(n) equals to a number of divisors.
divisorsProperty2 :: NonZero Natural -> Bool
divisorsProperty2 (NonZero n) = sum (runFunction divisorsA n) == runFunction (sigmaA 1) n

-- | All divisors of n truly divides n.
divisorsProperty3 :: NonZero Natural -> Bool
divisorsProperty3 (NonZero n) = all (\d -> n `rem` d == 0) (runFunction divisorsA n)

-- | 'divisorsA' matches 'divisorsSmallA'
divisorsProperty4 :: NonZero Int -> Bool
divisorsProperty4 (NonZero n) = S.toAscList (runFunction divisorsA n) == IS.toAscList (runFunction divisorsSmallA n)

-- | 'divisorsA' matches 'divisorsListA'
divisorsProperty5 :: NonZero Int -> Bool
divisorsProperty5 (NonZero n) = S.toAscList (runFunction divisorsA n) == sort (runFunction divisorsListA n)

-- | 'divisorsListInRange' matches 'divisorsA' with a filter
divisorsProperty6 :: Positive Int -> NonNegative Int -> NonNegative Int -> Bool
divisorsProperty6 (Positive a) (NonNegative b) (NonNegative c) = runFunction (divisorsInRangeA from to) n == expected
  where from = a
        to = from + b
        n = to + c
        expected = filter (\d -> d >= from && d <= to) $ S.toAscList (runFunction divisorsA n)

-- | tau matches baseline from OEIS.
tauOeis :: Assertion
tauOeis = oeisAssertion "A000005" tauA
  [ 1, 2, 2, 3, 2, 4, 2, 4, 3, 4, 2, 6, 2, 4, 4, 5, 2, 6, 2, 6, 4, 4, 2, 8
  , 3, 4, 4, 6, 2, 8, 2, 6, 4, 4, 4, 9, 2, 4, 4, 8, 2, 8, 2, 6, 6, 4, 2, 10
  , 3, 6, 4, 6, 2, 8, 4, 8, 4, 4, 2, 12, 2, 4, 6, 7, 4, 8, 2, 6, 4, 8, 2
  , 12, 2, 4, 6, 6, 4, 8, 2, 10, 5, 4, 2, 12, 4, 4, 4, 8, 2, 12, 4, 6, 4, 4
  , 4, 12, 2, 6, 6, 9, 2, 8, 2, 8
  ]

-- | sigma_0 coincides with tau by definition
sigmaProperty1 :: NonZero Natural -> Bool
sigmaProperty1 (NonZero n) = runFunction tauA n == (runFunction (sigmaA 0) n :: Natural)

-- | value of totient is bigger than argument
sigmaProperty2 :: NonZero Natural -> Bool
sigmaProperty2 (NonZero n) = n <= 1 || runFunction (sigmaA 1) n > n

-- | sigma_1 matches baseline from OEIS.
sigma1Oeis :: Assertion
sigma1Oeis = oeisAssertion "A000203" (sigmaA 1)
  [ 1, 3, 4, 7, 6, 12, 8, 15, 13, 18, 12, 28, 14, 24, 24, 31, 18, 39, 20
  , 42, 32, 36, 24, 60, 31, 42, 40, 56, 30, 72, 32, 63, 48, 54, 48, 91, 38
  , 60, 56, 90, 42, 96, 44, 84, 78, 72, 48, 124, 57, 93, 72, 98, 54, 120
  , 72, 120, 80, 90, 60, 168, 62, 96, 104, 127, 84, 144, 68, 126, 96, 144 :: Natural
  ]

-- | sigma_2 matches baseline from OEIS.
sigma2Oeis :: Assertion
sigma2Oeis = oeisAssertion "A001157" (sigmaA 2)
  [ 1, 5, 10, 21, 26, 50, 50, 85, 91, 130, 122, 210, 170, 250, 260, 341, 290
  , 455, 362, 546, 500, 610, 530, 850, 651, 850, 820, 1050, 842, 1300, 962
  , 1365, 1220, 1450, 1300, 1911, 1370, 1810, 1700, 2210, 1682, 2500, 1850
  , 2562, 2366, 2650, 2210, 3410, 2451, 3255 :: Natural
  ]

-- | value of totient if even, except totient(1) and totient(2)
totientProperty1 :: NonZero Natural -> Bool
totientProperty1 (NonZero n) = n <= 2 || even (runFunction totientA n)

-- | value of totient is smaller than argument
totientProperty2 :: NonZero Natural -> Bool
totientProperty2 (NonZero n) = n <= 1 || runFunction totientA n < n

-- | totient matches baseline from OEIS.
totientOeis :: Assertion
totientOeis = oeisAssertion "A000010" totientA
  [ 1, 1, 2, 2, 4, 2, 6, 4, 6, 4, 10, 4, 12, 6, 8, 8, 16, 6, 18, 8, 12, 10
  , 22, 8, 20, 12, 18, 12, 28, 8, 30, 16, 20, 16, 24, 12, 36, 18, 24, 16, 40
  , 12, 42, 20, 24, 22, 46, 16, 42, 20, 32, 24, 52, 18, 40, 24, 36, 28, 58
  , 16, 60, 30, 36, 32, 48, 20, 66, 32, 44
  ]

-- | jordan_0 is zero for argument > 1
jordanProperty1 :: NonZero Natural -> Bool
jordanProperty1 (NonZero n) = n <= 1 || runFunction (jordanA 0) n == 0

-- | jordan_1 coincides with totient by definition
jordanProperty2 :: NonZero Natural -> Bool
jordanProperty2 (NonZero n) = runFunction totientA n == runFunction (jordanA 1) n

-- | jordan_2 matches baseline from OEIS.
jordan2Oeis :: Assertion
jordan2Oeis = oeisAssertion "A007434" (jordanA 2)
  [ 1, 3, 8, 12, 24, 24, 48, 48, 72, 72, 120, 96, 168, 144, 192, 192, 288
  , 216, 360, 288, 384, 360, 528, 384, 600, 504, 648, 576, 840, 576, 960
  , 768, 960, 864, 1152, 864, 1368, 1080, 1344, 1152, 1680, 1152, 1848, 1440
  , 1728, 1584, 2208, 1536
  ]

-- | congruences 1,2,3,4 from https://en.wikipedia.org/wiki/Ramanujan_tau_function
ramanujanCongruence1 :: NonZero Natural -> Bool
ramanujanCongruence1 (NonZero n)
  | k == 1 = (ramanujan n' - sigma 11 n') `rem` (2^11) == 0
  | k == 3 = (ramanujan n' - 1217 * sigma 11 n') `rem` (2^13) == 0
  | k == 5 = (ramanujan n' - 1537 * sigma 11 n') `rem` (2^12) == 0
  | k == 7 = (ramanujan n' - 705 * sigma 11 n') `rem` (2^14) == 0
  | otherwise = True
  where k = n `mod` 8
        n' = fromIntegral n :: Integer

-- | congruences 8,9 from https://en.wikipedia.org/wiki/Ramanujan_tau_function
ramanujanCongruence2 :: NonZero Natural -> Bool
ramanujanCongruence2 (NonZero n)
  | (n `mod` 7) `elem` [0,1,2,4] = m `rem` 7 == 0
  | otherwise                    = m `rem` 49 == 0
  where m = ramanujan n' - n' * sigma 9 n'
        n' = fromIntegral n :: Integer

-- | ramanujan matches baseline from wolframAlpha: https://www.wolframalpha.com/input/?i=RamanujanTau%5BRange%5B100%5D%5D
ramanujanRange :: Assertion
ramanujanRange = wolframAlphaAssertion "A000594" ramanujanA [1..100]
  [ 1, -24, 252, -1472, 4830, -6048, -16744, 84480, -113643, -115920
  , 534612, -370944, -577738, 401856, 1217160, 987136, -6905934, 2727432
  , 10661420, -7109760, -4219488, -12830688, 18643272, 21288960, -25499225
  , 13865712, -73279080, 24647168, 128406630, -29211840, -52843168
  , -196706304, 134722224, 165742416, -80873520, 167282496, -182213314
  , -255874080, -145589976, 408038400, 308120442, 101267712, -17125708
  , -786948864, -548895690, -447438528, 2687348496, 248758272, -1696965207
  , 611981400, -1740295368, 850430336, -1596055698, 1758697920, 2582175960
  , -1414533120, 2686677840, -3081759120, -5189203740, -1791659520, 6956478662
  , 1268236032, 1902838392, 2699296768, -2790474540, -3233333376, -15481826884
  , 10165534848, 4698104544, 1940964480, 9791485272, -9600560640, 1463791322
  , 4373119536, -6425804700, -15693610240, -8951543328, 3494159424, 38116845680
  , 4767866880, 1665188361, -7394890608, -29335099668, 6211086336, -33355661220
  , 411016992, 32358470760, 45164021760, -24992917110, 13173496560, 9673645072
  , -27442896384, -13316478336, -64496363904, 51494658600, -49569988608
  , 75013568546, 40727164968, -60754911516, 37534859200
  ]

-- | ramanujan matches baseline from wolframAlpha: https://www.wolframalpha.com/input/?i=RamanujanTau%5B2%5ERange%5B20%5D%5D
ramanujanPowers2 :: Assertion
ramanujanPowers2 = wolframAlphaAssertion "wolframAlpha2^n" ramanujanA [2^n | n <- [1..20]]
  [ -24, -1472, 84480, 987136, -196706304, 2699296768, 338071388160
  , -13641873096704, -364965248630784, 36697722069188608, -133296500464680960
  , -71957818786545926144, 1999978883828768833536, 99370119662955604738048
  , -6480839625992253084794880, -47969854045919004468445184
  , 14424036051134190424902598656, -247934604141178449046286630912
  , -23589995333334539213089642905600, 1073929957281162404760946449842176
  ]

-- | ramanujan matches baseline from wolframAlpha: https://www.wolframalpha.com/input/?i=RamanujanTau%5B3%5ERange%5B20%5D%5D
ramanujanPowers3 :: Assertion
ramanujanPowers3 = wolframAlphaAssertion "wolframAlpha3^n" ramanujanA [3^n | n <- [1..20]]
  [ 252, -113643, -73279080, 1665188361, 13400796651732, 3082017633650397
  , -1597242480784468560, -948475282905952954479, 43930942451226107469612
  , 179090148438649827109433637, 37348482744132405171657919560
  , -22313464873940134819697044764519, -12239164820907737153507340756954108
  , 868493827155123300221022518147812077, 2386991774972433985188062567645398013280
  , 447670851294004737003138291024309833342241
  , -310035377434952569449318870332553243856267428
  , -157432463407787104647123294163886831498857358283
  , 15248856227707192449163419793501327951694151780600
  , 31731400364681474724113131979212395183355010696469801
  ]

-- | moebius does not require full factorisation
moebiusLazy :: Assertion
moebiusLazy = assertEqual "moebius" MoebiusZ (runFunction moebiusA (2^2 * (2^100000-1) :: Natural))

-- | moebius matches baseline from OEIS.
moebiusOeis :: Assertion
moebiusOeis = oeisAssertion "A008683" moebiusA
  [ MoebiusP, MoebiusN, MoebiusN, MoebiusZ, MoebiusN, MoebiusP, MoebiusN, MoebiusZ, MoebiusZ, MoebiusP, MoebiusN, MoebiusZ, MoebiusN, MoebiusP, MoebiusP, MoebiusZ, MoebiusN, MoebiusZ, MoebiusN, MoebiusZ, MoebiusP, MoebiusP, MoebiusN
  , MoebiusZ, MoebiusZ, MoebiusP, MoebiusZ, MoebiusZ, MoebiusN, MoebiusN, MoebiusN, MoebiusZ, MoebiusP, MoebiusP, MoebiusP, MoebiusZ, MoebiusN, MoebiusP, MoebiusP, MoebiusZ, MoebiusN, MoebiusN, MoebiusN, MoebiusZ, MoebiusZ, MoebiusP
  , MoebiusN, MoebiusZ, MoebiusZ, MoebiusZ, MoebiusP, MoebiusZ, MoebiusN, MoebiusZ, MoebiusP, MoebiusZ, MoebiusP, MoebiusP, MoebiusN, MoebiusZ, MoebiusN, MoebiusP, MoebiusZ, MoebiusZ, MoebiusP, MoebiusN, MoebiusN, MoebiusZ, MoebiusP
  , MoebiusN, MoebiusN, MoebiusZ, MoebiusN, MoebiusP, MoebiusZ, MoebiusZ, MoebiusP
  ]

-- | liouville values are [-1, 1]
liouvilleProperty1 :: NonZero Natural -> Bool
liouvilleProperty1 (NonZero n) = runFunction liouvilleA n `elem` [-1, 1]

-- | moebius is zero or equal to liouville
liouvilleProperty2 :: NonZero Natural -> Bool
liouvilleProperty2 (NonZero n) = m == MoebiusZ || l == runMoebius m
  where
    l = runFunction liouvilleA n
    m = runFunction moebiusA   n

-- | liouville matches baseline from OEIS.
liouvilleOeis :: Assertion
liouvilleOeis = oeisAssertion "A008836" liouvilleA
  [ 1, -1, -1, 1, -1, 1, -1, -1, 1, 1, -1, -1, -1, 1, 1, 1, -1, -1, -1, -1, 1, 1
  , -1, 1, 1, 1, -1, -1, -1, -1, -1, -1, 1, 1, 1, 1, -1, 1, 1, 1, -1, -1, -1, -1
  , -1, 1, -1, -1, 1, -1, 1, -1, -1, 1, 1, 1, 1, 1, -1, 1, -1, 1, -1, 1, 1, -1
  , -1, -1, 1, -1, -1, -1, -1, 1, -1, -1, 1, -1, -1, -1, 1, 1, -1, 1, 1, 1, 1, 1
  , -1, 1, 1, -1, 1, 1, 1, 1, -1, -1, -1, 1, -1
  ]

-- | carmichaeil divides totient
carmichaelProperty1 :: NonZero Natural -> Bool
carmichaelProperty1 (NonZero n) = runFunction totientA n `rem` runFunction carmichaelA n == 0

-- | carmichael matches baseline from OEIS.
carmichaelOeis :: Assertion
carmichaelOeis = oeisAssertion "A002322" carmichaelA
  [ 1, 1, 2, 2, 4, 2, 6, 2, 6, 4, 10, 2, 12, 6, 4, 4, 16, 6, 18, 4, 6, 10, 22, 2
  , 20, 12, 18, 6, 28, 4, 30, 8, 10, 16, 12, 6, 36, 18, 12, 4, 40, 6, 42, 10, 12
  , 22, 46, 4, 42, 20, 16, 12, 52, 18, 20, 6, 18, 28, 58, 4, 60, 30, 6, 16, 12
  , 10, 66, 16, 22, 12, 70, 6, 72, 36, 20, 18, 30, 12, 78, 4, 54
  ]

-- | smallOmega is smaller than bigOmega
omegaProperty1 :: NonZero Natural -> Bool
omegaProperty1 (NonZero n) = runFunction smallOmegaA n <= runFunction bigOmegaA n

-- | smallOmega matches baseline from OEIS.
smallOmegaOeis :: Assertion
smallOmegaOeis = oeisAssertion "A001221" smallOmegaA
  [ 0, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 2, 1, 2, 2, 1, 1, 2, 1, 2, 2, 2, 1, 2, 1, 2
  , 1, 2, 1, 3, 1, 1, 2, 2, 2, 2, 1, 2, 2, 2, 1, 3, 1, 2, 2, 2, 1, 2, 1, 2, 2, 2
  , 1, 2, 2, 2, 2, 2, 1, 3, 1, 2, 2, 1, 2, 3, 1, 2, 2, 3, 1, 2, 1, 2, 2, 2, 2, 3
  , 1, 2, 1, 2, 1, 3, 2, 2, 2, 2, 1, 3, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 1, 3, 1, 2
  , 3, 2, 1, 2, 1, 3, 2
  ]

-- | bigOmega matches baseline from OEIS.
bigOmegaOeis :: Assertion
bigOmegaOeis = oeisAssertion "A001222" bigOmegaA
  [ 0, 1, 1, 2, 1, 2, 1, 3, 2, 2, 1, 3, 1, 2, 2, 4, 1, 3, 1, 3, 2, 2, 1, 4, 2, 2
  , 3, 3, 1, 3, 1, 5, 2, 2, 2, 4, 1, 2, 2, 4, 1, 3, 1, 3, 3, 2, 1, 5, 2, 3, 2, 3
  , 1, 4, 2, 4, 2, 2, 1, 4, 1, 2, 3, 6, 2, 3, 1, 3, 2, 3, 1, 5, 1, 2, 3, 3, 2, 3
  , 1, 5, 4, 2, 1, 4, 2, 2, 2, 4, 1, 4, 2, 3, 2, 2, 2, 6, 1, 3, 3, 4, 1, 3, 1, 4
  , 3, 2, 1, 5, 1, 3, 2
  ]

-- | expMangoldt matches baseline from OEIS.
mangoldtOeis :: Assertion
mangoldtOeis = oeisAssertion "A014963" expMangoldtA
  [ 1, 2, 3, 2, 5, 1, 7, 2, 3, 1, 11, 1, 13, 1, 1, 2, 17, 1, 19, 1, 1, 1, 23, 1
  , 5, 1, 3, 1, 29, 1, 31, 2, 1, 1, 1, 1, 37, 1, 1, 1, 41, 1, 43, 1, 1, 1, 47, 1
  , 7, 1, 1, 1, 53, 1, 1, 1, 1, 1, 59, 1, 61, 1, 1, 2, 1, 1, 67, 1, 1, 1, 71, 1
  , 73, 1, 1, 1, 1, 1, 79, 1, 3, 1, 83, 1, 1, 1, 1, 1, 89, 1, 1, 1, 1, 1, 1
  ]

nFreedomProperty1 :: Word -> NonZero Natural -> Bool
nFreedomProperty1 n (NonZero m) =
    isNFree n m == (all ((< n) . snd) . factorise) m

nFreedomProperty2 :: Power Word -> NonNegative Int -> Bool
nFreedomProperty2 (Power n) (NonNegative m) =
    let n' | n == maxBound = n
           | otherwise     = n + 1
    in take m (filter (isNFree n') [1 ..]) == take m (nFrees n' :: [Integer])

nFreedomProperty3 :: Power Word -> Positive Int -> Bool
nFreedomProperty3 (Power n) (Positive m) =
    let n' | n == maxBound = n
           | otherwise     = n + 1
        zet = 1 / zetas 1e-14 !! (fromIntegral n') :: Double
        m' = 100 * m
        nfree = fromIntegral m' /
                fromIntegral (head (drop (m' - 1) $ nFrees n' :: [Integer]))
    in 1 / fromIntegral m >= abs (zet - nfree)

-- |
-- * Using a bounded integer type like @Int@ instead of @Integer@ here means
-- even a relatively low value of @n@, e.g. 20 may cause out-of-bounds memory
-- accesses in @nFreesBlock@.
-- * Using @Integer@ prevents this, so that is the numeric type used here.
nFreesBlockProperty1 :: Power Word -> Positive Integer -> Word -> Bool
nFreesBlockProperty1 (Power n) (Positive lo) w =
    let block = nFreesBlock n lo w
        len   = length block
        blk   = take len . dropWhile (< lo) . nFrees $ n
    in block == blk

nFreedomAssertion1 :: Assertion
nFreedomAssertion1 =
    assertEqual "1 is the sole 0-free number" (nFrees 0) ([1] :: [Int])

nFreedomAssertion2 :: Assertion
nFreedomAssertion2 =
    assertEqual "1 is the sole 1-free number" (nFrees 1) ([1] :: [Int])

testSuite :: TestTree
testSuite = testGroup "ArithmeticFunctions"
  [ testGroup "Divisors"
    [ testSmallAndQuick "length . divisors = tau"  divisorsProperty1
    , testSmallAndQuick "sum . divisors = sigma_1" divisorsProperty2
    , testSmallAndQuick "matches definition"       divisorsProperty3
    , testSmallAndQuick "divisors = divisorsSmall" divisorsProperty4
    , testSmallAndQuick "divisors = divisorsList"  divisorsProperty5
    , testSmallAndQuick "divisors = divisorsListInRange"  divisorsProperty6
    ]
  , testGroup "Tau"
    [ testCase "OEIS" tauOeis
    ]
  , testGroup "Sigma"
    [ testSmallAndQuick "sigma_0 = tau" sigmaProperty1
    , testSmallAndQuick "sigma_1 n > n" sigmaProperty2
    , testCase          "OEIS sigma_1"  sigma1Oeis
    , testCase          "OEIS sigma_2"  sigma2Oeis
    ]
  , testGroup "Totient"
    [ testSmallAndQuick "totient is even"      totientProperty1
    , testSmallAndQuick "totient n < n"        totientProperty2
    , testCase          "OEIS"                 totientOeis
    ]
  , testGroup "Jordan"
    [ testSmallAndQuick "jordan_0 = [== 1]"  jordanProperty1
    , testSmallAndQuick "jordan_1 = totient" jordanProperty2
    , testCase          "OEIS jordan_2"      jordan2Oeis
    ]
  , testGroup "Ramanujan"
    [ testSmallAndQuick "ramanujan mod 8 congruences" ramanujanCongruence1
    , testSmallAndQuick "ramanujan mod 7 congruences" ramanujanCongruence2
    , testCase          "baseline ramanujan range"    ramanujanRange
    , testCase          "baseline ramanujan powers2"  ramanujanPowers2
    , testCase          "baseline ramanujan powers3"  ramanujanPowers3
    ]
  , testGroup "Moebius"
    [ testCase          "OEIS"           moebiusOeis
    , testCase          "Lazy"           moebiusLazy
    ]
  , testGroup "Liouville"
    [ testSmallAndQuick "liouville values"          liouvilleProperty1
    , testSmallAndQuick "liouville matches moebius" liouvilleProperty2
    , testCase          "OEIS"                      liouvilleOeis
    ]
  , testGroup "Carmichael"
    [ testSmallAndQuick "carmichael divides totient" carmichaelProperty1
    , testCase          "OEIS"                       carmichaelOeis
    ]
  , testGroup "Omegas"
    [ testSmallAndQuick "smallOmega <= bigOmega" omegaProperty1
    , testCase          "OEIS smallOmega"        smallOmegaOeis
    , testCase          "OEIS bigOmega"          bigOmegaOeis
    ]
  , testGroup "Mangoldt"
    [ testCase "OEIS" mangoldtOeis
    ]
  , testGroup "N-freedom"
    [ testSmallAndQuick "`isNFree` matches the definition" nFreedomProperty1
    , testSmallAndQuick "numbers produces by `nFrees`s are `n`-free" nFreedomProperty2
    , testSmallAndQuick "distribution of n-free numbers matches expected" nFreedomProperty3
    , testSmallAndQuick "nFreesBlock matches nFrees" nFreesBlockProperty1
    , testCase "`1` is the only 0-free number" nFreedomAssertion1
    , testCase "`1` is the only 1-free number" nFreedomAssertion2
    ]
  ]
