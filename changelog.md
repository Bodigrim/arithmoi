# Changelog

## 0.11.0.1

### Changed

* Switch to `smallcheck-1.2.0`.

## 0.11.0.0

### Added

* Brand new machinery to deal with Dirichlet characters ([#180](https://github.com/Bodigrim/arithmoi/pull/180)).

* Generate preimages of the Jordan and the sum-of-powers-of-divisors
  functions ([#148](https://github.com/Bodigrim/arithmoi/pull/148)).

* More flexible interface for Pascal triangle: in addition to `binomial`
  we now provide also `binomialRotated`, `binomialLine` and `binomialDiagonal`
  ([#151](https://github.com/Bodigrim/arithmoi/pull/151)). There are also `factoriseFactorial` and `factoriseBinomial` ([#152](https://github.com/Bodigrim/arithmoi/pull/152)).

* Add `Semiring` instance of `SomeMod` ([#174](https://github.com/Bodigrim/arithmoi/pull/174)).

* Generate divisors in range ([#183](https://github.com/Bodigrim/arithmoi/pull/183)).

### Changed

* Speed up `partition`, using better container for memoization ([#176](https://github.com/Bodigrim/arithmoi/pull/176)).

* Speed up `integerRoot`, using better starting approximation ([#177](https://github.com/Bodigrim/arithmoi/pull/177)).

### Deprecated

* Deprecate `Math.NumberTheory.Euclidean`, use `Data.Euclidean` instead.

* Deprecate `chineseRemainder`, `chineseRemainder2`, `chineseCoprime`,
  use `chinese` instead. Deprecate `chineseCoprimeSomeMod`, use `chineseSomeMod`.

* Deprecate `Math.NumberTheory.Powers` except `Math.NumberTheory.Powers.Modular`.
  Use `Math.NumberTheory.Roots` instead.

* Deprecate `Math.NumberTheory.Moduli.Jacobi`, use `Math.NumberTheory.Moduli.Sqrt`
  instead.

* Deprecate `Math.NumberTheory.Moduli.{DiscreteLogarithm,PrimitiveRoot}`,
  use `Math.NumberTheory.Moduli.Multiplicative` instead.

### Fixed

* Fix subtraction of `SomeMod` ([#174](https://github.com/Bodigrim/arithmoi/pull/174)).

## 0.10.0.0

### Added

* The machinery of cyclic groups, primitive roots and discrete logarithms
  has been completely overhauled and rewritten using singleton types ([#169](https://github.com/Bodigrim/arithmoi/pull/169)).

  There is also a new singleton type, linking a type-level modulo with
  a term-level factorisation. It allows both to have a nicely-typed API
  of `Mod m` and avoid repeating factorisations ([#169](https://github.com/Bodigrim/arithmoi/pull/169)).

  Refer to a brand new module `Math.NumberTheory.Moduli.Singleton` for details.

* Add a new function `factorBack`.

* Add `Ord SomeMod` instance ([#165](https://github.com/Bodigrim/arithmoi/pull/165)).

* Add `Semiring` and `Ring` instances for Eisenstein and Gaussian integers.

### Changed

* Embrace the new `Semiring -> GcdDomain -> Euclidean` hierarchy
  of classes, refining `Num` and `Integral` constraints.

* Reshuffle exports from `Math.NumberTheory.Zeta`, do not advertise
  its submodules as available to import.

* Add a proxy argument storing vector's flavor to
  `Math.NumberTheory.MoebiusInversion.{generalInversion,totientSum}`.

* `solveQuadratic` and `sqrtsMod` require an additional argument: a singleton
  linking a type-level modulo with a term-level factorisation ([#169](https://github.com/Bodigrim/arithmoi/pull/169)).

* Generalize `sieveBlock` to handle any flavor of `Vector` ([#164](https://github.com/Bodigrim/arithmoi/pull/164)).

### Deprecated

* Deprecate `Math.NumberTheory.Primes.Factorisation`, use
  `Math.NumberTheory.Primes.factorise` instead. Deprecate
  `Math.NumberTheory.Primes.Sieve`, use `Enum` instance instead.

* Deprecate `Math.NumberTheory.Primes.Factorisation.Certified` and
  `Math.NumberTheory.Primes.Testing.Certificates`.

* Deprecate `Math.NumberTheory.MoebiusInversion.Int`.

* Deprecate `Math.NumberTheory.SmoothNumbers.{fromSet,fromSmoothUpperBound}`.
  Use `Math.NumberTheory.SmoothNumbers.fromList` instead.

* Deprecate `Math.NumberTheory.SmoothNumbers.smoothOverInRange` in favor
  of `smoothOver` and `Math.NumberTheory.SmoothNumbers.smoothOverInRange`
  in favor of `isSmooth`.

### Removed

* Move `Euclidean` type class to `semirings` package ([#168](https://github.com/Bodigrim/arithmoi/pull/168)).

* Remove deprecated earlier `Math.NumberTheory.Recurrencies.*`
  and `Math.NumberTheory.UniqueFactorisation` modules.
  Use `Math.NumberTheory.Recurrences.*` and `Math.NumberTheory.Primes`
  instead.

* Remove deprecated earlier an old interface of `Math.NumberTheory.Moduli.Sqrt`.

## 0.9.0.0

### Added

* Introduce `Prime` newtype. This newtype
  is now used extensively in public API:

  ```haskell
  primes :: Integral a => [Prime a]
  primeList :: Integral a => PrimeSieve -> [Prime a]
  sieveFrom :: Integer -> [Prime Integer]
  nthPrime :: Integer -> Prime Integer
  ```

* New functions `nextPrime` and `precPrime`. Implement an instance of `Enum` for primes ([#153](https://github.com/Bodigrim/arithmoi/pull/153)):

  ```haskell
  > [nextPrime 101 .. precPrime 130]
  [Prime 101,Prime 103,Prime 107,Prime 109,Prime 113,Prime 127]
  ```
* Add the Hurwitz zeta function on non-negative integer arguments ([#126](https://github.com/Bodigrim/arithmoi/pull/126)).

* Implement efficient tests of n-freeness: pointwise and in interval. See `isNFree` and `nFreesBlock` ([#145](https://github.com/Bodigrim/arithmoi/pull/145)).

* Generate preimages of the totient and the sum-of-divisors functions ([#142](https://github.com/Bodigrim/arithmoi/pull/142)):

  ```haskell
  > inverseTotient 120 :: [Integer]
  [155,310,183,366,225,450,175,350,231,462,143,286,244,372,396,308,248]
  ```

* Generate coefficients of Faulhaber polynomials `faulhaberPoly` ([#70](https://github.com/Bodigrim/arithmoi/pull/70)).

### Changed

* Support Gaussian and Eisenstein integers in smooth numbers ([#138](https://github.com/Bodigrim/arithmoi/pull/138)).

* Change types of `primes`, `primeList`, `sieveFrom`, `nthPrime`, etc.,
  to use `Prime` newtype.

* `Math.NumberTheory.Primes.{Factorisation,Testing,Counting,Sieve}`
  are no longer re-exported from `Math.NumberTheory.Primes`.
  Merge `Math.NumberTheory.UniqueFactorisation` into
  `Math.NumberTheory.Primes` ([#135](https://github.com/Bodigrim/arithmoi/pull/135), [#153](https://github.com/Bodigrim/arithmoi/pull/153)).

* From now on `Math.NumberTheory.Primes.Factorisation.factorise`
  and similar functions return `[(Integer, Word)]` instead of `[(Integer, Int)]`.

* `sbcFunctionOnPrimePower` now accepts `Prime Word` instead of `Word`.

* Better precision for exact values of Riemann zeta and Dirichlet beta
  functions ([#123](https://github.com/Bodigrim/arithmoi/pull/123)).

* Speed up certain cases of modular multiplication ([#160](https://github.com/Bodigrim/arithmoi/pull/160)).

* Extend Chinese theorem to non-coprime moduli ([#71](https://github.com/Bodigrim/arithmoi/pull/71)).

### Deprecated

* Deprecate `Math.NumberTheory.Recurrencies.*`.
  Use `Math.NumberTheory.Recurrences.*` instead ([#146](https://github.com/Bodigrim/arithmoi/pull/146)).

### Removed

* Remove `Prime` type family.

* Remove deprecated `Math.NumberTheory.GCD` and `Math.NumberTheory.GCD.LowLevel`.

## 0.8.0.0

### Added

* A new interface for `Math.NumberTheory.Moduli.Sqrt`, more robust and type safe ([#87](https://github.com/Bodigrim/arithmoi/pull/87), [#108](https://github.com/Bodigrim/arithmoi/pull/108)).

* Implement Ramanujan tau function ([#112](https://github.com/Bodigrim/arithmoi/pull/112)):

  ```haskell
  > map ramanujan [1..10]
  [1,-24,252,-1472,4830,-6048,-16744,84480,-113643,-115920]
  ```

* Implement partition function ([#115](https://github.com/Bodigrim/arithmoi/pull/115)):

  ```haskell
  > take 10 partition
  [1,1,2,3,5,7,11,15,22,30]
  ```

* Add the Dirichlet beta function on non-negative integer arguments ([#120](https://github.com/Bodigrim/arithmoi/pull/120)).
  E. g.,

  ```haskell
  > take 5 $ Math.NumberTheory.Zeta.Dirichlet.betas 1e-15
  [0.5,0.7853981633974483,0.9159655941772191,0.9689461462593693,0.9889445517411055]
  ```

* Solve linear and quadratic congruences ([#129](https://github.com/Bodigrim/arithmoi/pull/129)).

* Support Eisenstein integers ([#121](https://github.com/Bodigrim/arithmoi/pull/121)).

* Implement discrete logarithm ([#88](https://github.com/Bodigrim/arithmoi/pull/88)).

### Changed

* Stop reporting units (1, -1, i, -i) as a part of factorisation
  for integers and Gaussian integers ([#101](https://github.com/Bodigrim/arithmoi/pull/101)). Now `factorise (-2)`
  is `[(2, 1)]` and not `[(-1, 1), (2, 1)]`.

* Move `splitIntoCoprimes` to `Math.NumberTheory.Euclidean.Coprimes`.

* Change types of `splitIntoCoprimes`, `fromFactors` and `prefFactors`
  using newtype `Coprimes` ([#89](https://github.com/Bodigrim/arithmoi/pull/89)).

* Sort Gaussian primes by norm ([#124](https://github.com/Bodigrim/arithmoi/pull/124)).

* Make return type of `primes` and `primeList` polymorphic instead of
  being limited to `Integer` only ([#109](https://github.com/Bodigrim/arithmoi/pull/109)).

* Speed up factorisation of Gaussian integers ([#116](https://github.com/Bodigrim/arithmoi/pull/116)).

* Speed up computation of primitive roots for prime powers ([#127](https://github.com/Bodigrim/arithmoi/pull/127)).

### Deprecated

* Deprecate an old interface of `Math.NumberTheory.Moduli.Sqrt`.

* Deprecate `Math.NumberTheory.GCD` and `Math.NumberTheory.GCD.LowLevel` ([#80](https://github.com/Bodigrim/arithmoi/pull/80)).
  Use `Math.NumberTheory.Euclidean` instead ([#128](https://github.com/Bodigrim/arithmoi/pull/128)).

* Deprecate `jacobi'` ([#103](https://github.com/Bodigrim/arithmoi/pull/103)).


* Deprecate `Math.NumberTheory.GaussianIntegers` in favor of
  `Math.NumberTheory.Quadratic.GaussianIntegers`.

## 0.7.0.0

### Added

* A general framework for bulk evaluation of arithmetic functions ([#77](https://github.com/Bodigrim/arithmoi/pull/77)):

  ```haskell
  > runFunctionOverBlock carmichaelA 1 10
  [1,1,2,2,4,2,6,2,6,4]
  ```

* Implement a sublinear algorithm for Mertens function ([#90](https://github.com/Bodigrim/arithmoi/pull/90)):

  ```haskell
  > map (mertens . (10 ^)) [0..9]
  [1,-1,1,2,-23,-48,212,1037,1928,-222]
  ```

* Add basic support for cyclic groups and primitive roots ([#86](https://github.com/Bodigrim/arithmoi/pull/86)).

* Implement an efficient modular exponentiation ([#86](https://github.com/Bodigrim/arithmoi/pull/86)).

* Write routines for lazy generation of smooth numbers ([#91](https://github.com/Bodigrim/arithmoi/pull/91)).

  ```haskell
  > smoothOverInRange (fromJust (fromList [3,5,7])) 1000 2000
  [1029,1125,1215,1225,1323,1575,1701,1715,1875]
  ```

### Changed

* Now `moebius` returns not a number, but a value of `Moebius` type ([#90](https://github.com/Bodigrim/arithmoi/pull/90)).

* Now factorisation of large integers and Gaussian integers produces
  factors as lazy as possible ([#72](https://github.com/Bodigrim/arithmoi/pull/72), [#76](https://github.com/Bodigrim/arithmoi/pull/76)).

### Deprecated

* Deprecate `Math.NumberTheory.Primes.Heap`.
  Use `Math.NumberTheory.Primes.Sieve` instead.

* Deprecate `FactorSieve`, `TotientSieve`, `CarmichaelSieve` and
  accompanying functions. Use new general approach for bulk evaluation
  of arithmetic functions instead ([#77](https://github.com/Bodigrim/arithmoi/pull/77)).

### Removed

* Remove `Math.NumberTheory.Powers.Integer`, deprecated in 0.5.0.0.

## 0.6.0.1

### Changed

* Switch to `smallcheck-1.1.3`.

## 0.6.0.0

### Added

* Brand new `Math.NumberTheory.Moduli.Class` ([#56](https://github.com/Bodigrim/arithmoi/pull/56)), providing
  flexible and type safe modular arithmetic. Due to use of GMP built-ins
  it is also significantly faster.

* New function `divisorsList`, which is lazier than `divisors` and
  does not require `Ord` constraint ([#64](https://github.com/Bodigrim/arithmoi/pull/64)). Thus, it can be used
  for `GaussianInteger`.

### Changed

* `Math.NumberTheory.Moduli` was split into
  `Math.NumberTheory.Moduli.{Chinese,Class,Jacobi,Sqrt}`.

* Functions `jacobi` and `jacobi'` return `JacobiSymbol`
  instead of `Int`.

* Speed up factorisation over elliptic curve up to 15x ([#65](https://github.com/Bodigrim/arithmoi/pull/65)).

* Polymorphic `fibonacci` and `lucas` functions, which previously
  were restricted to `Integer` only ([#63](https://github.com/Bodigrim/arithmoi/pull/63)). This is especially useful
  for modular computations, e. g., `map fibonacci [1..10] :: [Mod 7]`.

* Make `totientSum` more robust and idiomatic ([#58](https://github.com/Bodigrim/arithmoi/pull/58)).

### Removed

* Functions `invertMod`, `powerMod` and `powerModInteger` were removed,
  as well as their unchecked counterparts. Use new interface to
  modular computations, provided by `Math.NumberTheory.Moduli.Class`.

## 0.5.0.1

### Changed

  Switch to `QuickCheck-2.10`.

## 0.5.0.0

### Added

* Add basic combinatorial sequences: binomial coefficients, Stirling
  numbers of both kinds, Eulerian numbers of both kinds, Bernoulli
  numbers ([#39](https://github.com/Bodigrim/arithmoi/pull/39)). E. g.,

  ```haskell
  > take 10 $ Math.NumberTheory.Recurrencies.Bilinear.bernoulli
  [1 % 1,(-1) % 2,1 % 6,0 % 1,(-1) % 30,0 % 1,1 % 42,0 % 1,(-1) % 30,0 % 1]
  ```

* Add the Riemann zeta function on non-negative integer arguments ([#44](https://github.com/Bodigrim/arithmoi/pull/44)).
  E. g.,

  ```haskell
  > take 5 $ Math.NumberTheory.Zeta.zetas 1e-15
  [-0.5,Infinity,1.6449340668482262,1.2020569031595945,1.0823232337111381]
  ```

### Changed

* Rename `Math.NumberTheory.Lucas` to `Math.NumberTheory.Recurrencies.Linear`.

* Speed up `isPrime` twice; rework `millerRabinV` and `isStrongFermatPP` ([#22](https://github.com/Bodigrim/arithmoi/pull/22), [#25](https://github.com/Bodigrim/arithmoi/pull/25)).

### Deprecated

* Deprecate `integerPower` and `integerWordPower` from
  `Math.NumberTheory.Powers.Integer`. Use `(^)` instead ([#51](https://github.com/Bodigrim/arithmoi/pull/51)).

### Removed

* Remove deprecated interface to arithmetic functions (`divisors`, `tau`,
  `sigma`, `totient`, `jordan`, `moebius`, `liouville`, `smallOmega`, `bigOmega`,
  `carmichael`, `expMangoldt`). New interface is exposed via
  `Math.NumberTheory.ArithmeticFunctions` ([#30](https://github.com/Bodigrim/arithmoi/pull/30)).

* `Math.NumberTheory.Logarithms` has been moved to the separate package
  `integer-logarithms` ([#51](https://github.com/Bodigrim/arithmoi/pull/51)).

## 0.4.3.0

### Added

* Add `Math.NumberTheory.ArithmeticFunctions` with brand-new machinery
  for arithmetic functions: `divisors`, `tau`, `sigma`, `totient`, `jordan`,
  `moebius`, `liouville`, `smallOmega`, `bigOmega`, `carmichael`, `expMangoldt` ([#30](https://github.com/Bodigrim/arithmoi/pull/30)).
  Old implementations (exposed via `Math.NumberTheory.Primes.Factorisation`
  and `Math.NumberTheory.Powers.Integer`) are deprecated and will be removed
  in the next major release.

* Add Karatsuba sqrt algorithm, improving performance on large integers ([#6](https://github.com/Bodigrim/arithmoi/pull/6)).

### Fixed

* Fix incorrect indexing of `FactorSieve` ([#35](https://github.com/Bodigrim/arithmoi/pull/35)).

## 0.4.2.0

### Added

* Add new cabal flag `check-bounds`, which replaces all unsafe array functions with safe ones.

* Add basic functions on Gaussian integers.

* Add Möbius mu-function.

### Changed

* Forbid non-positive moduli in `Math.NumberTheory.Moduli`.

### Fixed

* Fix out-of-bounds errors in `Math.NumberTheory.Primes.Heap`, `Math.NumberTheory.Primes.Sieve` and `Math.NumberTheory.MoebiusInversion`.

* Fix 32-bit build.

* Fix `binaryGCD` on negative numbers.

* Fix `highestPower` (various issues).

## 0.4.1.0

### Added

* Add `integerLog10` variants at Bas van Dijk's request and expose
  `Math.NumberTheory.Powers.Integer`, with an added `integerWordPower`.

## 0.4.0.4

### Fixed

* Update for GHC 7.8, the type of some primops changed, they return `Int#` now
  instead of `Bool`.

* Fixed bugs in modular square roots and factorisation.

## 0.4.0.3

### Changed

* Relaxed dependencies on mtl and containers.

### Fixed

* Fixed warnings from GHC 7.5, `Word(..)` moved to `GHC.Types`.

* Removed `SPECIALISE` pragma from inline function (warning from GHC 7.5, probably
  pointless anyway).

## 0.4.0.2

### Changed

* Sped up factor sieves. They need more space now, but the speedup is worth it, IMO.

* Raised spec-constr limit in `MoebiusInversion.Int`.

## 0.4.0.1

### Fixed

* Fixed Haddock bug.

## 0.4.0.0

### Added

* Added generalised Möbius inversion, to be continued.

## 0.3.0.0

### Added

* Added modular square roots and Chinese remainder theorem.

## 0.2.0.6

### Changed

* Performance tweaks for `powerModInteger` (~10%) and `invertMod` (~25%).

## 0.2.0.5

### Fixed

* Fix bug in `psieveFrom`.

## 0.2.0.4

### Fixed

* Fix bug in `nthPrime`.

## 0.2.0.3

### Fixed

* Fix bug in `powerMod`.

## 0.2.0.2

### Changed

* Relax bounds on `array` dependency for GHC 7.4.

## 0.2.0.1

### Fixed

* Fix copy-pasto (only relevant for GHC 7.3).

* Fix imports for GHC 7.3.

## 0.2.0.0

### Added

* Added certificates and certified testing/factorisation

## 0.1.0.2

### Fixed

* Fixed doc bugs

## 0.1.0.1

### Changed

* Elaborate on overflow, work more on native `Ints` in Eratosthenes.

## 0.1.0.0

### Added

* First release.
