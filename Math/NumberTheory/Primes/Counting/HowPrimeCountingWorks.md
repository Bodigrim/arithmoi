## Algorithm and Implementation Description

The prime counting implementation used here is based on the Xavier Gourdon 2001 algorithm [as per his unpublised paper](http://numbers.computation.free.fr/Constants/Primes/Pix/piNalgorithm.ps), with a few corrections as omissions, as follows:

1. The formula for Sigma[6] on page 6 is incorrect for truncating integer division so should be the summation over (sqrt(x/p)) squared rather than the summation over (sqrt(x)/sqrt(p)) squared even though mathematically to infinite precision they are the same.
2. The formula for Phi0 on page 7 is incomplete and the formula from page 3 should be used, although they become the same if the tuning factor for `z` isn't used as we don't; see below...
3. This implementation does not use the `z` tuning factor (sometimes called "alpha-z") so `y` is exactly the same value as `z` in the formulas; this tuning factor only makes about a five percent improvement in performance and only for fairly small ranges of prime counting while adding quite a bit of program complexity so the factor is just "one" in this implementation.

### History and Formulas

#### Legendre

All combinational prime counting algorithms are based on the work by Adrien-Marie Legendre, who in about the year 1800 observed that the count of the primes to a limit, Pi(limit), can be determined by the inclusion-exclusion principle as noted at the following link:

https://en.wikipedia.org/wiki/Prime-counting_function#Algorithms_for_evaluating_%CF%80(x)

In analysis of this series, it can be seen that it is also an expression of what the Sieve of Eratosthenes does when extended to become the Wheel Sieve where no composite number is culled more than once since the products of the unique primes must always be unique. Note that the terms with the product of odd numbers of primes on the denominator are subtracted and with even numbers of primes are added: thus the inclusion/exclusion principle. This expression can then be expressed recursively using a "Phi" expression as follows:

Phi(m, n) = Phi(m, n - 1) - Phi(floor(m / pn), n - 1) where m is the current limit for this "branch" of the Phi function and n is the one-based index of the nth prime so p1 is 2, p2 is 3, etc and with the terminating conditions that Phi(0, n) is 0 and Phi(m, 0) is m.

Now the Legendre formula for the prime count, Pi(limit), is as follows:

Pi(limit) = Phi(limit, Pi(sqrt(limit))) + Pi(sqrt(limit)) - 1

where from this it can be seen that one needs only know the primes to the square root of the range to be counted (and thus to know the number of primes within the entire range).

The above recursive solution is quite inefficient as to complexity due to the deeply nested recursion, but a further terminating condition can stop the splitting of the "tree" a little sooner as follows:

Phi(m, n) when m is less than or equal to pn must be exactly one

This can be seen to be true because Phi(m, n) expresses the number of values up to m that cannot be divided evenly by any of the primes up to pn (this includes one, which can't be divided evenly by all numbers higher than it), thus excludes the sieving primes in the range p1 to pn. Therefore, the Phi value for any m <= pn must be just one.

The computational complexity of this recursive expression is as follows:

O(limit/((log limit)^2)), which while lower than by the Sieve of Eratosthenes due to the log squared in the denominator, is practically quite slow due to the high cost of the multiple recursions and (long) divisions.

#### Meissel

In about the year 1870, Daniel Friedrich Ernst Meissel extended the work of Legendre in several major ways, as follows:

1. He realized that one doesn't have to compute all of the products of all the base primes (in his case to the cube root of the prime counting range of a billion - nine zeros) if one uses "partial sieving" in the sieving; "partial sieving" means doing a computation based on the state of the sieve buffer after each culling pass of each individual base prime, which state reflects the recursive `a - 1` "Phi" values (or "Pi" values as related to "Phi" values by a simple relation) and can then be used directly by simple count value lookup (he actually kept accumulated counts of groups of sieve values, and only had to count within a given group for the exact values).  By using this technique to directly look up the counts for all values falling within the range of his sieve (where the products of the primes were greater than the square root of the counting range or 32,622 in his case), he could bypass most of the recursion as in the above algorithm, and for those few cases where the product of the primes was less than the limit (32,622 in his case, or 1,000 if he had used the full sieve to the two thirds power of x), these values can be simply determined by a small look up table and a little bit of math.
2. He used wheel factorization by the small primes of two, three, and five in his sieving and in the use of this last mentioned "tiny Phi" look up table.
3. He realized that by only using the "base primes" to the cube root of the counting limit instead of the square root, that the total number of division and counting operations could be greatly reduced, only requiring a relatively simple "P2" calculation to compensate for the calculations not done.  However, the full implementation of his algorithm requires that one sieve to the prime counting range to the two thirds power rather than the square root of the prime counting range, so in his hand calculation of the number to a billion (nine zeros), he desided to only sieve to the square root (32,622) rather than the full million (six zeros) which lost some of the reduction in division and counting operations he might have had, although still less than through the use of the Legendre algorithm, even with the use of "partial sieving".  I think that this decision to use the smaller sieve was a mistake as the extra simple work of sieving to a million seems to me to be much less than the many extra divisions and table lookups that he had to do using the smaller sieve.

#### Lagarias, Miller, and Odlyzko (LMO)

In 1985, Lagarias, Miller, and Odlyzko wrote a paper about a mainframe implementation implementing a new class of prime counting functions, with the following breakthrough's:

1. They realized that they could tune the performance of their prime counting algorithm by adjusting it to between that of Legendre with not much sieving but many division and counting operations (even when combined with "partial sieving") and that of Meissel which required much fewer division and counting operations but much more sieving.
2. Their work broke the classifications of what would otherwise be the recursive tree "leaves" into classes of the S1 "easy" class and the S2 "hard" class with different treatments for different ranges of the S2 class.
3. The proved that such an algorithm could be implemented as multi-threaded to run on a many core CPU, and implemented this on their two core IBM mainframe computer.
4. Their work improved the asymptotic complexity of the algorithm from `O(x**(2/3)*(log log x))` for Meissel to `O(x**(2/3)/(log x))` (note the extra division by a log term where the log log x term is quite close to one).

#### Deleglise and Rivat

Deleglise and Rivat published some refinements to the LMO algorithm in 1996 with some further subclassification of the "leaves" into the "S2" easy special leaves and the "S2" hard special leaves with different optimizations for each and were thus able to improve the asymptotic complexity to O(x**(2/3)/((log x)**2)) (another extra division by a log term) while still using about the same amount of memory. With these refinements the tuning factor can be increased above that used for LMO and the required sieving range is reduced proportionally.

#### Xavier Gourdon

Gourdon's paper of 2001 referenced in the introduction is the ultimate refinement to all of the above work, lacking only implementation improvements to bring in up to the modern standards such as the "primecount" implementation by Kim Walisch which has been used to break the known primes to the limit of 10 to the 28th power and beyond, only limited by available computer cores and memory.  Gourdon's work has about the same computational complexity as the Deleglise work but enjoys extra constant factor improvements due to his taking advantage of some algorithmic shortcuts such as recognizing symetric an cutting the number of certain computations in two and just multiplying the result once by two.

### Synopsis of Implementation

First, the base primes up to `y` are found using sieving as well as the "Pi(y)" look up array over this range and the "factors" array over this same range containing the Least Prime Factors and Moebius function values of each represented number, with these last two arrays containing representations for only the odd numbers.  As well, the Sigma and Phi0 values are calculated.  All work up to this point is single threaded.  The main work is then (possibly using multi-threading if enabled) to do page-segmentation across all required pages in order up to the sieve limit, with the values for A, B, C, and D accumulated using accumulated intermediate "Phi" values for each "partial sieving" base prime used over each page segment.  As the pages up to the square root of the counting limit are only used for the A values but which are very densely packed points whereas the B, C, and D values come from the less packed pages from that point upwards, special load balancing planning is done for the lower range where computation is chunked over groups of base primes sized so that each task is about the same size as for each full page of work for the upper range; this balances the work done per thread to about the same levels.  Finally, the accumulated values and previously calculated values are combined for the final answer as in the `Pi(x) = Sigma + Phi0 + A - B + C + D` Gourdon formula.

### Conclusions

In short, this is an effective implementation of the Gourdon algorithm using modern implementation improvements such as a "factors" table of Least Prime Factors and Moebius values and the delta counts technique of block counting while implementing a full mtuli-threading capabilities to take advantage of all available cores.  Memory use is only proportional to the cube root of the counting range, so isn't a factor on modern computers when calculating the number of primes up to the 64-bit Word size (only a few million bytes used).
