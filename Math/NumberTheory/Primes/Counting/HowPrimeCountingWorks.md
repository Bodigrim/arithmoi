## Algorithm and Implementation Description

### History and Formulas

This prime counting implementation is based on the work by Adrien-Marie Legendre, who in about the year 1800 observed that the count of the primes to a limit, Pi(limit), can be determined by the inclusion-exclusion principle as noted at the following link:

https://en.wikipedia.org/wiki/Prime-counting_function#Algorithms_for_evaluating_%CF%80(x)

In analysis of this series, it can be seen that it is also an expression of what the Sieve of Eratosthenes does when extended to become the Wheel Sieve where no composite number is culled more than once since the products of the unique primes must always be unique.  Note that the terms with the product of odd numbers of primes on the denominator are subtracted and with even primes are added:  thus the inclusion/exclusion principle.  This expression can then be expressed recursively using a "Phi" expression as follows:

Phi(m, n) = Phi(m, n - 1) - Phi(floor(m / pn), n - 1) where
  `m` is the current limit for this "branch" of the Phi function and
  `n` is the one-based index of the nth prime so p1 is 2, p2 is 3, etc
     and with the terminating conditions that
       Phi(0, n) is 0 and Phi(m, 0) is m.

Now the Legendre formula for the prime count, Pi(limit), is as follows:

Pi(limit) = Phi(limit, Pi(sqrt(limit))) + Pi(sqrt(limit)) - 1

where from this it can be seen that one needs only know the primes to the square root of the range to be counted (and thus the number of them).

The above recursive solution is quite inefficient as to complexity due to the deeply nested recursion, but a further terminating condition can stop the splitting of the "tree" a little sooner as follows:

Phi(m, n) when m is less than or equal to pn must be exactly one

This can be seen to be true because Phi(m, n) expresses the number of values up to `m` that cannot be divided evenly by any of the primes up to `pn` (this includes one, which can't be divided evenly by all numbers higher than it), thus excludes the sieving primes in the range p1 to pn. Therefore, the Phi value for any `m` <= `pn` must be just one.

The computational complexity of this recursive expression is as follows:

O(limit/((log limit)^2)), which while lower than by the Sieve of Eratosthenes due to the log squared in the denominator, is practically quite slow due to the high cost of the multiple recursions and (long) divisions.

However, it has been realized quite recently (1985) that one does not have to solve that expression recursively by, instead of starting from the top of the "tree" and "splitting" first by the larger primes and building up the prime products down to the lowest primes, one can start at the bottom of the "tree" and divide by the lowest prime first on up; also, one can do all of the divisions by each of the primes in partial sieving "waves" while retaining the results in an array of intermediate results which replace the recursive method's "stack".  One then finds that the results of all of the prime products already exist in the as yet unculled values in the partial sieved passes, greatly reducing the amount of computation (especially divisions) of having to build up these prime products from individual primes.

"Phi's" as described are closely related to "rough" numbers, which are the values remaining after culling of all multiples of base primes up to some "partial sieving" base prime and including a count for the one value, so the number of rough numbers up to a limit sieved by primes up to pn is exactly Phi(that limit, n). Using an array of rough numbers and a parallel array of the Phi's (or Pi's) for each of those rough numbers is an efficient way to organize the intermediate calculations.

Note that Phi's and Pi's are also closely related and can be converted one to the other because:

Phi(x, Pi(sqrt(x))) = Pi(x) - Pi(sqrt(x)) + 1

Rough numbers: https://en.wikipedia.org/wiki/Rough_number.Applicative

### Method

In general, the method is as follows:

1. All arrays are of size the square root of the counting range limit, as follows:
   
    a. A `roughs` array to process the remaining rough numbers in culling waves, initialized to all odds starting from 1 (ie. 1, 3, ...),

    b. A `phis` array which contains counts of remaining values after culling up to some level of base prime but is used for emulating the stack during processing initialized with the values as culled by two as in ((`limit`/roughs[`i`] + 1) `div` 2) where `i` is the zero-base index, and

    c. A `pisndxs` array that also contains a simple count of values remaining after culling up to a given culling base prime level but is in `Pi` form with the representations of the already culled base primes included in the count, and with no count for 1 just as for a normal pi; this is initialized with the index numbers as in (0, 1, ...).

    d. Variables are initialized to track the number of base primes processed initialized to zero as well as the current effective size of the `roughs`/`pis` arrays initialized to their current total size.


   The `pisndxs` array and the `roughs` array are closely related to each other in that if one takes a given rough value at a given index, divides that rough value by two and uses the quotient as an index to this array will produce an element value that when the number of (odd) base primes already used is subtracted, produces the index in the roughs array of the original roughs element to form a connected loop; also, this array is used to look up "Pi" values that are used to add and subtract the counts in the manner as described for the phi/pi calculation.  The implementation tries to reduce the number of offsets added and subtracted in the "hottest" loops.

2. The main loop takes the following recursive "bottom-up" function and
   flattens it by using the `pis` array to replace the stack used
   by this function:

   For the following function `baseprms` is an array of odd primes plus non-prime one and values from three up to the square root limit from above.

   ```haskell
   phip2 x = (x + 1) / 2

   phi pmult pilmt =
     let looppi pi acc =
        if pi >= pilmt then acc else
        p = unsafeAt baseprms pi
        npmult = pmult * p
        -- special termination condition for when p >= limit / npmult...
        if p * npmult >= limit then acc + pilmit - pi
        else looppi (pi + 1)
                    (acc + phip2(limit / npmult) - phi npmult pi) -- recursion
   ```

   Phi(limit (Pi(sqrt(limit))) is the above function called with:   
   phi 1 (lenth paseprms).

   Using the recursive function is not efficient because it doesn't have the asymptotic improvements to performance due to partial sieving.

   The loop is for each "base prime" in succession up to the square root of the square root of the `limit` in three phases:

     a. the given base prime value and all multiples of it are marked to be culled if they haven't already been eliminated in previous passes by something like the Sieve of Eratosthenes culling pass per base prime.

     b. this phase processes each of the remaining `roughs` values in turn for all remaining after being marked and splits the processing depending on whether the base prime value multiplied by the remaining rough element is:

    - less than or equal to the square root limit, in which case it compresses the `roughs` and `pis` array by subtracting the next `pis` element from the current one and writing it into an index that doesn't included the marked/skipped elements, or
  
    - if greater than it subtracts a value looked up by rough value index in the `pisndxs` array and again writes the result into the compression index as per above.  
  
    - in all cases it moves the `roughs` to synchronizes with the pi's indices to move values to the left to fill the holes left by eliminating the marked rough values.
  
     c. the last phase is to adjust pisndxs values so they address the correct rough/pis values after the above compression of these arrays has taken place.

3. The partial sieving loop only runs the base prime values up to the square root of the square root of the limit; after that point all of the roughs are prime (other than one) and therefore, the `limit` divided by each of the unique products of pairs of these roughs are added to the final answer from the loop with the usual procedure of looking up the values to be added from the `pisndx` array by using as an index: toIndex (limit / p1 / p2) but with the limitation that p1 can not be larger than (limit / p1 / p1) which satisfies the terminating condition that `p1` must be less than or equal to the quotient:

   `limit` `div` `pi` `div` `p1`.

4. There are various offset calculations and compenstation to make sure allowance has be made for the various conversions between Phi and Pi.

### Detailed Method

In more detail, the "bottom up" partial sieving method is as follows; this implementeds the odds-only optimization so the arrays represent only the odd numbers:

1. Keep track of the number of base primes that have been processed yet and the current effective number of rough numbers; INITIALIAZE `y` as the square root of the limit and three arrays each of the size `y`, with the first small counts array keeping track of the current number of possible primes up to the number represented by the index value so initialized with each element containing the index value, the second the roughs array containing the current level zero rough numbers as per the current culling passes (starts with nothing culled, so all odd values could be prime), and finally, the third containing the current Phi's/ (Pi's in this case) for each rough number in the rough numbers array (initially limit divided by the rough value corresponding to the index).

2. For each base prime in order from the lowest to the limit^(1/4) (same as the square root of `y`) do the following:

    a. Do a partial sieve culling pass of the composites boolean array by the Sieve of Eratosthenes marking all multiples of the current base prime, also marking the value representing the current base prime as "non rough".  The marked values will be those that can't be rough number.

    b. Update the Phi's table for each value that is still rough, while
    eliminating and removing them from the array of roughs by moving them down the table (to the left, to lower indices).  The adjustment is done differently depending whether the product of the base prime and the current rough is greater than the cube root of the limit (where limit divided by this product would be less than the square root of the limit and the counts can be just looked up in the counts table) or less than or equal to the square root in which case they can't be looked up in the counts table; however, the value in the counts table can be looked up in the counts table and that can be used to loop up the adjustment value in the phis table.  All adjustments are subtracted from the current value of the phis table and, since all accumulated phi values will be subtracted from the first accumulation in the phi table, has the effect of subtracting for the first partial sieve pass, adding for the seconds, subtracting for the third, and so on, thus implementing the inclusion/exclusion principle.

    c. Adjust the counts table so the counts reflect the rough values culled in the last partial sieve culling pass.

    d. Repeat all above steps of the stop 2x steps for each base prime up to the limit where the rough values have been fully sieved, adjusting the number of base primes and the effective used sizes of the roughs and phis arrays for every pass.

3. All the accumulated phis from the roughs higher than one can be subtracted from the accumulated phi for one to get an amalgamated answer which is still not complete, as per the last steps.  At this point there are no further changes made to the counts, roughs, or phis arrays.

4. The remaining base primes are now all higher than the the square root of `y` and the remaining roughs higher than these base primes are all prime so the following results can all be added (add for even number of primes in the denominator product - inclusion).  Also, these products are all higher than `y` so the square root of the limit, so all counts can now be looked up in the counts table as follows:

   For each base prime from above the `y`^(1/2) to limit^(1/3) value multiplied by each value above the first to just below `y`` such that the product of the first prime squared and second primes is never higher than the limit (the terminal condition that Phi(0, n) is 0), divide the limit by this product and use the quotient to index the counts value to be added to the answer.

5. The final answer can be obtained by doing any necessary conversions from Phi to Pi.

There are some additional offset and compensation adjustments to make the array indexing and final reaults work out right, especially adjusting for whether particular count/phi values included the current base primes or not; these are negligible in computing cost.

### Asymptotic Complexity

It can easily be seen that the memory requirements are proportional to the square root of the limit; however, the computational complexity isn't quite so obvious as follows:

1. All of the 3a steps represent a Sieve of Eratosthenes to the square root of the limit, so `y` times log log `y`); this is close enough to `y` and the operations are fast, so this is not a significant portion of the time used.

2. All of the 3b steps take the number of base primes to the square root of `y` or `y`^(1/2)/(log (`y`^(1/2))) or 2*`y`^(1/2)/(log `y`) times the number of roughs which quickly reduces to about `y`/log`y` so the product is 2*`y`^(3/2)/((log `y`)^2) or 8*`limit`^(3/4)/((log `limit`)^2).  For large limits, this is clearly a stronger term than the first.

3. All of the 3c stops are the product of all the base primes to the square root of `y` as above times almost all of the counts values (of size `y`) so this is 2*`y`^(3/2)/(log `y`) or 4*`limit`^(3/4)/(log `limit`), which makes this a slightly stronger term than that of 2 above due to the loss of a log factor on the dividend; however, the operations of this step are faster than those of step 2 because step 2 has divisions for almost all operations, so these two may be effectively about the same until large\counting ranges when this term will be the strongest.

4. The operations of step 4 would takes a long time at `limit`/((log `y`)^2) but are saved by the terminating condition when p >= `limit`/`p`/`p` so are about `y`^(4/3)/((log `y`)^2) or 4*`limit`^(2/3)/((log `limit`)^2), meaning that it is considerably less strong than the operations of step 2, although also slightly more complex.

5. Thus, the time complexity is likely mostly controlled by step 2 for smaller counting ranges and by step 3 for large counting ranges meaning that the asymptotic time complexity is about that of 3, which lacks one log factor on the denominator due to having to process most of the count array values for each base prime pass.  Thus the asymptotic complexity for large ranges can be expressed as O(limit^(3/4)/(log limit)), although the epirical complexity won't approach that for the ranges where this algorithm is usable due to memory use constraints.

This implementation only deals with odd values (1, 3, 5 ...), thereby reducing memory storage requirements by a factor of two.

### Conclusions

In short, this is an effective implementation of the Legendre algorithm because any of the "Phi" values for the products of unique primes have already been computed simply and are available in the arrays where this isn't available for the fully recursive expression (for instance, the limit/(p1*p2*P3) value will be immediately available where the recursive expression would have to build it
up from all the combinations of limit/(p1*p2), limit/(p1*p3), and limit/(p2*p3), and so on for longer chains of base prime products).

The worst problem with this algorithm and implementation is its relatively high memory use of O(limit^(1/2)) which in this case is eight times the square root of the counting range in bytes. This means that to sieve to 1e16 takes about 800 Megabytes and to the theoretical maximum limit of 2^19 - 1 would take about 32 Gigabytes, even if one were willing to wait the hours required to return a result.  For such larger counting ranges, there are much better algorithms that aren't that much more difficult to implement, such as a variation on this with the Daniel Friedrich Ernst Meissel modification in about 1870 to change `y` to the cube root of the counting range instead of the square root to use memory as the cube root of the counting range and somewhat improve on the execution asymptotic complexity to proportial to the limit to the two thirds power.
