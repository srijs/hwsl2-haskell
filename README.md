# Hashing with SL2

An algebraic hash function, inspired by the paper _Hashing with SL2_ by Tillich and Zemor.

The hash function is based on matrix multiplication in the special linear group
of degree 2, over a Galois field of order 2^127, with all computations modulo
the polynomial x^127 + x^63 + 1.

This construction gives some nice properties, which traditional bit-scambling
hash functions don't possess, including it being composable. It holds:

    hash (m1 <> m2) == hash m1 <> hash m2

Following that, the hash function is also parallelisable. If a message `m` can be divided into a list of chunks `cs`, the hash of the message can be calculated in parallel:

    mconcat (parMap rpar hash cs) == hash m

All operations in this package are implemented in a very efficient manner using SSE instructions.