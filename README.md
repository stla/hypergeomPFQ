# hypergeomPFQ

## Evaluation of the hypergeometric function of a matrix argument (Koev & Edelman's algorithm)

The module `HypergeomU` uses unboxed arrays, while `Hypergeom` uses boxed arrays.
`Hypergeom` allows to input rational parameters.
`Hypergeom2` is like `Hypergeom` but it prevents `alpha` to be a complex number 
(see <https://stackoverflow.com/q/58056296/1100107>).

### Reference

Koev & Edelman. *The efficient evaluation of the hypergeometric function of a matrix argument*.
Mathematics of computation, vol. 75, n. 254, 833-846, 2006.
