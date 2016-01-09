# Jenkins_Traub
Implementations of the Jenkins Traub polynomial complex root finding algorithm in Haskell and C.

# Haskell
The Haskell implementation is complete although care should be used as the algorithm often fails to achieve high precision, especially for higher degree polynomials.
### Dependencies
This library requires only the `Data.Complex` library to run.
### Usage
Complex numbers are represented in the standard notation used in `Data.Complex`. A polynomial is represented by a finite list so that the polynomial `a0 + a1 x + ... an x ^ n` is given by the list `a0, a1, ... , an]`.

The JT_Haskell library implements the jT function which has type `jT :: Poly -> Int -> Double -> [Complex Double]`. An example usage is as follows:
	```
	jT :: p n d
	```
This solves the polynomial `p` using a maximum of `n` iterations with epsilon error`d`.

# C
C implementation to come.