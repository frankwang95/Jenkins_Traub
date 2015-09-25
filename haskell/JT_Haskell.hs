module Main where

import Data.Complex


---------- POLYNOMIALS ----------

-- Data type representing a polynomial -- finite lists only
-- Representation [a1, a2, ... , an] = a1 + a2 x + ... an x ^ n
type Poly = [Complex Double]

-- Data type representing a list of complex roots
type Roots = [Complex Double]

-- Add two polynomials
pAdd :: Poly -> Poly -> Poly
pAdd (a:as) (b:bs) = (a + b) : pAdd as bs
pAdd [] bs = bs
pAdd as [] = as

pSub :: Poly -> Poly -> Poly
pSub p q = pAdd p $ map (* ((-1) :+ 0)) q

-- Polynomial scalar multiplication with a complex constant
pSMult :: Complex Double -> Poly -> Poly
pSMult s = map (*s)

-- Evaluate a polynomial at a point
pEval :: Complex Double -> Poly -> Complex Double
pEval x (a:as) = a + (x * pEval x as)
pEval _ [] = 0

-- Polynomial derivative
pD :: Poly -> Poly
pD (x:xs) = zipWith ((*).(:+ 0)) [1..] xs

-- Polynomial division of linear term
-- Computational overlap with pEval
pLinDiv :: Poly -> Complex Double -> Complex Double -> Poly
pLinDiv p _ _ = p

-- Remove roots at origin
-- Improved with vector implementation?
pRemZeroRoot :: Poly -> (Poly, Roots)
pRemZeroRoot as = (take shifts as, replicate shifts 0)
	where
		shifts = countLeadZeros $ reverse as
		countLeadZeros (0 : as) = 1 + countLeadZeros as
		countLeadZeros (_:as) = 0 


---------- JENKINS-TRAUB ----------

-- S-Sequence
sS :: [Complex Double]
sS = map (:+ 0) [0..]

-- Stage one fixed-shift progressor
s1HAdv :: Poly -> Poly -> Poly
s1HAdv p h = pLinDiv inner 1 0
	where
		c = - pEval 0 h / pEval 0 p
		inner = pSub h $ pSMult c p 