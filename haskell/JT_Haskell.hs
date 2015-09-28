module Main where

import Data.Complex


---------- POLYNOMIALS ----------

-- Data type representing a polynomial -- finite lists only
-- Representation [a0, a1, ... , an] = a0 + a1 x + ... an x ^ n
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

-- Polynomial division by a linear term
-- Computational overlap with pEval
-- Returns floor( P(x) / (x - c) )
pLinDiv :: Poly -> Complex Double -> Poly
pLinDiv ps c = tail $ bSeqTrack (reverse ps) c []

bSeqTrack (p:ps) c [] = bSeqTrack ps c [p]
bSeqTrack (p:ps) c xss@(x:xs) = bSeqTrack ps c $ (p + c * x) : xss
bSeqTrack [] c xs = xs

-- Remove roots at origin
-- Improved with vector implementation?
pRemZeroRoot :: Poly -> (Poly, Roots)
pRemZeroRoot as = (take shifts as, replicate shifts 0)
	where
		shifts = countLeadZeros $ reverse as
		countLeadZeros (0 : as) = 1 + countLeadZeros as
		countLeadZeros (_:as) = 0

-- Check a polynomial for valididty
pCheck :: Poly -> Bool
pCheck p = (last p) == 1


---------- JENKINS-TRAUB ----------

-- S-Sequence
sS :: [Complex Double]
sS = map (:+ 0) [0..]

-- inititial h value
h0 :: Poly
h0 = pD pT

-- Stage one s=0 progressor function for h-sequence
s1HAdv :: Poly -> Poly -> Poly
s1HAdv p h = pLinDiv inner 0
	where
		c = pEval 0 h / pEval 0 p
		inner = pSub h $ pSMult c p

tFunc :: Poly -> Poly -> Complex Double
tFunc p h = - pEval 0 p / (pEval 0 $ pSMult (1 / last h) h)

s1HSeq :: Poly -> Int -> Poly
s1HSeq p n
	| n == 0 = h0
	| otherwise = s1HAdv p $ s1HSeq p $ n - 1

---------- TESTING ----------
pT :: [Complex Double]
pT = [-6, 11, -6, 1]


---------- MAIN----------