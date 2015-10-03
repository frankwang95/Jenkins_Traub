module Main where

import Data.Complex
import Data.Maybe


---------- POLYNOMIALS ----------

-- Data type representing a polynomial -- finite lists only
-- Representation [a0, a1, ... , an] = a0 + a1 x + ... an x ^ n
type Poly = [Complex Double]

-- Data type representing a list of complex roots
type Roots = [Complex Double]

instance Num a => Num ([] a) where
	negate = map negate
	(+) [] bs = bs
	(+) as [] = as
	(+) (a:as) (b:bs) = (a + b) : (+) as bs
	(*) _ _ = []
	fromInteger a = [fromInteger a]
	abs = map abs
	signum _ = []

-- Polynomial scalar multiplication with a complex constant
pSMult :: Complex Double -> Poly -> Poly
pSMult s = map (*s)

-- Polynomial derivative
pD :: Poly -> Poly
pD (x:xs) = zipWith ((*).(:+ 0)) [1..] xs

-- Evaluate a polynomial at a point
pEval :: Complex Double -> Poly -> Complex Double
pEval x (a:as) = a + (x * pEval x as)
pEval _ [] = 0

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


---------- NEWTON ----------

xAdv :: Complex Double -> Poly -> Complex Double
xAdv x p
	| otherwise = ((x * pDp) - pEval x p) /pDp
		where pDp = pEval x $ pD p

newton :: Complex Double -> Poly -> Complex Double
newton x0 p
	| x == x0 = x
	| otherwise = newton x p
		where x = xAdv x0 p


---------- JENKINS-TRAUB ----------

m = 5

-- s-Sequence
sSeq :: [Complex Double]
sSeq = map (:+ 0) [0..]

-- inititial h value
h0 :: Poly -> Poly
h0 = pD

-- s1 s=0 progressor function for h-sequence
s1HAdv :: Poly -> Poly -> Poly
s1HAdv p h = pLinDiv inner 0
	where
		c = pEval 0 h / pEval 0 p
		inner = h - pSMult c p

-- computes the new polynomial after m s1 shifts
s1H :: Poly -> Poly
s1H p = foldr s1HAdv (h0 p) $ replicate m pT

-- returns the Cauchy Polynomial of p, coefficients will be real
cauchyP :: Poly -> Poly
cauchyP (x:xs) = (- abs x) : map abs xs

-- takes a hM and returns hL after performing s2 shifts
s2H :: Poly -> Poly
s2H _ = []

-- gets the value of t given a certain polynomial p and h-poly h.
tFunc :: Poly -> Poly -> Complex Double
tFunc p h = - pEval 0 p / (pEval 0 $ pSMult (1 / last h) h)

jT :: Poly -> Roots
jT p = []
	where
		(pClean, zRoots) = pRemZeroRoot $ pSMult (last p) p
		hM = s1H pClean


---------- TESTING ----------

pT :: [Complex Double]
pT = [-6, 11, -6, 1]


---------- MAIN----------

main = do
	let p = pT
	putStrLn "Nothing here yet"