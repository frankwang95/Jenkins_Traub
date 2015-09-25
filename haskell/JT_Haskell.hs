module Main where


---------- COMPLEX NUMBERS ----------

data Complex = C {re :: Double, img:: Double}
	deriving (Show, Eq)

cAdd :: Complex -> Complex -> Complex
cAdd (C a b) (C c d) = C (a + c) (b + d)

cMult :: Complex -> Complex -> Complex
cMult (C a b) (C c d) = C (a*c - b*d) (a*d + b*c)

cSMult :: Double -> Complex -> Complex
cSMult d (C a b) = C (a*d) (b*d)

cSub :: Complex -> Complex -> Complex 
cSub a b = cAdd a $ cSMult (-1) b

cDiv :: Complex -> Complex -> Complex
cDiv (C a b) (C c d) = C ((a*c + b*d)/(c*c + d*d)) ((b*c - a*d)/(c*c + d*d))

fromReal :: Double -> Complex
fromReal = flip C 0


---------- POLYNOMIALS ----------

-- Data type representing a polynomial -- finite lists only
type Poly = [Complex]

-- Data type representing a list of complex roots
type Roots = [Complex]

-- Add two polynomials
pAdd :: Poly -> Poly -> Poly
pAdd (a:as) (b:bs) = cAdd a b : pAdd as bs
pAdd [] bs = bs
pAdd as [] = as

-- Polynomial scalar multiplication with a complex constant
pSMult :: Complex -> Poly -> Poly
pSMult s = map (cMult s)

-- Evaluate a polynomial at a point
pEval :: Complex -> Poly -> Complex
pEval x (a:as) = a `cAdd` (x `cMult` pEval x as)
pEval _ [] = C 0 0

-- Polynomial derivative
pD :: Poly -> Poly
pD (x:xs) = zipWith (cMult.fromReal) [1..] xs

-- Remove roots at origin
-- Improved with vector implementation??
pRemZeroRoot :: Poly -> (Poly, Roots)
pRemZeroRoot as = (take shifts as, replicate shifts (C 0 0))
	where
		shifts = countLeadZeros $ reverse as
		countLeadZeros ((C 0 0):as) = 1 + countLeadZeros as
		countLeadZeros (_:as) = 0


--------- JENKINS-TRAUB ----------

sS :: [Complex]
sS = map fromReal [0..]

h0 :: Poly -> Complex -> Complex
h0 p c = pEval c $ pD p

-- Overflowing issues
hSeq :: Poly -> [Complex -> Complex]
hSeq p = (h0 p) : map adv (hSeq p)
	where
		adv h = \x -> cDiv (cSub (pEval x p)(cMult (cDiv (pEval (fromReal 0) p) (h (fromReal 0))) (h x))) x
	

--------- TESTING ----------

pT = [C 1 0, C 1 0, C 1 0]
pT2 = map fromReal [2, -3, 1]


---------- TODO ----------
-- Use correct sS sequence -- What is necessary??