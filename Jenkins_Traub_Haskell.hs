module Main where


---------- COMPLEX NUMBERS ----------

data Complex = C {re :: Double, img:: Double}
	deriving (Show, Eq)

cAdd :: Complex -> Complex -> Complex
cAdd (C a b) (C c d) = C (a + c) (b + d)

cMult :: Complex -> Complex -> Complex
cMult (C a b) (C c d) = C (a*c - b*d) (a*d + b*c)

cSMult :: Complex -> Double -> Complex
cSMult (C a b) d = C (a*d) (b*d)



---------- POLYNOMIALS ----------

newtype Poly = [Complex] -- finite needed
	deriving (Show, Eq)

pAdd = zipWith (+)
