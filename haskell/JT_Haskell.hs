module JT_Haskell where

import Data.Complex


---------- POLYNOMIALS ----------

-- data type representing a polynomial -- finite lists only
-- representation [a0, a1, ... , an] = a0 + a1 x + ... an x ^ n
type Poly = [Complex Double]

instance Num a => Num ([] a) where
    negate = map negate
    (+) [] bs = bs
    (+) as [] = as
    (+) (a:as) (b:bs) = (a + b) : (+) as bs
    (*) _ _ = []
    fromInteger a = [fromInteger a]
    abs = map abs
    signum _ = []

-- polynomial scalar multiplication with a complex constant
pSMult :: Complex Double -> Poly -> Poly
pSMult s = map (*s)

-- polynomial derivative
pD :: Poly -> Poly
pD (x:xs) = zipWith ((*).(:+ 0)) [1..] xs

-- evaluate a polynomial at a point
pEval :: Complex Double -> Poly -> Complex Double
pEval x (a:as) = a + (x * pEval x as)
pEval _ [] = 0

-- polynomial division by a linear term
-- computational overlap with pEval
-- returns floor( P(x) / (x - c) )
pLinDiv :: Poly -> Complex Double -> Poly
pLinDiv ps c = tail $ bSeqTrack (reverse ps) c []

bSeqTrack (p:ps) c [] = bSeqTrack ps c [p]
bSeqTrack (p:ps) c xss@(x:xs) = bSeqTrack ps c $ (p + c * x) : xss
bSeqTrack [] c xs = xs

-- remove roots at origin
-- improved with vector implementation?
pRemZeroRoot :: Poly -> (Poly, Int)
pRemZeroRoot p = (drop shifts p, shifts)
    where
        shifts = countLeadZeros p
        countLeadZeros (0 : p) = 1 + countLeadZeros p
        countLeadZeros _ = 0


---------- NEWTON ----------

-- progressor function for Newton's Method
newtonAdv :: Complex Double -> Poly -> Complex Double
newtonAdv x p
    | otherwise = ((x * pDp) - pEval x p) /pDp
        where pDp = pEval x $ pD p

-- solve with Newton's Method
-- Redundancy not need because Cauchy Polynomials are nice
newton :: Double -> Complex Double -> Poly -> Complex Double
newton ep x0 p
    | realPart (abs (x - x0)) < ep  = x
    | otherwise = newton ep x p
        where x = newtonAdv x0 p


---------- JENKINS-TRAUB ----------

m = 10 -- number of stage 1 iterations
s2M = 1 -- number of conversion attempts before stage 2 attempts new s

-- inititial h value by taking derivative
h0 :: Poly -> Poly
h0 p = pD p

-- gets the value of t given a certain polynomial p and h-poly h.
tFunc :: Poly -> Poly -> Complex Double
tFunc p h = - pEval 0 p / (pEval 0 $ h)

-- general h-polynomial advancer
-- takes an error value, an s-value, the original polynomial, and the last h-polynomial
-- returns a polynomial and a code for root found (0) and continued (1)
sHAdv :: Double -> Complex Double -> Poly -> Poly -> (Poly, Int)
sHAdv ep s p h
    | realPart (abs c) < ep = ([s], 0)
    | otherwise = (pLinDiv inner s, 1)
    where
        c = pEval s p / pEval s h
        inner = p - pSMult c h

-- performs stage-1 iterations
-- takes an polynomial and an error value
-- returns a polynomial and a code for root found (0) and continued (1)
s1H :: Poly -> Double -> (Poly, Int)
s1H p ep = s1HRec m $ h0 p
    where
        s1HRec mm hh
            | code == 0 = (next, 0)
            | mm == 0 = (next, 1)
            | otherwise = s1HRec (mm - 1) $ next
            where (next, code) = sHAdv ep 0 p hh  

-- returns the Cauchy Polynomial of p, coefficients will be real
cauchyP :: Poly -> Poly
cauchyP (x:xs) = (- abs x) : map abs xs

-- performs stage-2 iterations
-- takes the original polynomial, the result of the stage-1 shifts, and an error value
-- returns a polynomial and a code for root found (0) and continued (1)
-- if P(s) == 0, function will eventually advance to next s
s2H :: Poly -> (Poly, Int) -> Double -> (Poly, [Complex Double], Int)
s2H p (hM, c) ep
    | c == 0 = (hM, [], c)
    | otherwise = s2HRec hM s2M []
        where
            h00 = h0 p
            sFunc x = newton ep 0 (cauchyP p) * (cos (49 + x * 94) :+ sin (49 + x * 94))
            s = map sFunc [0..]
            s2HRec h n xs@(x:y:z:zs) -- prevents redundant computation of s
                | c == 0 = (next, [], c)
                | n == 0 = s2HRec h s2M []
                | (realPart (abs (x - y)) <= 0.1 * realPart (abs y)) &&
                  (realPart (abs (y - z)) <= 0.1 * realPart (abs z)) = (h, [x,y], 1)
                | otherwise = s2HRec next (n - 1) $ tFunc p h : xs
                    where (next, c) = sHAdv ep (s!!m) p h
            s2HRec h n xs = s2HRec next (n - 1) $ tFunc p h : xs
                where (next, c) = sHAdv ep (s!!m) p h

-- takes hL and returns the Jenkins-Traub root
s3H :: Poly -> (Poly, [Complex Double], Int) -> Int -> Double -> (Complex Double, Int)
s3H p (hL, sL, c) n ep
    | c == 0 = ((head hL), 2)
    | otherwise = s3HRec hL sL n
    where
        s3HRec h xss@(x:y:xs) n
            | n == 0 = (x, 2)
            | realPart (abs (x - y)) < ep = (x, 1)
            | otherwise = s3HRec (fst (sHAdv ep x p h)) ((tFunc p h) : xss) $ n - 1

-- takes a polynomial deg >= 2 to solve, a max number of stage 3 iterations, and an epsilon error
-- returns polynomial roots and a completion (2) or cutoff (1) code
jT :: Poly -> Int -> Double -> [Complex Double]
jT p n ep
    | code == 2 = []
    | length pDef == 1 = [foundRoot]
    | otherwise = foundRoot : jT pDef n ep
    where
        (foundRoot, code) = (s3H pClean hL n ep)
        pDef = pLinDiv p foundRoot
        (pClean, nRoots) = pRemZeroRoot $ pSMult (1 / last p) p
        hL = s2H pClean (s1H pClean ep) ep

pT :: [Complex Double]
pT = [(-17.3184), 28.988, (-9.73), (-3.2), 1]