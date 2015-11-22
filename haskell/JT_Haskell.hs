module JT_Haskell where

import Data.Complex


---------- POLYNOMIALS ----------

-- Data type representing a polynomial -- finite lists only
-- Representation [a0, a1, ... , an] = a0 + a1 x + ... an x ^ n
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
newton :: Complex Double -> Poly -> Complex Double
newton x0 p
    | x == x0 = x
    | otherwise = newton x p
        where x = newtonAdv x0 p


---------- JENKINS-TRAUB ----------

m = 5 -- number of stage 1 iterations
s2M = 5 -- number of conversion attempts before stage 2 attempts new s

-- s-Sequence
sSeq :: [Complex Double]
sSeq = map (:+ 0) [0..]

-- inititial h value
h0 :: Poly -> Poly
h0 p = pD p

-- s1 s=0 progressor function for h-sequence
sHAdv :: Complex Double -> Poly -> Poly -> Poly
sHAdv s p h = pLinDiv inner s
    where
        c = pEval s p / pEval s h
        inner = p - pSMult c h

-- computes the new polynomial after m s1 shifts
s1H :: Poly -> Poly
s1H p = foldr (sHAdv 0) h00 $ replicate m pp
    where
        h00 = h0 p
        pp = pSMult (last p) p

-- returns the Cauchy Polynomial of p, coefficients will be real
cauchyP :: Poly -> Poly
cauchyP (x:xs) = (- abs x) : map abs xs

-- takes hM and returns hL after performing s2 shifts
-- if P(s) == 0, function will eventually advance to next s
s2H :: Poly -> Poly -> (Poly, [Complex Double])
s2H p hM = s2HRec hM s2M []
    where
        s2HRec h n xs@(x:y:z:zs) -- prevents redundant computation of s
            | n == 0 = s2HRec h s2M []
            | (realPart (abs (x - y)) <= 0.1 * realPart (abs y)) &&
              (realPart (abs (y - z)) <= 0.1 * realPart (abs z)) = (h, [x,y])
            | otherwise = s2HRec (sHAdv (s!!m) p h) (n - 1) $ tFunc p h : xs
        s2HRec h n xs = s2HRec (sHAdv (s!!m) p h) (n - 1) $ tFunc p h : xs
        h00 = h0 p
        s = map (\x -> newton 0 (cauchyP p) * (cos (49 + x * 94) :+ sin (49 + x * 94))) [0..]

-- tests for function explosion
cNaN :: Complex Double -> Bool
cNaN (a :+ b) = isNaN a || isNaN b
naNList :: [Complex Double] -> Bool
naNList (x:xs)
    | cNaN x = True
    | otherwise = naNList xs
naNList [] = False

-- takes hL and returns the Jenkins-Traub root
s3H :: Poly -> (Poly, [Complex Double]) -> Int -> (Complex Double ,Int)
s3H p (hL, sL) n = s3HRec hL sL n
    where
        s3HRec h xss@(x:y:xs) n
            | naNList h = (x, 0)
            | n == 0 = (x, 1)
            | x == y = (x, 2)
            | otherwise = s3HRec (sHAdv x p h) ((tFunc p h) : xss) $ n - 1

-- gets the value of t given a certain polynomial p and h-poly h.
tFunc :: Poly -> Poly -> Complex Double
tFunc p h = - pEval 0 p / (pEval 0 $ h)

jT :: Poly -> (Complex Double ,Int)
jT p = s3H pClean hL 100
    where
        (pClean, nRoots) = pRemZeroRoot $ pSMult (1 / last p) p
        hL = s2H pClean $ s1H pClean

jTTest :: Poly -> IO()
jTTest p = do
    let (pClean, nRoots) = pRemZeroRoot $ pSMult (1 / last p) p
    let st1 = s1H pClean
    let (st2, xs) = s2H pClean st1
    let st3 = s3H pClean (st2, xs) 1000
    putStrLn "=========================================================================================================================="
    putStrLn $ "Testing Polynomial: " ++ show p
    putStrLn "=========================================================================================================================="
    putStrLn $ "\tZero Roots: " ++ show nRoots
    putStrLn $ "\tClean Polynomial: " ++ show pClean
    putStrLn $ "\tStage 1 Polynomial: " ++ show st1
    putStrLn $ "\tStage 2 Polynomial: " ++ show st2
    putStrLn $ "\tStage 3 Polynomial: " ++ show st3


---------------------------------------- TODO LIST
-- Why NaN explosion close to root?? Does halting solve??
-- Deflation

pTF :: [Complex Double]
pTF = [(-1.65), 0.4, (-2.1), 1.2]

pTS :: [Complex Double]
pTS = [-2, 3, 1]