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
            s2HRec h n xs@(x:y:z:zs) -- prevents redundant computation of s
                | n == 0 = s2HRec h s2M []
                | (realPart (abs (x - y)) <= 0.1 * realPart (abs y)) &&
                  (realPart (abs (y - z)) <= 0.1 * realPart (abs z)) = (h, [x,y], 1)
                | otherwise = s2HRec (fst (sHAdv ep (s!!m) p h)) (n - 1) $ tFunc p h : xs
            s2HRec h n xs = s2HRec (fst (sHAdv ep (s!!m) p h)) (n - 1) $ tFunc p h : xs
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
s3H :: Poly -> (Poly, [Complex Double], Int) -> Int -> Double -> (Complex Double, Int)
s3H p (hL, sL, c) n ep
    | c == 0 = ((head hL), 1)
    | otherwise = s3HRec hL sL n
    where
        s3HRec h xss@(x:y:xs) n
           -- | naNList h = (x, 0)
            | n == 0 = (x, 2)
            | realPart (abs (x - y)) < ep = (x, 1)
            | otherwise = s3HRec (fst (sHAdv ep x p h)) ((tFunc p h) : xss) $ n - 1

-- takes a polynomial deg >= 2 to solve, a max number of stage 3 iterations, abd an epsilon error
-- returns polynomial roots and a completion (2) or cutoff (1) code
jT :: Poly -> Int -> Double -> (Complex Double ,Int)
jT p n ep = s3H pClean hL n ep
    where
        (pClean, nRoots) = pRemZeroRoot $ pSMult (1 / last p) p
        hL = s2H pClean (s1H pClean ep) ep

jTTest :: Poly -> IO()
jTTest p = do
    let (pClean, nRoots) = pRemZeroRoot $ pSMult (1 / last p) p
    let (st1, c) = s1H pClean 0.0000001
    let (st2, xs, c) = s2H pClean (st1, c) 0.0000001
    let st3 = s3H pClean (st2, xs, c) 1000 0.0000001
    putStrLn "=========================================================================================================================="
    putStrLn $ "Testing Polynomial: " ++ show p
    putStrLn "=========================================================================================================================="
    putStrLn $ "\tZero Roots: " ++ show nRoots
    putStrLn $ "\tClean Polynomial: " ++ show pClean
    putStrLn $ "\tStage 1 Polynomial: " ++ show st1
    putStrLn $ "\tStage 2 Polynomial: " ++ show st2
    putStrLn $ "\tStage 3 Polynomial: " ++ show st3


---------------------------------------- TODO LIST
-- Deflation

pTF :: [Complex Double]
pTF = [(-1.518), 3.965, (-3.45), 1]

pTS :: [Complex Double]
pTS = [-2, 3, 1]