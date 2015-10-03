module Main where

import JT_Haskell

import System.IO
import Data.Complex
import Control.Applicative as A
import Control.Monad.ST
import Control.Monad.Par
import Control.DeepSeq
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lex.Fractional
import qualified Data.Attoparsec.ByteString.Char8 as P

instance NFData Complexx where
	rnf (Complexx a b) = rnf a `seq` rnf b

apply :: a -> (a -> b) -> b
apply a = \f -> f a

getParen (x:xs) t
	| elem x "()" = ' ' : (getParen xs $ not t)
	| t = x : getParen xs t
	| otherwise = getParen xs t
getParen [] _ = []

extr (Right a) = a

conv (Complexx a b) = a :+ b


---------- PARSING ----------

iT = B.pack "*I"

data Complexx = Complexx Double Double
	deriving (Show)

cAdd:: Complexx -> [Complexx] -> Complexx
cAdd (Complexx a b) [(Complexx c d)] = Complexx (a + c) (b + d)
cAdd c _ = c
cConv :: Complexx -> Complexx
cConv (Complexx a b) = Complexx b a

parseTerm :: P.Parser Complexx
parseTerm = parseDouble <*> parseConv
	where 
		parseDouble = liftA (\x -> apply (Complexx x 0)) $
			liftA2 (\x y -> toDecimal (B.append x y)) (P.take 1) $ P.takeWhile $ \x -> P.isDigit x || x == '.'
		parseConv = P.option id $ liftA (const cConv) $ P.string $ B.pack "*I"
		toDecimal s = case (readSigned readDecimal) s of
                       Just (d, _) -> d

test :: P.Parser String
test = liftA2 (\x y -> (x:y)) P.anyChar $ many $ P.satisfy $ \x -> P.isDigit x || x == '.'

parseComplexx :: P.Parser Complexx
parseComplexx = liftA2 cAdd parseTerm $ many $ many (P.char '+') A.*> parseTerm


-------- MAIN --------
main = do
	rawData <- B.readFile "poly.txt"
	let text = filter (/= ' ') $ B.unpack rawData
	let p = map (conv.extr.P.parseOnly parseComplexx) $ map B.pack $ words $ getParen text False

	jT p