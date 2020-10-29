> module Compression where

> import Prelude
> import Data.List
> import Data.Ord
> import Data.Matrix 					hiding ((!),flatten) 
> import qualified Data.Vector			as V
> import qualified Data.Vector.Mutable	as MV
> import Data.Tree
> import Data.Graph
> import Data.Array
> import Matrix
> import Util
> import Automata
> import GraphFunctions
> import Martinez
> import Lin
> import Toggle
> import BinaryE
> import NFAe 
> import NFA
> import ArbitraryE

> data CompressedChr = CompressedChr {
>	tailLength :: Int
>	, loopLengths :: [Int]
>   , finals :: [Int]
>	} deriving Show

> compressChr :: ChrNF -> CompressedChr 
> compressChr c = CompressedChr (length $ tailStates c) (map length (loopStates c)) (chrFinal c)

 > uncompressChr :: CompressedChr -> ChrNF 
 > uncompressChr c = ChrNF tailSt loopSt 'a' (zip tailSt (map succ tailSt)) 
 > 	where {
 >		tailSt = [0..((tailLength c)-1)];
 >		loopSt = (addLoops ((tailLength c)-1) (loopLengths c));
 >		

> addLoops :: Int -> [Int] -> [[Int]]
> addLoops _ [] =  []
> addLoops f (x:xs) = [[(f+1)..(f+x)]] ++ (addLoops (f+x) xs)
