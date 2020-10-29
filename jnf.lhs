> module JNF where

> import Prelude
> import Data.List
> import Data.Ord
> import Data.Matrix 					hiding ((!),flatten) 
> import qualified Data.Vector			as V
> import qualified Data.Vector.Mutable	as MV
> import Data.Tree
> import Data.Graph
> import Data.Array
> import Automata
> import Util
> import GraphFunctions
> import Matrix
> import NFAe
> import ArbitraryE
> import BinaryE
> import Lin
> import Martinez
> import Toggle

> data CNF = CNF {
>	md :: Int
>	, yd :: [Int]
>	, fd :: [Int]
>	} deriving Show

> data NNF = NNF {
>	cnfs :: [(CNF,[(Int,Int)])] -- an array of all the CNFs in the JNF
>	} deriving Show

> convertToJNF :: NFAe -> NNF
> convertToJNF a = NNF (zip cnfChrobaks (map (placeIndices togMap) (zip (map snd togMap) (map chr2OriTog (map fst toggleForms)))))
>			where {	
>					toggles = removeDuplicates $ (initToggles a) ++ (getAllArbToggles a);
>					togMap = mapToStdVerts 0 (map fst toggles);
>					unarys = map (getUnary a (concat (map (decomposeByContig a ) (decomposeByLetter a (eAlphabet a))))) toggles;
>					chrobaks = zip (map fst toggles) ((map (linConvertToChrobak) (map fst unarys)));
>					cnfChrobaks = map cnfConvertToChrobak (map fst unarys);
>					toggleForms = (map (toToggleForm toggles) (zip (map snd chrobaks) unarys))}
 
 >					e = foldl (addCNF togMap) (initNFAe toggles togMap (eAlphabet a)) (map (toToggleForm toggles) (zip (map snd chrobaks) unarys));

> placeIndices :: [Edge] -> (Int,[Edge]) -> [(Int,Int)]
> placeIndices togMap (i, chr2OriTog) = splayIndices (map fst chr2OriTog) (map ((\y z -> [q | (p,q) <- y, p==z]) togMap) (map snd chr2OriTog))

> splayIndices :: [Int] -> [[Int]] -> [(Int,Int)]
> splayIndices [] _ = []
> splayIndices (x:xs) (y:ys) = (zip (repeat x) y) ++ (splayIndices xs ys)


> cnfConvertToChrobak :: UnaryAutomata -> CNF
> cnfConvertToChrobak a = CNF m y (tailS ++ loopS)
> 						where {	n = length (unStates a);
>								m = (n*n)+n;
>								tgraphs = getAllSCCSubGraphs (automataToGraph a);
>								y = map getSimpleLoopGCD (map graphToMatrix tgraphs);
>								k = length y;
>								init = initChrobak m k y (unAlphabet a);
>								tailS = getTailFinalStates [0..m-1] a [unInitial a];
>								loopS = getAllLoopFins a init tgraphs}

 > cnfToNFAe :: CNF -> NFAe 
 > 



> toggles = removeDuplicates $ (initToggles a) ++ (getAllArbToggles a);
> togMap = mapToStdVerts 0 (map fst toggles);
> unarys = map (getUnary a (concat (map (decomposeByContig a ) (decomposeByLetter a (eAlphabet a))))) toggles;
> chrobaks = zip (map fst toggles) ((map (linConvertToChrobak) (map fst unarys)));
> toggleForms = (map (toToggleForm toggles) (zip (map snd chrobaks) unarys));

