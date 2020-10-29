> module ArbitraryE where

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
> import Noam
> import Data.Csv
> import qualified Data.ByteString.Lazy as BSL

 > import Arbtests2

> getAllArbToggles :: NFAe -> [(Vertex,Char)]
> getAllArbToggles a = sort $ concat $ map (getArbitraryToggles a) (eAlphabet a)

> getArbitraryToggles :: NFAe -> Char -> [(Vertex,Char)]
> getArbitraryToggles a c = [(x,c) | 	x <- (eStates a),  
>													(	(elem x (map fst charTrans)) && 
>														(elem x (map snd notCharTrans)))]
>		where	{ 	charTrans = [edge1 | (character1,edge1) <- (eTransitions a), character1 == c];
>					notCharTrans = [edge1 | (character1,edge1) <- (eTransitions a), character1 /= c]};

> initToggles :: NFAe -> [(Vertex,Char)]
> initToggles a = if lettersWith == [] then [(0,'z')] else lettersWith 
>		where lettersWith = [(init,chr) | init <- (eStates a),elem init [eInitial a],(chr, ed) <- (eTransitions a),(fst ed) == init]

> cleanAutomata :: NFAe -> NFAe
> cleanAutomata a = NFAe (removeDuplicates $ eStates a) (eAlphabet a) (eTransitions a) (epsilons a) (eInitial a) (removeDuplicates $ eFinal a)

> convertArbAutomata :: NFAe -> NFAe
> convertArbAutomata a = foldl (addToggleForm togMap) (initNFAe toggles togMap (eAlphabet a)) (map (toToggleForm toggles) (zip (map snd chrobaks) unarys))
>			where {	toggles = removeDuplicates $ (initToggles a) ++ (getAllArbToggles a);
>					togMap = mapToStdVerts 0 (map fst toggles);
>					unarys = map (getUnary a (concat (map (decomposeByContig a ) (decomposeByLetter a (eAlphabet a))))) toggles;
>					chrobaks = zip (map fst toggles) ((map (linConvertToChrobak) (map fst unarys)));}

> convertSimpArbAutomata :: NFAe -> NFAe
> convertSimpArbAutomata a = foldl (addToggleForm togMap) 	(initNFAe toggles togMap (eAlphabet a)) (map (toToggleForm toggles) (zip (map snd chrobaks) unarys))
>			where {	toggles = removeDuplicates $ getAllArbToggles a;
>					togMap = mapToStdVerts 0 (map fst toggles);
>					unarys = map (getUnary a (concat (map (decomposeByContig a ) (decomposeByLetter a (eAlphabet a))))) toggles;
>					chrobaks = zip (map fst toggles) (map simplifyChrobak (map (linConvertToChrobak) (map fst unarys)));}

> testArbAutomata :: Int -> NFAe -> NFAe -> Bool
> testArbAutomata i a b = and ((map (acceptNFAe b) words) ++ (map (acceptNFAe a) words2))
>		where 	{
>					words =	snd (findWordse 0 i ([("",[eInitial a])],[]) a);
>					words2 = snd (findWordse 0 i ([("",[eInitial b])],[]) b);
>				}

> arbAutomata = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13] ['a','b','c'] [('b',(0,1)),('b',(1,2)),('b',(2,3)),('b',(3,4)),('b',(4,1)),('a',(4,5)),('a',(5,6)),('a',(6,7)),('a',(7,8)),('a',(8,9)),('a',(9,5)),('a',(5,1)),('a',(3,10)),('a',(10,12)),('a',(12,11)),('a',(11,10)),('a',(12,13)),('c',(2,3)),('c',(3,6)),('c',(6,2))] [] 0 [13]

 > arbAutomata2 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13] ['a','b','c'] [('b',(0,1)),('b',(1,2)),('b',(2,3)),('b',(3,4)),('b',(4,1)),('a',(4,5)),('a',(5,6)),('a',(6,7)),('a',(7,8)),('a',(8,9)),('a',(9,5)),('a',(5,1)),('a',(3,10)),('a',(10,12)),('a',(12,11)),('a',(11,10)),('a',(12,13)),('c',(2,3)),('c',(3,6))] [] 0 [13]


 > testAuto = NFAe [0,1,2,3] "ab" [('a',(0,1)),('a',(0,2)),('a',(0,3)),('b',(2,1)),('b',(3,1)),('b',(2,0))] [] 0 [1]

 > testAuto1 = NFAe [0,1,2] "ab" [('a',(0,0)),('a',(0,1)),('a',(1,2)),('a',(2,2)),('b',(0,1)),('b',(1,0)),('b',(1,2)),('b',(2,2))] [] 0 [2]

 > testAuto2 = NFAe [0,1,2] "ab" [('a',(1,1)),('a',(2,2)),('a',(0,2)),('b',(0,1)),('b',(1,2)),('b',(2,0))] [] 0 [2]

> a = arbAutomata

 > toggles = removeDuplicates $ getAllArbToggles a
 > togMap = mapToStdVerts 0 (map fst toggles)
 > unarys = map (getUnary a (concat (map (decomposeByContig a ) (decomposeByLetter a (eAlphabet a))))) toggles
 > chrobaks = zip (map fst toggles) (map linConvertToChrobak (map fst unarys))
 > initAuto = (initNFAe toggles togMap (eAlphabet a))
 > toggleForms = (map (toToggleForm toggles) (zip (map snd chrobaks) unarys))
 > newAuto = foldl (addToggleForm togMap) initAuto toggleForms

 > tog = fst $ toggleForms!!0
 > orig2Un = snd $ toggleForms!!0
 > building = initAuto
 > allTogStates = ((togTailStates tog) ++ (concat (togLoopStates tog)))
 > allTogTransitions = (togTailTransitions tog) ++ (concat (togLoopTransitions tog))
 > newTogMap = zip (map fst (chr2OriTog tog)) (map (mapVertex togMap) (map snd (chr2OriTog tog)))
 > nonToggleStates = [y | y <- allTogStates, not $ elem y (map fst newTogMap)]
 > totalMap = zip allTogStates [(length (eStates building))..((length (eStates building))+(length allTogStates))]

 > totalMap = nonTogMap

 > builtStates = (eStates building) ++ [x | x <- (map (mapVertex totalMap) allTogStates), not (elem x (eStates building))];
 > extraTransitions = (removeDuplicateTransitions (zip (repeat (togAlphabet tog)) (concat $ map (mapPair totalMap) allTogTransitions)))
 > builtTransitions = (eTransitions building) ++ extraTransitions;
 > builtFinal =  (eFinal building) ++ (map (mapVertex totalMap) (togFinal tog));




> eAutomata = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13] ['a','b','c'] [('b',(0,1)),('b',(1,2)),('b',(2,3)),('b',(3,4)),('b',(4,1)),('a',(4,5)),('a',(5,6)),('a',(6,7)),('a',(7,8)),('a',(8,9)),('a',(9,5)),('a',(5,1)),('a',(3,10)),('a',(10,12)),('a',(12,11)),('a',(11,10)),('a',(12,13)),('c',(2,3)),('c',(3,6)),('c',(6,2))] [(0,11)] 0 [13]

> simpleAuto = NFAe [0,1,2,3,4] ['a','b','c'] [('a',(1,3)),('b',(1,4)),('c',(2,1)),('a',(3,4)),('b',(3,2)),('c',(3,4))] [] 0 [1,4]

> diaAuto = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22] ['a','b'] [('a',(0,1)),('a',(1,2)),('a',(2,3)),('a',(3,4)),('a',(4,5)),('a',(5,6)),('a',(6,1)),('a',(0,7)),('a',(4,15)),('a',(15,16)),('a',(16,17)),('a',(17,18)),('a',(18,19)),('a',(19,15)),('a',(9,19)),('a',(18,22)),('a',(20,21)),('b',(7,8)),('b',(8,9)),('b',(9,10)),('b',(10,11)),('b',(11,12)),('b',(12,13)),('b',(13,14)),('b',(14,7)),('b',(13,10)),('b',(10,20))] [] 0 [21,22]

> automataGen = NFAe [0,1,2,3,4] ['a','b','c'] [('c',(0,2)),('c',(0,0)),('a',(1,1)),('a',(1,2)),('b',(1,0)),('b',(1,3)),('c',(1,1)),('a',(2,2)),('b',(2,0)),('b',(2,2)),('c',(2,3)),('a',(3,2)),('a',(3,3)),('b',(3,2)),('b',(3,3)),('c',(3,2)),('c',(4,1)),('c',(3,4))] [] 0 [0,2]

> newSimple = NFAe [0,1,2] ['a','b'] [('a',(0,1)),('b',(0,1)),('b',(1,0)),('b',(2,1))] [] 0 [0,1]