> module Arbitrary where

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
> import Binary
> import NFA 

> getAllArbToggles :: NFA -> [(Vertex,Char)]
> getAllArbToggles a = sort $ concat $ map (getArbitraryToggles a) (alphabet a)

> getArbitraryToggles :: NFA -> Char -> [(Vertex,Char)]
> getArbitraryToggles a c = [(init,chr) | init <- (states a),  --This is always just the initial state of the automata that should be considered a "toggle"
>										elem init [initial a], 
>										(chr, ed) <- (transitions a), 
>										(fst ed) == init,
>										chr == c] ++ 
>										[(x,c) | 	x <- (states a),  
>													(	(elem x (map fst charTrans)) && 
>														(elem x (map snd notCharTrans)))]
>		where	{ 	charTrans = [edge1 | (character1,edge1) <- (transitions a), character1 == c];
>					notCharTrans = [edge1 | (character1,edge1) <- (transitions a), character1 /= c]};

> convertArbAutomata :: NFA -> NFA 
> convertArbAutomata a = foldl (addToggleForm togMap) 	(initNFA toggles togMap (alphabet a)) (map (toToggleForm toggles) (zip (map snd chrobaks) unarys))
>			where {	toggles = removeDuplicates $ getAllArbToggles a;
>					togMap = mapToStdVerts 0 (map fst toggles); --HERE IS THE PROBLEM, WHEN YOU HAVE MULTIPLE TOGGLE POINTS WITH THE SAME NUMBER BUT MULTIPLE LETTERS.... ACTUALLY THIS SHOULD WORK...
>					unarys = map (getUnary a (concat (map (decomposeByContig a ) (decomposeByLetter a (alphabet a))))) toggles;
>					chrobaks = zip (map fst toggles) ((map (linConvertToChrobak) (map fst unarys)));}

> testArbAutomata :: Int -> NFA -> NFA -> Bool
> testArbAutomata i a b = and (map (acceptNFA b) words)
>		where { words =	snd (findWords 0 i ([("",[initial a])],[]) a);}

> arbAutomata = NFA [0,1,2,3,4,5,6,7,8,9,10,11,12,13] ['a','b','c'] [('b',(0,1)),('b',(1,2)),('b',(2,3)),('b',(3,4)),('b',(4,1)),('a',(4,5)),('a',(5,6)),('a',(6,7)),('a',(7,8)),('a',(8,9)),('a',(9,5)),('a',(5,1)),('a',(3,10)),('a',(10,12)),('a',(12,11)),('a',(11,10)),('a',(12,13)),('c',(2,3)),('c',(3,6)),('c',(6,2))] 0 [13]

> arbAutomata2 = NFA [0,1,2,3,4,5,6,7,8,9,10,11,12,13] ['a','b','c'] [('b',(0,1)),('b',(1,2)),('b',(2,3)),('b',(3,4)),('b',(4,1)),('a',(4,5)),('a',(5,6)),('a',(6,7)),('a',(7,8)),('a',(8,9)),('a',(9,5)),('a',(5,1)),('a',(3,10)),('a',(10,12)),('a',(12,11)),('a',(11,10)),('a',(12,13)),('c',(2,3)),('c',(3,6))] 0 [13]


> testAuto = NFA [0,1,2,3] "ab" [('a',(0,1)),('a',(0,2)),('a',(0,3)),('b',(2,1)),('b',(3,1)),('b',(2,0))] 0 [1]

> testAuto1 = NFA [0,1,2] "ab" [('a',(0,0)),('a',(0,1)),('a',(1,2)),('a',(2,2)),('b',(0,1)),('b',(1,0)),('b',(1,2)),('b',(2,2))] 0 [2]

> testAuto2 = NFA [0,1,2] "ab" [('a',(1,1)),('a',(2,2)),('a',(0,2)),('b',(0,1)),('b',(1,2)),('b',(2,0))] 0 [2]

> a = testAuto
> toggles = removeDuplicates $ getAllArbToggles a
> togMap = mapToStdVerts 0 (map fst toggles)
> unarys = map (getUnary a (concat (map (decomposeByContig a ) (decomposeByLetter a (alphabet a))))) toggles
> chrobaks = zip (map fst toggles) (map linConvertToChrobak (map fst unarys))
> initAuto = (initNFA toggles togMap (alphabet a))
> toggleForms = (map (toToggleForm toggles) (zip (map snd chrobaks) unarys))
> newAuto = foldl (addToggleForm togMap) initAuto toggleForms





