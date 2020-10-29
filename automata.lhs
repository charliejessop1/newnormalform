> module Automata where

> import Prelude
> import Data.List
> import Data.Ord
> import Data.Matrix hiding ((!),flatten) 
> import qualified Data.Vector             as V
> import qualified Data.Vector.Mutable     as MV
> import Data.Graph
> import Data.Array
> import Matrix
> import Util

Two kinds of automata are defined: a standard automata and a Chrobak Normal Form Automata. They both contin the same information but the Chrobak has its states and transitions segregated into tail and loop.

> data UnaryAutomata = UnaryAutomata {
>	unStates :: [Vertex] 
>	, unAlphabet :: Char
>	, unTransitions :: [Edge] 
>	, unInitial :: Vertex
>	, unFinal :: [Vertex]
>	} deriving Show



> data ChrNF = ChrNF {
>	tailStates :: [Vertex]
>	, loopStates :: [[Vertex]]
>	, chrAlphabet :: Char
>	, tailTransitions :: [Edge]
>	, loopTransitions :: [[Edge]]
>	, chrInitial :: Vertex 
>	, chrFinal :: [Vertex]
>	} deriving Show

ConvertC converts from a Chrobak Normal Form Automata into a standard Automata with a concatenation of the loop and tail : states and transitions

> convertC :: ChrNF -> UnaryAutomata
> convertC c = UnaryAutomata ((tailStates c)++(concat (loopStates c))) (chrAlphabet c) ((tailTransitions c)++(concat (loopTransitions c))) (chrInitial c) (chrFinal c)

> convertCnum :: ChrNF -> UnaryAutomata
> convertCnum c = UnaryAutomata newStates (chrAlphabet c) newTransitions (mapVertex statesMap (chrInitial c)) (map (mapVertex statesMap) (chrFinal c))
> 	where {
>		oldStates = (tailStates c) ++ (concat (loopStates c));
>		newStates = [0..((length oldStates)-1)];
>		statesMap = zip oldStates newStates;
>		oldTransitions = ((tailTransitions c)++(concat (loopTransitions c)));
>		newTransitions = zip (map (mapVertex statesMap) (map fst oldTransitions)) (map (mapVertex statesMap) (map snd oldTransitions));}

moveAutomata takes the vertex of an UnaryAutomata and returns all reachable Vertices in that Automata

> moveAutomata :: UnaryAutomata -> Vertex -> [Vertex]
> moveAutomata a i = [q | (p,q) <- unTransitions a, p == i]

> runAutomata :: UnaryAutomata -> String ->[Vertex] -> [Vertex]
> runAutomata _ [] vs = vs
> runAutomata _ _ [] = []
> runAutomata a s vs = 	(runAutomata a (tail s) (removeDuplicates (concat $ map newVs vs)))
> 		where newVs y = [q | (p,q) <- unTransitions a, p == y]

> acceptAutomata :: UnaryAutomata -> String -> Bool
> acceptAutomata a str = if or (map ((flip elem) (unFinal a)) (runAutomata a str [unInitial a])) 
>						 then True 
>						 else False


unary [1,2,3] [aaaaa]

 > runAutomata2 :: UnaryAutomata -> String -> [Vertex] -> [Vertex]
 > runAutomata2 a xs verts = un

runAutomata takes an automata, an input string and a set of automata states and runs the automata from each state to return the set of reached states.

 > runAutomata :: UnaryAutomata -> String -> [Vertex] -> [Vertex]
 > runAutomata a [] verts = verts
 > runAutomata a (x:xs) verts = (runAutomata a xs (removeDuplicates (concat $ map (moveAutomata a) verts)))

> automataToGraph :: UnaryAutomata -> Graph
> automataToGraph a = buildG (minimum (unStates a), maximum (unStates a)) (unTransitions a)

> removeDuplicateTransitions :: [(Char,Edge)] -> [(Char,Edge)]
> removeDuplicateTransitions [] = []
> removeDuplicateTransitions (x:xs) = if elem x xs then removeDuplicateTransitions xs else [x] ++ removeDuplicateTransitions xs

> mapToStdVerts :: Int -> ([Vertex]) -> [Edge]
> mapToStdVerts _ [] = []
> mapToStdVerts i (y:ys) = [(y,i)] ++ (mapToStdVerts (i+1) ys)

> flipMap :: [Edge] -> [(Vertex, Vertex)]
> flipMap (x:xs) = [(snd x, fst x)] ++ (flipMap xs)

> mapVertex :: [Edge] -> Vertex -> Vertex
> mapVertex (x:xs) v = if ((fst x) == v) then (snd x) else (mapVertex xs v)

> mapPair :: [Edge] -> Edge -> [Edge]
> mapPair es (p,q) =concat $ map (\x -> map (\y -> (x,y)) (mapVertex' es q)) (mapVertex' es p)

> mapVertex' :: [Edge] -> Vertex -> [Vertex]
> mapVertex' [] _ = []
> mapVertex' (x:xs) v = if ((fst x) == v) then [snd x]++(mapVertex' xs v) else (mapVertex' xs v) 
 

> mapAutomata :: UnaryAutomata -> [Edge] -> UnaryAutomata
> mapAutomata un1 maps = UnaryAutomata 
>							( (map (mapVertex maps) (unStates un1)))
>							(unAlphabet un1)
>							(zip ( (map (mapVertex maps) (map fst (unTransitions un1)))) ( (map (mapVertex maps) (map snd (unTransitions un1)))))
>							( (mapVertex maps (unInitial un1)))
>							( (map (mapVertex maps) (unFinal un1)))

> testEquivalence :: Int -> UnaryAutomata -> UnaryAutomata -> Bool
> testEquivalence n a b = and (zipWith (==) (map (acceptAutomata a) listWords) (map (acceptAutomata b) listWords))
>	where { listWords = map (copy1 'a') [0..n];
>			copy1 item qty = [item|_<-[1..qty]];}
