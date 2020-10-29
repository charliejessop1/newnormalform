> module NFA where

> import Prelude
> import Data.List
> import Data.Ord
> import Data.Matrix 					hiding ((!),flatten) 
> import qualified Data.Vector			as V
> import qualified Data.Vector.Mutable	as MV
> import Data.Tree
> import Data.Graph
> import Data.Array
> import Util

> data NFA = NFA {
>	states :: [Vertex]
>	, alphabet :: [Char]
>	, transitions :: [(Char,Edge)]
>	, initial :: Vertex
>	, final :: [Vertex]
>	} deriving Show


moveAutomata takes the vertex of an automata and returns all reachable Vertices in that Automata

> moveNFA :: NFA -> Char -> Vertex -> [Vertex]
> moveNFA a c v =[y | (input,(x,y)) <- (transitions a), input == c, x == v]

runAutomata takes an automata, an input string and a set of automata states and runs the automata from each state to return the set of reached states.

 > runNFA:: NFA -> String -> [Vertex] -> [Vertex]
 > runNFA a [] verts = verts
 > runNFA a (x:xs) verts = (runNFA a xs (removeDuplicates (concat $ map (moveNFA a x) verts)))

> runNFA :: NFA -> String ->[Vertex] -> [Vertex]
> runNFA _ [] vs = vs
> runNFA _ _ [] = []
> runNFA a s vs = runNFA a (tail s) ( removeDuplicates (concat $ map newVs vs))
> 		where newVs y = [q | (c,(p,q)) <- transitions a, p == y, c == (head s)]

> acceptNFA :: NFA -> String -> Bool 
> acceptNFA a str = if or (map ((flip elem) (final a)) (runNFA a str [initial a])) 
>					then True 
>					else False



> testNFAEquivalence:: Int -> Int -> [Vertex] -> [Vertex] -> NFA -> NFA -> Bool
> testNFAEquivalence i max vas vbs a b = if (i == max) then True else (	if ((acceptA && acceptB)||((not acceptA) && (not acceptB))) 
>																		then testNFAEquivalence (i+1) max newVas newVbs a b 
>																		else False )
>				where 	{	
>							newVas = runNFA a [(head $ tail $ alphabet a)] vas;
>							newVbs = runNFA b [(head $ alphabet b)] vbs;
>							acceptA = (intersect newVas (final a)) /= [];
>							acceptB = (intersect newVbs (final b)) /= []
>						}

> findWords :: Int -> Int -> ([(String,[Vertex])],[String]) -> NFA -> ([(String,[Vertex])],[String])
> findWords i max xs a = 	if (length (snd xs)) >= max 
>							then xs 
>							else findWords (i+1) max (newStringStates,(snd xs)++finals) a
>			where {	newStringStates = [y | y <- concat $ map (extendWord a) (fst xs), (snd y) /= []];
>					finals = [str | (str,vs) <- newStringStates, vs /= [], or (map ((flip elem) (final a)) vs)];}

> extendWord :: NFA -> (String,[Vertex]) -> [(String,[Vertex])]
> extendWord a (str,vs) = zip zips (zipWith newVs (zips) (repeat vs))
>			where {	zips = (zipWith (++) (repeat str) (map (\y -> [y]) (alphabet a)));
>					newVs str v = [y | (c,(x,y)) <- transitions a, elem x v, c == (head $ reverse $ str)];}

> moveNFA' :: NFA -> Char -> Vertex -> [Vertex]
> moveNFA' a c v =[y | (input,(x,y)) <- (transitions a), input == c, x == v]


