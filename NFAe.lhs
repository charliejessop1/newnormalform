> module NFAe where

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
> import NFA

> data NFAe = NFAe {
>	eStates :: [Vertex]
>	, eAlphabet :: [Char]
>	, eTransitions :: [(Char,Edge)]
>   , epsilons :: [Edge]
>	, eInitial :: Vertex
>	, eFinal :: [Vertex]
>	} deriving Show

> testNFAe = NFAe [0,1,2,3] ['a','b'] [('a',(0,1)),('b',(1,1)),('a',(2,2)),('b',(2,3))] [(1,2)] 0 [3]

> toNFAe :: NFA -> NFAe
> toNFAe a = NFAe (states a) (alphabet a) (transitions a) [] (initial a) (final a)

moveAutomata takes the vertex of an automata and returns all reachable Vertices in that Automata

> moveNFAe :: NFAe -> Char -> Vertex -> [Vertex]
> moveNFAe a c v =[y | (input,(x,y)) <- (eTransitions a), input == c, x == v] ++ [q | (p,q) <- epsilons a, p == v]

runAutomata takes an automata, an input string and a set of automata states and runs the automata from each state to return the set of reached states.

> runNFAe :: NFAe -> String ->[Vertex] -> [Vertex]
> runNFAe _ [] vs = vs
> runNFAe _ _ [] = []
> runNFAe a s vs = runNFAe a (tail s) ((getAlle a) $ removeDuplicates (concat $ map newVs vs))
> 		where {	newVs y = [q | (c,(p,q)) <- eTransitions a, p == y, c == (head s)]}

> getAlle :: NFAe -> [Vertex] -> [Vertex]
> getAlle a vs = if new /= [] then getAlle a (vs ++ new) else vs
>		where new = [y | (x,y) <- epsilons a, not (elem y vs), elem x vs]

--finished this now!

> acceptNFAe :: NFAe -> String -> Bool 
> acceptNFAe a str = if or (map ((flip elem) (eFinal a)) (runNFAe a str [eInitial a])) 
>					then True 
>					else False



> testNFAeEquivalence:: Int -> Int -> [Vertex] -> [Vertex] -> NFAe -> NFAe -> Bool
> testNFAeEquivalence i max vas vbs a b = if (i == max) then True else (	if ((acceptA && acceptB)||((not acceptA) && (not acceptB))) 
>																		then testNFAeEquivalence (i+1) max newVas newVbs a b 
>																		else False )
>				where 	{	
>							newVas = runNFAe a [(head $ tail $ eAlphabet a)] vas;
>							newVbs = runNFAe b [(head $ eAlphabet b)] vbs;
>							acceptA = (intersect newVas (eFinal a)) /= [];
>							acceptB = (intersect newVbs (eFinal b)) /= []
>						}

> findWordse :: Int -> Int -> ([(String,[Vertex])],[String]) -> NFAe -> ([(String,[Vertex])],[String])
> findWordse i max xs a = 	if ((length (snd xs)) >= max) || (newStringStates == []) || (i > 1000)
>							then xs 
>							else findWordse (i+1) max (newStringStates,(snd xs)++finals) a
>			where {	newStringStates = [y | y <- concat $ map (extendWorde a) (fst xs), (snd y) /= []];
>					finals = [str | (str,vs) <- newStringStates, vs /= [], or (map ((flip elem) (eFinal a)) vs)];}

> extendWorde :: NFAe -> (String,[Vertex]) -> [(String,[Vertex])]
> extendWorde a (str,vs) = zip zips (zipWith newVs (zips) (repeat vs))
>			where {	zips = (zipWith (++) (repeat str) (map (\y -> [y]) (eAlphabet a)));
>					newVs str v = getAlle a [y | (c,(x,y)) <- eTransitions a, elem x v, c == (head $ reverse $ str)];
>					plusEps vs = [q | (p,q) <- epsilons a, elem p vs];}
