> module JNFA where

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
> import Toggle
> import Arbitrary


> data JNFA = JNFA {
>	jStates :: [Vertex]
>	, jTogStates :: [[Vertex]]
>	, jAlphabet :: [Char]
>	, jTransitions :: [(Char,Edge)]
>	, jTogTransitions :: [[(Char,Edge)]]
>	, jInitial :: [Vertex]
>	, jFinal :: [Vertex]
>	} deriving Show

> moveJNFA :: JNFA -> Char -> Vertex -> [Vertex]
> moveJNFA a c v =[y | (input,(x,y)) <- (jTransitions a), input == c, x == v]

runAutomata takes an automata, an input string and a set of automata states and runs the automata from each state to return the set of reached states.

> runJNFA:: JNFA -> String -> [Vertex] -> [Vertex]
> runJNFA a [] verts = verts
> runJNFA a (x:xs) verts = (runJNFA a xs (removeDuplicates (concat $ map (moveJNFA a x) verts)))

> acceptJNFA :: JNFA -> String -> Bool 
> acceptJNFA a str = if or (map ((flip elem) (jFinal a)) (runJNFA a str (jInitial a))) then True else False

> initJNFA :: [(Vertex,Char)] -> [(Vertex,Vertex)] -> [Char] -> JNFA 
> initJNFA x toggleMaps inAlph = JNFA 	(map (mapVertex toggleMaps) (map fst x))
>							[]
>							inAlph
>							[]
>							[]
>							[0]
>							[]

--KEY POINT: a single "chrobak" point can lead to multiple toggle points. Here is the problem. 
-- Choices: 1. 

> convertToJNFA :: NFA -> JNFA
> convertToJNFA a = foldl (addToggleFormJNFA togMap) (initJNFA toggles togMap (alphabet a)) (map (toToggleForm toggles) (zip (map snd chrobaks) unarys))
>			where {	toggles = removeDuplicates $ getAllArbToggles a;
>					togMap = mapToStdVerts 0 (removeDuplicates $ map fst toggles);
>					unarys = map (getUnary a (concat (map (decomposeByContig a ) (decomposeByLetter a (alphabet a))))) toggles;
>					chrobaks = zip (map fst toggles) (map linConvertToChrobak (map fst unarys));}


> addToggleFormJNFA :: [Edge] -> JNFA -> (ToggleForm,[Edge]) -> JNFA
> addToggleFormJNFA togMap building (tog,orig2Un) = JNFA builtStates ((jTogStates building)++[plusStates]) (jAlphabet building) builtTransitions ((jTogTransitions building)++[extraTransitions]) (jInitial building) builtFinal
>		where 	{	--togShifted = 
>					allTogStates = ((togTailStates tog) ++ (concat (togLoopStates tog))); --all the toggle states
>					allTogTransitions = (togTailTransitions tog) ++ (concat (togLoopTransitions tog)); --all the toggle transitions
>					newTogMap = zip (map fst (chr2OriTog tog)) (map (mapVertex togMap) (map snd (chr2OriTog tog))); --chro Toggle states -> toggle states ->
>					nonToggleStates = [y | y <- allTogStates, not $ elem y (map fst newTogMap)];
>					nonTogMap = zip nonToggleStates [(length (jStates building))..((length (jStates building))+(length nonToggleStates))];
>					totalMap = newTogMap ++ nonTogMap;
>					plusStates = [x | x <- (map (mapVertex totalMap) allTogStates)];
>					extraStates = [x | x <- (map (mapVertex totalMap) allTogStates), not (elem x (jStates building))];
>					builtStates = (jStates building) ++ extraStates;
>					extraTransitions = (removeDuplicateTransitions (zip (repeat (togAlphabet tog)) (concat $ map (mapPair totalMap) allTogTransitions)));
>					builtTransitions = (jTransitions building) ++ extraTransitions;
>					builtFinal =  (jFinal building) ++ (map (mapVertex totalMap) (togFinal tog));}
 

> arbAutomata3 = NFA [0,1,2,3,4,5,6,7,8,9,10,11,12,13] ['a','b','c'] [('b',(0,1)),('b',(1,2)),('b',(2,3)),('b',(3,4)),('b',(4,1)),('a',(4,5)),('a',(5,6)),('a',(6,7)),('a',(7,8)),('a',(8,9)),('a',(9,5)),('a',(5,1)),('a',(3,10)),('a',(10,12)),('a',(12,11)),('a',(11,10)),('a',(12,13)),('c',(2,3)),('c',(3,6))] [0] [13]

> ja = testAuto
> jtoggles = removeDuplicates $ getAllArbToggles ja
> jtogMap = mapToStdVerts 0 (map fst jtoggles)
> junarys = map (getUnary ja (concat (map (decomposeByContig ja ) (decomposeByLetter ja (alphabet ja))))) jtoggles
> jchrobaks = zip (map fst jtoggles) (map linConvertToChrobak (map fst junarys))
> jinitAuto = (initJNFA jtoggles jtogMap (alphabet ja))
> jtoggleForms = (map (toToggleForm jtoggles) (zip (map snd jchrobaks) junarys))
> jnewAuto = foldl (addToggleFormJNFA jtogMap) jinitAuto jtoggleForms
> jbuilding = (initJNFA toggles togMap (alphabet a)) 
> jtog = fst $ jtoggleForms !! 1
> jorig2Un = snd $ jtoggleForms !! 1
> jallTogStates = ((togTailStates jtog) ++ (concat (togLoopStates jtog)));
> jallTogTransitions = (togTailTransitions jtog) ++ (concat (togLoopTransitions jtog)); 
> jnewTogMap = zip (map fst (chr2OriTog jtog)) (map (mapVertex jtogMap) (map snd (chr2OriTog jtog)));
> jnonToggleStates = [y | y <- jallTogStates, not $ elem y (map fst jnewTogMap)];
> jnonTogMap = zip jnonToggleStates [(length (jStates jbuilding))..((length (jStates jbuilding))+(length jnonToggleStates))];
> jtotalMap = jnewTogMap ++ jnonTogMap;
> jplusStates = [x | x <- (map (mapVertex jtotalMap) jallTogStates)];
> jextraStates = [x | x <- (map (mapVertex jtotalMap) jallTogStates), not (elem x (jStates jbuilding))];
> jbuiltStates = (jStates jbuilding) ++ jextraStates;
> jextraTransitions = (removeDuplicateTransitions (zip (repeat (togAlphabet jtog)) (concat $ map (mapPair jtotalMap) jallTogTransitions)));
> jbuiltTransitions = (jTransitions jbuilding) ++ jextraTransitions;
> jbuiltFinal =  (jFinal jbuilding) ++ (map (mapVertex jtotalMap) (togFinal jtog));	








 