> module Lin where

> import Prelude
> import Data.List
> import Data.Ord
> import Data.Matrix hiding ((!)) 
> import qualified Data.Vector             as V
> import qualified Data.Vector.Mutable     as MV
> import Data.Graph
> import Data.Array
> import Automata
> import GraphFunctions
> import Util
> import Matrix
> import Data.Maybe
> import Martinez

-- makes the assumption that UnaryAutomata has states starting from 0 and going without interuption to the end...

> linConvertToChrobak :: UnaryAutomata -> ChrNF 
> linConvertToChrobak a = addChrFinalStates init (tailS++loopS)
> 						where {	n = length (unStates a);
>								m = (n*n)+n;
>								tgraphs = getAllSCCSubGraphs (automataToGraph a);
>								y = map getSimpleLoopGCD (map graphToMatrix tgraphs);
>								k = length y;
>								init = initChrobak m k y (unAlphabet a);
>								tailS = getTailFinalStates [0..m-1] a [unInitial a];
>								loopS = getAllLoopFins a init tgraphs}

 
runAutomata (convertC chrAuto) (replicate (length (tailStates chrAuto)) [chrAlphabet chrAuto]) [init]

This function returns the ChrobakNormal form with the loop states filtered down to just the provided index

> getAllLoopFins :: UnaryAutomata -> ChrNF -> [Graph] -> [Vertex]
> getAllLoopFins oriAuto chrAuto sccs = concat ( map getFinalStates1 (zip 	(map splitChrobak [0..((length sccs)-1)])
>																			(map (reachAutomata oriAuto sccs) sccs)))
>		where {	getFinalStates1 (chrA, oriA) = getFinalStates 	(runAutomata oriA 
>																			 tailInput
>																			[(unInitial oriA)])
>																(runAutomata (convertC chrA)
>																			tailInput
>																			[(unInitial (convertC chrA))]) 
>																(chrA, oriA);
>				splitChrobak i = ChrNF 	(tailStates chrAuto) 
>										([(loopStates chrAuto)!!i]) 
>										(chrAlphabet chrAuto) 
>										(tailTransitions chrAuto) 
>										([(loopTransitions chrAuto)!!i]) 
>										(chrInitial chrAuto) 
>										(intersect 	(chrFinal chrAuto) 
>													((tailStates chrAuto)++((loopStates chrAuto)!!i)));
>				tailInput = replicate ((length (tailStates chrAuto))) (unAlphabet oriAuto);}


> getFinalStates :: [Vertex] -> [Vertex] -> (ChrNF,UnaryAutomata) -> [Vertex]
> getFinalStates oriVs chrVs (chrAuto, filteredOri) = (	if (intersect newOriStates (unFinal filteredOri)) /= [] 
>														then newChrStates 
>														else []) ++ recur
> 			where {	newOriStates = runAutomata filteredOri [(unAlphabet filteredOri)] oriVs;
>					newChrStates = runAutomata (convertC chrAuto) [(unAlphabet filteredOri)] chrVs;
>					recur = if (elem (head $ reverse $ head $ loopStates chrAuto) chrVs) 
>							then [] 
>							else getFinalStates newOriStates newChrStates (chrAuto, filteredOri);}
 
These three algorithms construct a new automata using a base automata but removing all Sccs except a single provided one.
 
> reachAutomata :: UnaryAutomata ->[Graph] -> Graph -> UnaryAutomata
> reachAutomata a [] _ = a
> reachAutomata a (x:xs) scc = 	if (not $ elem (head $ vertices x) (vertices scc)) && (elem (head $ vertices x) (reachable (automataToGraph a) (head $ vertices scc)))
>								then reachAutomata (removeSubGraph a x) xs scc 
>								else reachAutomata a xs scc 

> removeSubGraph :: UnaryAutomata -> Graph -> UnaryAutomata
> removeSubGraph a g = UnaryAutomata 	((unStates a) \\ allVertices) 
>										(unAlphabet a) 
>										(removeEdges allVertices (unTransitions a)) 
>										(unInitial a) 
>										((unFinal a) \\ allVertices)
>				where 	{removeEdges _ [] = [];
>						removeEdges vs (x:xs) = if ((elem (fst x) vs) || (elem (snd x) vs)) 
>												then [] 
>												else [x] ++ (removeEdges vs xs);
>							allVertices = (map fst (edges g))++(map snd (edges g))  }


---------------------------------------------------------------------------------------------------------------------------

> automataZ = UnaryAutomata [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17] 'a' [(0,1),(1,2),(2,3),(3,4),(4,5),(5,1),(4,17),(3,6),(6,7),(7,8),(8,6),(8,17),(0,9),(9,10),(10,11),(11,12),(12,13),(13,14),(14,15),(15,9),(15,10),(12,16),(16,17)] 0 [17]

 > automataZ = UnaryAutomata [0,1,2,3,4,5,6,7,8] ['a'] [(0,1),(1,2),(2,3),(2,4),(4,5),(5,6),(6,7),(7,5),(7,8),(8,2)] 0 [1,6,8]

 > n = length (unStates automataZ)
 > m = (n*n)+n
 > localSccs = getAllSCCSubGraphs (automataToGraph automataZ)
 > y	 = map getSimpleLoopGCD (map graphToMatrix localSccs)
 > k = length y
 > initialA = initChrobak m k y 'a'

 > finalA = linConvertToChrobak automataZ
 > finalA2 = simplifyChrobak finalA

 > testAuto = UnaryAutomata [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17] "a" [(0,1),(1,2),(2,3),(3,4),(4,5),(5,1),(4,17),(3,6),(6,7),(7,8),(8,6),(8,17),(0,9),(9,10),(10,11),(11,12),(12,13),(13,14),(14,15),(15,9),(15,10),(12,16),(16,17)] 0 [17]
 
 > splitChrobak1 = splitChrobak initialA 0
 > chr = (map (splitChrobak initialA) [0..((length localSccs)-1)])

 > ori = (map (reachAutomata automataZ localSccs) localSccs)

 > zipped = zip chr ori

 > unconcatt = ( map getFinalStates1 (zip (map (splitChrobak initialA) [0..((length localSccs)-1)]) (map (reachAutomata automataZ localSccs) localSccs)))


 > fins = getFinalStates (runAutomata (convertC (chr!!0)) (replicate (length (tailStates (chr!!0))) 'a') [0]) (runAutomata (ori!!0) (replicate (length (tailStates (chr!!0))) 'a') [0]) ((chr!!0), (ori!!0))
 > chrStarts = (runAutomata (convertC (chr!!0)) (replicate ((length (tailStates (chr!!0)))) 'a') [0])
 > oriStarts = (runAutomata (ori!!0) (replicate ((length (tailStates (chr!!0)))) 'a') [0])
 > chr1 = runAutomata (convertC (chr!!0)) "a" chrStarts
 > ori1 = runAutomata (ori!!0) "a" oriStarts
 > finals1 = intersect ori1 (final (ori!!0))
 > elem1 = (elem (head $ reverse $ head $ loopStates (chr!!0)) oriStarts)
 > chr2 = runAutomata (convertC (chr!!0)) "a" chr1
 > ori2 = runAutomata (ori!!0) "a" ori1
 > finals2 = intersect ori2 (final (ori!!0))
 > elem2 = (elem (head $ reverse $ head $ loopStates (chr!!0)) ori1)
