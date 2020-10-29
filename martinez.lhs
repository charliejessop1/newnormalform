> module Martinez where

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

ALGORITHM 3

> automata3 = UnaryAutomata [0,1,2,3,4,5,6,7,8,9,10] 'a' [(0,1),(1,2),(2,3),(3,4),(4,5),(5,3),(2,6),(6,8),(6,6),(2,7),(7,8),(8,9),(9,10),(10,7)] 0 [2,5,6,9,10]
> myGraph =buildG (0,10) (unTransitions automata3)
> mySCCs = getAllSCCSubGraphs myGraph

> automata1 = UnaryAutomata [0,1,2,3,4] 'a' [(0,1),(1,2),(2,1),(1,3),(3,4),(4,4)] 0 [2,4]

> automata2 = UnaryAutomata [0,1,2,3] 'a' [(0,1),(0,2),(0,3),(1,0),(1,2),(1,3),(2,0),(2,1),(2,3),(3,0),(3,1),(3,2)] 0 [0]

> automata5 = UnaryAutomata [0..20] 'a' [(0,1),(1,2),(2,3),(3,4),(4,5),(5,6),(6,1),(0,7),(7,8),(8,9),(9,10),(10,11),(11,12),(12,13),(13,14),(14,7),(13,10),(10,20),(9,19),(4,15),(15,16),(16,17),(17,18),(18,19),(19,15),(18,20)] 0 [20]

26

 
 > automataZ = UnaryAutomata [0,1,2,3,4,5,6,7,8] 'a' [(0,1),(1,2),(2,3),(2,4),(4,5),(5,6),(6,7),(7,5),(7,8),(8,2)] 0 [1,6,8]
 > n = length (unStates automataZ)
 > m = (n*n)+n
 > localSccs = getAllSCCSubGraphs (automataToGraph automataZ)
 > y	 = map getSimpleLoopGCD (map graphToMatrix localSccs)
 > k = length y
 > initialA = initChrobak m k y 'a'
 > tailStates1 = getTailFinalStates [0..m-1] automataZ [0]
 > loopStates1 = getLoopFinalStates (head $ reverse $((loopStates initialA)!!(snd (maximumBy (comparing fst) (zip y [0..]))))) automataZ initialA  localSccs (updateSccVertices (runSccAutomata automataZ (replicate (m) 'a') [(0,Nothing)]) localSccs) (map head (loopStates initialA))
 > finalA = addChrFinalStates initialA ((tailStates1)++(loopStates1))
 > finalA2 = simplifyChrobak finalA

Need state for 

> simplifyChrobak :: ChrNF -> ChrNF
> simplifyChrobak a = if ((cond1 /= cond2) && (length (tailStates a) /= 1))
>						then simplifyChrobak (ChrNF (map (mapVertex totalMap) (delete qm1 (tailStates a))) (map (map (mapVertex totalMap)) (loopStates a)) (chrAlphabet a) (concat $ map (mapPair totalMap) (init (tailTransitions a))) (map concat (map (map (mapPair totalMap)) (zipWith (++)  (fmap (:[]) (zip (repeat qm2) (map head (loopStates a)))) (map tail (loopTransitions a))))) (mapVertex totalMap (chrInitial a)) (map (mapVertex totalMap) (delete qm1 (chrFinal a))))
>						else a 
>			 where {cond1 = elem qm1 (chrFinal a);
>					cond2 = (intersect (chrFinal a) p) == []; 
>					p = map head (map (reverse) (loopStates a)); 
>					qm1 = head (reverse (tailStates a));
>					qm2 = head $ tail (reverse (tailStates a));
>					tailStatesMap = zip (tailStates a) (tailStates a);
>					loopStatesMap = zip (concat (loopStates a)) (map pred (concat (loopStates a)));
>					totalMap = tailStatesMap ++ loopStatesMap;}



> automataG = UnaryAutomata [0,1,2,3] 'a' [(0,1),(0,2),(0,3),(1,0),(1,2),(1,3),(2,0),(2,1),(2,3),(3,0),(3,1),(3,2)] 0 [0]
> automataG2 = UnaryAutomata [0,1,2] 'a' [(0,1),(1,2),(2,2)] 0 [0,2]

> convertToChrobak :: UnaryAutomata -> ChrNF 
> convertToChrobak a = addChrFinalStates init (tailS++loopS)
> 						where {	init = initChrobak m k y (unAlphabet a);
>								n = length (unStates a);
>								m = (n*n)+n;
>								tgraphs = getAllSCCSubGraphs (automataToGraph a);
>								y = map getSimpleLoopGCD (map graphToMatrix tgraphs);
>								k = length y;
>								tailS = getTailFinalStates [0..m-1] a [unInitial a];
>								loopS = removeDuplicates $ getLoopFinalStates (head $ reverse $((loopStates init)!!(snd (maximumBy (comparing fst) (zip y [0..]))))) a init tgraphs (runSccAutomata a (replicate (m) (unAlphabet a)) [((unInitial a),Nothing)]) (map head (loopStates init))}	


THIS IS STILL NOT QUITE RIGHT, NEED TO CHECK THE MOST RECENT SCC NOT WHETHER THE FINAL VERTEX IS IN THE SCC... THIS IS A FUNDAMENTAL ERROR

finalVertex -> originalAutomata -> ChrNFAutomata -> [SCCs] -> [(currentOriginAutomataState,Int)] -> [chrNFAutomataState] -> [finalStates]


 finalSccs is the intersect of the final Ori states with the current oriStates intersect with each of the scc graphs (WHICH IS WRONG) -> [[Vertex]] first line of vertices is graph1 final state vertices, second line is graph2 final state vertices etc. 
 MAKE IT: filter the finalOriState tuples down to just the ones that are final, then somehow get the graph vertices of the chrobak
 
 I have the set of current states with the scc marker and I have the set of final states. Want to
 
 I have the set of final states with the associated last scc marker
 
 finalSccs is the 
 
> getLoopFinalStates :: Vertex -> UnaryAutomata -> ChrNF -> [Graph] -> [(Vertex,Maybe Int)] -> [Vertex] -> [Vertex]
> getLoopFinalStates fin oriAuto chrAuto sccs oriVs chrVs = (getFinalSccVertices finalSccs chrAuto newChrStates)  ++ recur
> 			where {	finalSccs = concat (map (\x -> if elem (fst x) (unFinal oriAuto) then [x] else []) newOriStates);
>					newOriStates = updateSccVertices(runSccAutomata oriAuto [unAlphabet oriAuto] oriVs) sccs;
>					newChrStates = runAutomata (convertC chrAuto) [chrAlphabet chrAuto] chrVs;
>					recur = if ((intersect chrVs [fin])/=[]) then [] else getLoopFinalStates fin oriAuto chrAuto sccs newOriStates newChrStates;
>				  }

Take the set of vertices from the oriAuto, the ChrNF automata, and the set of ChrNF Vertices

> getFinalSccVertices :: [(Vertex,Maybe Int)] -> ChrNF -> [Vertex] -> [Vertex]
> getFinalSccVertices [] _ _ = []
> getFinalSccVertices (x:xs) auto chrNFVertices = if (snd x) == Nothing then [] else (intersect ((loopStates auto)!!(fromJust (snd x))) chrNFVertices)++ getFinalSccVertices xs auto chrNFVertices

> runSccAutomata :: UnaryAutomata -> String -> [(Vertex,Maybe Int)] -> [(Vertex,Maybe Int)]
> runSccAutomata a [] verts = verts
> runSccAutomata a (x:xs) verts = (runSccAutomata a xs (removeDuplicates (concat $ map (moveSccAutomata a) verts)))

> moveSccAutomata :: UnaryAutomata -> (Vertex,Maybe Int) -> [(Vertex,Maybe Int)]
> moveSccAutomata a (i,scc) = zip (map snd (nodeEdges i (unTransitions a))) (repeat scc)

> updateSccVertices :: [(Vertex,Maybe Int)] -> [Graph] -> [(Vertex,Maybe Int)]
> updateSccVertices [] _ = []
> updateSccVertices (x:xs) gs = [((fst x), (intToPut (snd x) newIndex))]++(updateSccVertices xs gs)
>				where {newIndex = (elemIndex True (map (containsVertex (fst x)) gs));
>						intToPut _ (Just y) = (Just y);
>						intToPut (Just p) Nothing = (Just p);
>						intToPut Nothing Nothing = Nothing}

> containsVertex :: Vertex -> Graph -> Bool 
> containsVertex v g = if  elem v (vertices g)
>						then True else False

> testAutomataEquivalence:: Int -> Int -> [Vertex] -> [Vertex] -> UnaryAutomata -> UnaryAutomata -> Bool
> testAutomataEquivalence i max vas vbs a b = if (i == max) then True else (if ((acceptA && acceptB)||((not acceptA) && (not acceptB))) then testAutomataEquivalence (i+1) max newVas newVbs a b else False )
>				where 	{	
>							newVas = runAutomata a [(unAlphabet a)] vas;
>							newVbs = runAutomata b [(unAlphabet b)] vbs;
>							acceptA = (intersect newVas (unFinal a)) /= [];
>							acceptB = (intersect newVbs (unFinal b)) /= []
>						}

UnaryAutomata -> String -> [Vertex] -> [Vertex]

> addFinalStates :: UnaryAutomata -> [Vertex] -> UnaryAutomata
> addFinalStates a vs = UnaryAutomata (unStates a) (unAlphabet a) (unTransitions a) (unInitial a) ((unFinal a)++vs)

> addChrFinalStates :: ChrNF -> [Vertex] -> ChrNF
> addChrFinalStates a vs = ChrNF (tailStates a) (loopStates a) (chrAlphabet a) (tailTransitions a) (loopTransitions a) (chrInitial a) ((chrFinal a)++vs)

> initChrobak :: Int -> Int -> [Int] -> Char -> ChrNF
> initChrobak m k y chr = ChrNF tails loops chr tailTrans loopTrans  0 []
>		where { tails = [0..(m-1)];
>				loops = yStates m y;
>				tailTrans = zip [0..(m-2)] [1..(m-1)];
>				loopTrans = yTransitions m m y}

Takes the set of tail states, the automata in question and the current set of states reachable

> getTailFinalStates :: [Vertex] -> UnaryAutomata -> [Vertex] -> [Vertex]
> getTailFinalStates [] auto vs = []
> getTailFinalStates (x:xs) auto vs = 	if (intersect vs (unFinal auto)) /= []
>									then [x] ++ (getTailFinalStates xs auto newStates)
>									else getTailFinalStates xs auto newStates
>					where newStates = removeDuplicates (concat (map (moveAutomata auto) vs))

Want to progress the automata around each of the loops, at each progression check to see if it is a final state, if so check which SCC it is part of -> assign that the relevant loop the current final state.

newStates is the set of all states reachable in that order
(map vertices sccs) is an [[Vertex]]
(intersect (final auto) vs) is any current states that is a final state in the automata :: [Vertex]
map (intersect (intersect (final auto) vs)) (map vertices sccs) is the intersect with the current final states with each set of vertices
yielding [[Vertex]] where each row is any SCC vertex which is in the current final state set
recur: (intersect vs [fin])\=[] 
if (any of the current states is the final state) then [] else recur with another loop with the new states 

> yTransitions :: Int -> Int -> [Int] -> [[Edge]]
> yTransitions _ _ [] = []
> yTransitions m z (y:ys) = [([(m-1,z)]++(zip [z..(z+y-2)] [z+1..(z+y-1)])++[(z+(y-1),z)])] ++ (yTransitions m (z+y) ys)

> yStates :: Int -> [Int] -> [[Vertex]]
> yStates z [] = []
> yStates z (y:ys) = [[z..(z+y-1)]] ++ (yStates (z+y) ys)

ALGORITHM 1

S = empty set
A adjacency matrix of G
for each vertex i 
	compute A^i boolean matrix multiplication
	if A^i has non zero entry on diagonal then it is a loop 
	Add that integer to the set S 
end for 
set g to be GCD of elements in S 
return g

 > getLoops :: (Num a,Ord a) => Matrix a -> Int 
 > getLoops m = gcd' (loop1 (nrows m) m)

> getSimpleLoopGCD :: (Num a, Ord a) => Matrix a -> Int
> getSimpleLoopGCD m =  foldl gcd (head lengths) (tail lengths)
>	where {	lengths = (lenLoop (nrows m) m);
>			lenLoop x m = if x == 0 then [] 
>					else(	if trace (m^x) > 0 
>							then x:(lenLoop (x-1) m)
>							else lenLoop (x-1) m);}
		

This algorithm loops through the number of rows in the matrix, raises the matrix to a power. Currently not used in favour of existing implementation in Data.Graph 

> loop1 :: (Num a, Ord a) => Int -> Matrix a -> [Int]
> loop1 0 m = 	[]
> loop1 x m = if trace (m^x) > 0
>					then x:(loop1 (x-1) m)
>					else loop1 (x-1) m

This is an algorithm which recursively calls the gcd function -> giving the gcd of a list of items

> gcd' :: (Integral a) => [a] -> a
> gcd' [] = 1
> gcd' [x] = x
> gcd' (x:xs) = gcd x (gcd' xs)