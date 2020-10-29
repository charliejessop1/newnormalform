> module BinaryE where

> import Prelude
> import Data.List
> import Data.Ord
> import Data.Matrix 					hiding ((!),flatten) 
> import qualified Data.Vector			as V
> import qualified Data.Vector.Mutable	as MV
> import Data.Tree
> import Data.Graph
> import Data.Array
> import Martinez
> import Util
> import Automata
> import GraphFunctions
> import Lin
> import NFAe
> import Toggle  
> import Data.Csv
> import qualified Data.ByteString.Lazy as BSL

 > import BinaryTests

Algorithm 4:

"1. Find the toggle states:"

> findToggleStates :: NFAe -> [(Vertex,Char)]
> findToggleStates a = 	[(init,chr) | 	init <- (eStates a), 
>										elem init [eInitial a], 
>										(chr, ed) <- (eTransitions a), 
>										(fst ed) == init] ++ 
>						[(x,(if elem x (map fst firstSet) then 'a' else 'b')) | 
>										x <- (eStates a),  
>										((elem x (map fst firstSet)) && (elem x (map snd secondSet))) || 
>										((elem x (map snd firstSet)) && (elem x (map fst secondSet)))]  
> --vertices which has an incoming edge in the 'a' set and an outgoing edge in the 'b' set or vice 
>		where { firstSet = [edge1 | (character1,edge1) <- (eTransitions a), character1 == 'a']; -- set of 'a' eTransitions
>				secondSet = [edge2 | (character2,edge2) <- (eTransitions a), character2 == 'b']} -- set of 'b' eTransitions

 > toggles = findToggleStates binAutomata

"2. Create the Unarily Contiguous Automatons for each toggle state for each letter"

--creates a unary automata for each letter at a toggle point , and a that converts the original vertices to the new (0 starting) vertices.

> getUnary :: NFAe ->  [([Vertex],Char)] -> (Vertex,Char) -> (UnaryAutomata, [Edge])
> getUnary nfa [] (initV, chr) = (UnaryAutomata 		[(mapVertex orig2UnI initV)]
>														(chr)
>														[]
>														(mapVertex orig2UnI initV)
>														((map (mapVertex orig2UnI) (intersect [initV] (eFinal nfa)))),orig2UnI)
>				where {	orig2UnI = mapToStdVerts 0 [initV];}
> getUnary nfa (x:xs) (initV, chr) = if ((chr == (snd x)) && (elem initV (fst x)))
>									then (UnaryAutomata ( (map (mapVertex orig2Un) (fst x)))
>														(chr)
>														(zip ( (map (mapVertex orig2Un) (map fst trns))) ( (map (mapVertex orig2Un) (map snd trns))))
>														( mapVertex orig2Un initV)
>														((map (mapVertex orig2Un) (intersect (fst x) (eFinal nfa)))),orig2Un)
>									else getUnary nfa xs (initV, chr)
>				where {	trns = [e | (c,e) <- eTransitions nfa, c==chr, ((elem (fst e) (fst x)) && (elem (snd e) (fst x)))];
>						orig2Un = mapToStdVerts 0 (fst x);}

> decomposeByContig :: NFAe -> ([Vertex],Char) -> [([Vertex],Char)]
> decomposeByContig nfa (verts,chr) =  if verts == [] then [] else zip (convertedComps) (repeat chr)
>		where 	{	
>					maps = mapToStdVerts 0 verts;
>					revMaps = flipMap maps;
>					transitionsOverall = [e | (c,e) <- eTransitions nfa, c == chr];
>					mappedTransitions = zip ( (map (mapVertex maps) (map fst transitionsOverall))) ( (map (mapVertex maps) (map snd transitionsOverall)));
>					graph = buildG (0,(maximum (map snd maps))) mappedTransitions ;
>					comps = components graph;
>					convertedComps =  (map (map (mapVertex revMaps)) (map flatten comps))
>				}

> decomposeByLetter :: NFAe -> [Char] -> [([Vertex],Char)]
> decomposeByLetter _ [] = []
> decomposeByLetter nfa (c:cs) = [(newStates,c)] ++ (decomposeByLetter nfa cs)
>		where 	{
>				newTransitions = [(a,b) | (letter,(a,b)) <- (eTransitions nfa), letter == c];
>				newStates = [s | s <- (eStates nfa), elem s (map fst newTransitions) || elem s (map snd newTransitions)];
>				}

 > unarys = map (getUnary binAutomata (concat (map (decomposeByContig binAutomata ) (decomposeByLetter binAutomata (eAlphabet binAutomata))))) toggles

"3. Convert each UCA into ChrNF with each toggle state preserved" -- Now calling this ToggleForm

 > chrobaks = zip (map fst toggles) (map linConvertToChrobak (map fst unarys))

-- this give chr2OriTog!

> getAllToggles :: (UnaryAutomata,[Edge]) -> ChrNF -> [Graph] -> [(Vertex,Char)] -> [(Vertex,Vertex)]
> getAllToggles (un,orig2Un) chr sccs toggles = flattenToggles( (getTailToggles (tailStates chr) toggles (un,orig2Un) [unInitial un])
>										++ (getLoopToggles (un,orig2Un) chr sccs toggles))

-- I think this is redundant but not 100% sure (flattenToggles)

> flattenToggles :: [(Vertex,[Vertex])] -> [Edge]
> flattenToggles [] = []
> flattenToggles (x:xs) = (zip (repeat (fst x)) (snd x)) ++ (flattenToggles xs)

-- tail states -> toggle states -> Unary Automata -> eInitial states -> eFinal states in the Chrobak
-- toggleMap is 

> getTailToggles :: [Vertex] -> [(Vertex,Char)] -> (UnaryAutomata,[Edge]) -> [Vertex] -> [(Vertex,[Vertex])]
> getTailToggles [] toggles (un,orig2Un) vs = []
> getTailToggles (x:xs) toggles (un,orig2Un) vs = 	if intersects /= []
>									then [(x,intersects)] ++ (getTailToggles xs toggles (un,orig2Un) newStates)
>									else getTailToggles xs toggles (un,orig2Un) newStates
>					where {	newStates = removeDuplicates (concat (map (moveAutomata un) vs));
>							intersects = map (mapVertex (flipMap filteredOrig2Un)) (intersect vs (map snd filteredOrig2Un));
>							filteredOrig2Un = zip (intersect (map fst toggles) (map fst orig2Un)) (map (mapVertex orig2Un) (intersect (map fst toggles) (map fst orig2Un)));}

--Split Chrobak is the chrobak nf automatas without the other loops (one for each loop)
-- the downstream automata is the unary automata with the other sccs removeDuplicates
-- getToggleStates1 takes the split chrobak and it's associated downstream automata with all the appropriate eFinal states etc removed and runs getToggleStates on the downstream automata shifted upto the end of the tail position, the filtered chrobak automata run upto the end of the tail position, and the pair of the automatas

> getLoopToggles :: (UnaryAutomata,[Edge]) -> ChrNF -> [Graph] -> [(Vertex,Char)] -> [(Vertex,[Vertex])]
> getLoopToggles (un,orig2Un) chrAuto sccs toggles = concat ( map getToggleStates1 (zip 	(map (splitChrobak) [0..((length sccs)-1)]) 
>																					(map (reachAutomata un sccs) sccs)))
>				where 	{	getToggleStates1 (chrA, oriA) = getToggleStates 	
>													(runAutomata oriA (replicate ((length (tailStates chrA))) (unAlphabet un)) [unInitial un]) 
>													(runAutomata (convertC chrA) (replicate ((length (tailStates chrA))) (unAlphabet un)) [0])
>													(chrA, (oriA,orig2Un))
>													toggles;
>							splitChrobak i = ChrNF 	(tailStates chrAuto) 
>													([(loopStates chrAuto)!!i]) 
>													(chrAlphabet chrAuto) 
>													(tailTransitions chrAuto) 
>													([(loopTransitions chrAuto)!!i]) 
>													(chrInitial chrAuto) 
>													(intersect (chrFinal chrAuto) ((tailStates chrAuto)++((loopStates chrAuto)!!i)));
>						}

--getToggleStates takes the end tail downstream automata states and the end tail single loop ChrNF automata and the pair of the automatas and...:	
--		tests to see if the downstream automata new states contains any eFinal states
--		if so then it adds the current Chrobak states to the list
--		if not it adds nothing
--		it then recurrs anyway using the recur below
--newOriStates is the downstream automata run through another position
--newChrStates is the single loop chrNF automata run through another position 
--recur tests whether the chrStates is the last item in the loop if so it stops else it recurs by calling this function again

> getToggleStates :: [Vertex] -> [Vertex] -> (ChrNF,(UnaryAutomata,[Edge])) -> [(Vertex,Char)] -> [(Vertex,[Vertex])]
> getToggleStates oriVs chrVs (chrAuto, (filteredOri,orig2Un)) toggles = (if intersects /= [] 
>																then zip newChrStates (repeat intersects) 
>																else []) ++ recur
> 			where {	newOriStates = runAutomata filteredOri [(unAlphabet filteredOri)] oriVs;
>					newChrStates = runAutomata (convertC chrAuto) [(unAlphabet filteredOri)] chrVs;
>					recur = if (elem (head $ reverse $ head $ loopStates chrAuto) chrVs) 
>							then []
>							else getToggleStates newOriStates newChrStates (chrAuto, (filteredOri,orig2Un)) toggles;
>					intersects = intersect (map (mapVertex (flipMap orig2Un)) newOriStates) (map fst toggles);} 


> toToggleForm :: [(Vertex,Char)] ->  (ChrNF, (UnaryAutomata,[Edge])) -> (ToggleForm,[Edge])
> toToggleForm toggles (chr, (un,orig2Un)) = (ToggleForm (tailStates chr)
>								(loopStates chr)
>								(chrAlphabet chr)
>								(tailTransitions chr)
>								(loopTransitions chr)
>								(chrInitial chr)
>								(chrFinal chr)
>								(getAllToggles (un,orig2Un) chr (getAllSCCSubGraphs (automataToGraph un)) toggles),orig2Un)

 > toggleForms = map (toToggleForm toggles) (zip (map snd chrobaks) unarys)

(3.a optional) find any equivalent ChrNF Automata and combine into {multi tailed ChrNF}

"4. Assign the toggle states as 1 -> n to the eFinal Automata "

-- toggleMap (transforms original states -> eFinal states)

 > toggleMap = mapToStdVerts 0 (map fst toggles)

> initNFAe :: [(Vertex,Char)] -> [(Vertex,Vertex)] -> [Char] -> NFAe 
> initNFAe x toggleMaps inAlph = NFAe (map (mapVertex toggleMaps) (map fst x))
>							inAlph
>							[]
>							[]
>							0
>							[]

 > firstInit = initNFAe toggles toggleMap (eAlphabet binAutomata)

"5. For each ChrNF Append the states and eTransitions to the eFinal Automata, ignoring the toggle states and putting in the correct eTransitions to and from toggles."

orig2Un :-: 1. -> 2
chr2OriTog :-: 3 -> 1 
togMap  :-: 1 -> 4

$ togMap = [(0,0),(1,1),(3,2),(4,3)]

> addToggleForm :: [Edge] -> NFAe -> (ToggleForm,[Edge]) -> NFAe
> addToggleForm togMap building (tog,orig2Un) = NFAe builtStates (eAlphabet building) builtTransitions builtEpsilons (eInitial building) builtFinal
>		where 	{	--togShifted = 
>					allTogStates = ((togTailStates tog) ++ (concat (togLoopStates tog))); --all the toggle states
>					allTogTransitions = (togTailTransitions tog) ++ (concat (togLoopTransitions tog)); --all the toggle transitions
>					exceptTogStates = tail allTogStates;
>					exceptTogTransitions = [(x,y) | (x,y) <- allTogTransitions, elem x exceptTogStates && elem y exceptTogStates];
>					exceptMap = zip exceptTogStates [(length (eStates building))..((length (eStates building))+(length allTogStates))];
>					extraMap = ((head allTogStates),mapVertex togMap (mapVertex (chr2OriTog tog) (head allTogStates)));
>					totalMap = [extraMap] ++ exceptMap;
>					newStates = map (mapVertex totalMap) exceptTogStates;
>					newTransitions = zip (repeat $ togAlphabet tog) (concat $ map (mapPair totalMap) allTogTransitions);
>					newEpsilons = zip (map (mapVertex totalMap) $ map fst (chr2OriTog tog)) (map (mapVertex togMap) $map snd (chr2OriTog tog));
>					builtStates = (eStates building) ++ newStates;
>					builtTransitions = (eTransitions building) ++ newTransitions;
>					builtEpsilons = (epsilons building) ++ newEpsilons;
>					builtFinal =  (eFinal building) ++ (map (mapVertex totalMap) (togFinal tog));}

Tactic here is to place all the toggle states as new states but then to add epsilon transitions in and out of all of them to the associated toggle state 

--SERIOUS ISSUE WITH MY EPSILON IMPLEMENTATION - NEED TO SKIP A LETTER!!! SO WHEN MAKING A MOVE ALSO LOOK FOR ONGOING EPSILON TRANSITIONS AND TAKE THEM ALL!
 
 
$ foldl (addToggleForm togMap) firstInit toggleForms

> convertAutomata :: NFAe -> NFAe 
> convertAutomata a = foldl (addToggleForm togMap) (initNFAe toggles togMap (eAlphabet a)) (map (toToggleForm toggles) (zip (map snd chrobaks) unarys))
>			where {	toggles = findToggleStates a;
>					togMap = mapToStdVerts 0 (map fst toggles);
>					unarys = map (getUnary a (concat (map (decomposeByContig a ) (decomposeByLetter a (eAlphabet a))))) toggles;
>					chrobaks = zip (map fst toggles) (map simplifyChrobak (map linConvertToChrobak (map fst unarys)));}


-- coming into this function we're getting the eInitial state for theNFAe, and the toggle form. We run theNFAe from the appropriate start state and also the toggleForm. Want to verify that appropriate eFinal states are identified, but also that appropriate toggle states are. So at each step we should identify any toggle states in either version, and check that the two lists are the same.


> automata34 = NFAe [0,1,2,3,4,5,6,7,8,9,10] ['a','b'] [('a',(0,1)),('a',(1,2)),('a',(2,3)),('a',(3,4)),('a',(4,5)),('a',(5,3)),('a',(2,6)),('a',(6,8)),('a',(6,6)),('a',(2,7)),('a',(7,8)),('a',(8,9)),('a',(9,10)),('a',(10,7))] [] 0 [2,5,6,9,10]

> binAutomata = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13] ['a','b'] [('b',(0,1)),('b',(1,2)),('b',(2,3)),('b',(3,4)),('b',(4,1)),('a',(4,5)),('a',(5,6)),('a',(6,7)),('a',(7,8)),('a',(8,9)),('a',(9,5)),('a',(5,1)),('a',(3,10)),('a',(10,12)),('a',(12,11)),('a',(11,10)),('a',(12,13))] [] 0 [13]

