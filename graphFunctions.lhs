> module GraphFunctions where

> import Prelude
> import Data.List
> import Data.Ord
> import Data.Matrix hiding ((!)) 
> import qualified Data.Vector             as V
> import qualified Data.Vector.Mutable     as MV
> import Data.Graph
> import Data.Array
> import Matrix

getAllSCCSubGraphs this finds all SCC graphs found in a given graph where each graph has the original node numberings and with non-present nodes filtered out.

> getAllSCCSubGraphs :: Graph -> [Graph]
> getAllSCCSubGraphs graph = sort $ map makeGraph subGraphs
>		where subGraphs = map (getVertexEdges (edges graph)) (myScc graph)

This is an implementation of the scc search which filters out the individual vertices not part of the SCC definition used by Martinz. Individual vertices can be included but only if they have an edge from themselves to themselves!! 

> myScc :: Graph -> [[Vertex]]
> myScc graph = [x | x <- filterAcyclic (stronglyConnComp (arraysToNodeKeys 0 (matrixToArrays(graphToMatrix graph )))), x /= []]

> filterAcyclic :: [SCC Int] -> [[Vertex]]
> filterAcyclic [] = []
> filterAcyclic (x:xs) = [(isAcyclic x )]++(filterAcyclic xs)

> isAcyclic :: SCC Int -> [Vertex]
> isAcyclic (AcyclicSCC a) = [] 
> isAcyclic (CyclicSCC b) = b

This takes a SCC tree and a set of edges and returns the relevant edges

> getVertexEdges :: [Edge] -> [Vertex] -> [Edge]
> getVertexEdges [] v = []
> getVertexEdges (x:xs) v = (if (intersect [fst x] v) /= [] && (intersect [snd x] v) /= [] then [x] else []) ++ (getVertexEdges xs v)

> makeGraph :: [Edge] -> Graph
> makeGraph [] = buildG (0,0) []
> makeGraph edges = buildG ((minimum ((map fst edges))),(maximum ((map fst edges)))) edges 

