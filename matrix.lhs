> module Matrix where

> import Prelude
> import Data.List
> import Data.Ord
> import Data.Matrix hiding ((!)) 
> import qualified Data.Vector             as V
> import qualified Data.Vector.Mutable     as MV
> import Data.Graph
> import Data.Array

> edgesToFlatMatrix :: (Int,Int) -> [Vertex] -> [Edge] -> [Int]
> edgesToFlatMatrix _ [] _ = []
> edgesToFlatMatrix (p,q) x allEdges = (createRow (q-p+1) (map ((\m n -> n-m) p) (map snd (nodeEdges (head x) (sort allEdges))))) ++ (edgesToFlatMatrix (p,q) (tail x) allEdges)

> nodeEdges :: Int -> [Edge] -> [Edge]
> nodeEdges _ [] = []
> nodeEdges y (x:xs) = if (fst x) == y
>						then [x] ++ (nodeEdges y xs)
>						else (nodeEdges y xs)

> createRow :: Int -> [Int] -> [Int]
> createRow i [] = (replicate (i) 0)
> createRow i (x:xs) = (replicate (x) 0) ++ [1] ++ (createRow (i-x-1) (map ((\y z -> z-(y+1)) x) xs))

want to pass back in the number of rows to be written with the new indices




> matrixToArrays :: Matrix a -> [[a]]
> matrixToArrays m = splitEvery (ncols m) (toList m)

> splitEvery _ [] = []
> splitEvery n list = first : (splitEvery n rest)
>  where
>    (first,rest) = splitAt n list

> arraysToNodeKeys :: Int -> [[Int]] -> [(Int, Int, [Int])]
> arraysToNodeKeys _ [] = []
> arraysToNodeKeys y (x:xs) = [(y, y, (getIndices 0 x))] ++ (arraysToNodeKeys (y+1) xs)

> getIndices :: Int -> [Int] -> [Int]
> getIndices _ [] = []
> getIndices y (x:xs) = if (x > 0)
>						then [y] ++ getIndices (y+1) xs
>						else getIndices (y+1) xs

Not used but kept for now

> matrixToGraph :: Matrix Int -> Graph
> matrixToGraph m = x
>				where (x,_,_) =	(graphFromEdges (arraysToNodeKeys 1 (matrixToArrays m)))

> graphToMatrix :: Graph -> Matrix Int
> graphToMatrix graph = fromList ((snd size) -(fst size)+1) ((snd size) -(fst size)+1) (edgesToFlatMatrix size (vertices graph) (filterEdges (fst size,snd size) (edges graph)))
>				where size = ((minimum $ vertices graph),(maximum $ vertices graph))

> filterEdges :: Bounds -> [Edge] -> [Edge]
> filterEdges _ [] = []
> filterEdges (p,q) (x:xs) = if and ([(fst x >= p)]++[fst x <= q]++[snd x >= p]++[snd x <= q])
>						then [x] ++ (filterEdges (p,q) xs)
>						else filterEdges (p,q) xs

> printMatrix :: Show a => Matrix a -> String
> printMatrix m =  if (nrows m) == 1
> 				   then  (show (getRow 1 m))
>				   else  (show (getRow 1 m)) ++ "\n" ++ (printMatrix (submatrix 2 (nrows m) 1 (ncols m) m))
