> module Toggle where

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
> import Automata

> data ToggleForm = ToggleForm {
>	togTailStates :: [Vertex]
>	, togLoopStates :: [[Vertex]]
>	, togAlphabet :: Char
>	, togTailTransitions :: [Edge]
>	, togLoopTransitions :: [[Edge]]
>	, togInitial :: Vertex
>	, togFinal :: [Vertex]
>	, chr2OriTog :: [(Vertex,Vertex)]
>	} | Empty deriving Show

> testToggleEquivalence:: Int -> Int -> [Vertex] -> [Vertex] -> [(Vertex,Char)] -> NFA -> ToggleForm -> Bool
> testToggleEquivalence i max vas vbs toggles auto tog = 	if (i == max) 
>															then True 
>															else (	if equiToggles && ((acceptA && acceptB) || ((not acceptA) && (not acceptB)))
>																	then testToggleEquivalence (i+1) max newVas newVbs toggles auto tog 
>																	else False )
>				where 	{	
>							newVas = runNFA auto [togAlphabet tog] vas;
>							newVbs = runToggle tog [togAlphabet tog] vbs;
>							autoToggles = removeDuplicates [x | x <- newVas, elem x (map fst toggles)];
>							togToggles = removeDuplicates [y | y <- (map (mapVertex (chr2OriTog tog)) [z | z <- newVbs, elem z (map fst (chr2OriTog tog))])];
>							acceptA = (intersect newVas (final auto)) /= [];
>							acceptB = (intersect newVbs (togFinal tog)) /= [];
>							equiToggles = if autoToggles == togToggles then True else False;
>						}

> moveToggle :: ToggleForm -> Vertex -> [Vertex]
> moveToggle t v = [b | (a,b) <- ((togTailTransitions t)++(concat $ togLoopTransitions t)),a == v]

> runToggle :: ToggleForm -> String -> [Vertex] -> [Vertex]
> runToggle a [] verts = verts
> runToggle a (x:xs) verts = (runToggle a xs (removeDuplicates (concat $ map (moveToggle a) verts)))
