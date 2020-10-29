> module Util where

> import Prelude

Removes duplicates from a set

> removeDuplicates :: Eq a => [a] ->[a]
> removeDuplicates = foldl (\y x -> if elem x y
>                                       then y
>                                       else y ++ [x]) []

