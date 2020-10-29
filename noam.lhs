> module Noam where

> import Prelude
> import NFAe

> toNoamSerial :: NFAe -> String
> toNoamSerial a = "#states\n" ++ states ++ "#initial\n" ++ "s" ++ (show $ eInitial a) ++ "\n" ++ "#accepting\n" ++ fins ++ "#alphabet\n" ++ alph ++ "#transitions\n" ++ trans ++ eps
> 	where {
> 		states =  concat $ zipWith (++) (zipWith (++) (repeat "s") (map show (eStates a))) (repeat "\n");
>		fins = concat $ zipWith (++) (zipWith (++) (repeat "s") (map show (eFinal a))) (repeat "\n");
>		alph = concat [[b] ++ "\n" | b <- eAlphabet a];
>		trans = concat $ map (\x -> ("s" ++ (show $ fst $ snd x)) ++ ":" ++ ([fst x]) ++ ">" ++ ("s" ++ (show $ snd $ snd x)) ++ "\n") (eTransitions a);
>		eps = concat $ map (\x -> ("s" ++ (show $ fst x)) ++ ":" ++ ("$") ++ ">" ++ ("s" ++ (show $ snd x)) ++ "\n") (epsilons a);
>			}

