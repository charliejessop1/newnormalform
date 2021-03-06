module UnaryTests where

import NFAe
import Lin
import Automata
import Data.Csv
import Noam


convertNFAeToUn :: NFAe -> UnaryAutomata 
convertNFAeToUn a = UnaryAutomata (eStates a) 'a' (map snd (eTransitions a)) (eInitial a) (eFinal a)


test1 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11] ['a'] [('a',(1,2)),('a',(3,9)),('a',(3,10)),('a',(4,6)),('a',(5,4)),('a',(6,1)),('a',(8,5)),('a',(8,10)),('a',(10,0)),('a',(11,8)),('a',(11,9))] [] 0 [1,2,3,4,5,11]
 
test2 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28] ['a'] [('a',(0,8)),('a',(0,28)),('a',(1,2)),('a',(1,28)),('a',(3,11)),('a',(4,16)),('a',(5,12)),('a',(6,16)),('a',(6,22)),('a',(7,19)),('a',(8,21)),('a',(9,9)),('a',(9,17)),('a',(10,22)),('a',(11,1)),('a',(12,8)),('a',(14,18)),('a',(14,21)),('a',(15,19)),('a',(15,21)),('a',(16,19)),('a',(16,26)),('a',(17,9)),('a',(18,0)),('a',(19,4)),('a',(19,5)),('a',(21,4)),('a',(22,28)),('a',(23,19)),('a',(24,19)),('a',(25,21)),('a',(25,25)),('a',(28,8)),('a',(28,28))] [] 0 [2,4,5,6,7,11,14,16,17,18,19,20,21,22,23,26,28]
 
test3 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22] ['a'] [('a',(1,21)),('a',(1,22)),('a',(4,19)),('a',(7,21)),('a',(8,15)),('a',(9,14)),('a',(13,16)),('a',(16,19)),('a',(18,19)),('a',(20,3)),('a',(21,19))] [] 0 [2,3,4,5,6,8,14,15,18,19]
 
test5 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26] ['a'] [('a',(0,4)),('a',(0,7)),('a',(1,4)),('a',(2,9)),('a',(3,2)),('a',(4,18)),('a',(4,20)),('a',(5,18)),('a',(6,16)),('a',(6,24)),('a',(7,2)),('a',(9,26)),('a',(10,2)),('a',(10,25)),('a',(11,4)),('a',(12,11)),('a',(12,18)),('a',(13,17)),('a',(13,21)),('a',(15,18)),('a',(18,25)),('a',(18,26)),('a',(19,21)),('a',(20,7)),('a',(21,15)),('a',(21,20)),('a',(22,10)),('a',(22,11)),('a',(23,21)),('a',(23,25)),('a',(26,2))] [] 0 [4,5,6,8,10,11,13,15,16,24]
 
test6 = NFAe [0,1,2,3,4,5,6] ['a'] [('a',(0,4)),('a',(0,6)),('a',(3,1)),('a',(3,5)),('a',(4,6)),('a',(5,6))] [] 0 [4]
 
test7 = NFAe [0,1,2,3,4,5,6,7,8,9,10] ['a'] [('a',(0,6)),('a',(0,10)),('a',(4,6)),('a',(5,9)),('a',(5,10)),('a',(8,0)),('a',(8,9)),('a',(9,5)),('a',(9,9)),('a',(10,3))] [] 0 [3,4,5,6,7,8,9,10]
 
test8 = NFAe [0,1,2,3,4,5,6,7,8,9] ['a'] [('a',(0,6)),('a',(0,8)),('a',(1,2)),('a',(1,7)),('a',(3,1)),('a',(5,4)),('a',(5,7)),('a',(6,6)),('a',(6,8)),('a',(8,8))] [] 0 [3,5,6]
 
test9 = NFAe [0,1,2,3,4,5,6] ['a'] [('a',(0,2)),('a',(1,5)),('a',(2,0)),('a',(3,0)),('a',(3,2)),('a',(4,4)),('a',(4,5)),('a',(6,4))] [] 0 [0,1,4,5,6]
 
test10 = NFAe [0,1,2,3,4,5,6,7,8] ['a'] [('a',(0,1)),('a',(0,7)),('a',(1,3)),('a',(3,7)),('a',(4,5)),('a',(5,2)),('a',(8,5)),('a',(8,8))] [] 0 [0,2,3,5,8]
 
test11 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15] ['a'] [('a',(0,9)),('a',(0,10)),('a',(2,8)),('a',(3,7)),('a',(4,3)),('a',(5,14)),('a',(6,1)),('a',(7,3)),('a',(7,6)),('a',(8,4)),('a',(11,8)),('a',(11,10)),('a',(13,5)),('a',(13,9)),('a',(15,13)),('a',(15,14))] [] 0 [0,1,2,3,4,5,8,10,12,15]
 
test12 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16] ['a'] [('a',(0,6)),('a',(0,16)),('a',(1,8)),('a',(2,8)),('a',(2,15)),('a',(3,11)),('a',(3,16)),('a',(4,3)),('a',(5,2)),('a',(6,4)),('a',(6,6)),('a',(7,14)),('a',(8,2)),('a',(9,0)),('a',(11,12)),('a',(13,13)),('a',(13,14)),('a',(16,14)),('a',(16,15))] [] 0 [2,3,7,13,14,16]
 
test13 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27] ['a'] [('a',(0,11)),('a',(0,26)),('a',(1,0)),('a',(2,27)),('a',(5,21)),('a',(5,22)),('a',(8,26)),('a',(8,27)),('a',(9,2)),('a',(9,13)),('a',(10,13)),('a',(10,20)),('a',(11,27)),('a',(12,3)),('a',(13,14)),('a',(13,17)),('a',(17,0)),('a',(17,25)),('a',(18,10)),('a',(18,15)),('a',(21,18)),('a',(23,9)),('a',(25,2)),('a',(27,15))] [] 0 [0,1,8,9,13,15,16,18,21,22,24,26,27]
 
test14 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21] ['a'] [('a',(0,13)),('a',(0,14)),('a',(2,21)),('a',(3,14)),('a',(4,6)),('a',(5,11)),('a',(5,18)),('a',(6,0)),('a',(7,9)),('a',(7,15)),('a',(10,5)),('a',(11,3)),('a',(12,6)),('a',(13,11)),('a',(13,20)),('a',(15,5)),('a',(16,15)),('a',(17,8)),('a',(18,0)),('a',(18,18)),('a',(19,7)),('a',(20,7)),('a',(20,16))] [] 0 [0,1,2,6,11,12,13,14,15,17,18,19]
 
test15 = NFAe [0,1,2,3,4,5,6,7,8,9] ['a'] [('a',(0,2)),('a',(0,6)),('a',(1,6)),('a',(1,7)),('a',(2,8)),('a',(3,1)),('a',(3,2)),('a',(4,9)),('a',(5,4)),('a',(5,7)),('a',(6,1)),('a',(7,0)),('a',(9,8))] [] 0 [1,6]
 
test16 = NFAe [0,1,2,3,4] ['a'] [('a',(1,0)),('a',(2,2)),('a',(4,0)),('a',(4,4))] [] 0 [0,1,4]
 
test17 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19] ['a'] [('a',(1,8)),('a',(3,11)),('a',(3,14)),('a',(5,17)),('a',(5,19)),('a',(6,0)),('a',(6,13)),('a',(7,7)),('a',(8,8)),('a',(10,8)),('a',(10,13)),('a',(11,17)),('a',(13,14)),('a',(13,15)),('a',(16,1)),('a',(16,14)),('a',(17,9)),('a',(17,15))] [] 0 [1,2,4,6,7,10,11,12,13,14,18]
 
test18 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21] ['a'] [('a',(2,20)),('a',(3,8)),('a',(5,6)),('a',(5,12)),('a',(7,15)),('a',(8,2)),('a',(9,5)),('a',(9,15)),('a',(10,18)),('a',(11,12)),('a',(12,1)),('a',(13,16)),('a',(14,6)),('a',(15,12)),('a',(16,8)),('a',(17,16)),('a',(18,18)),('a',(18,19)),('a',(19,12)),('a',(19,18)),('a',(20,12)),('a',(20,16))] [] 0 [1,2,3,4,5,11,12,17,18,20,21]
 
test19 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23] ['a'] [('a',(0,18)),('a',(1,6)),('a',(1,18)),('a',(3,8)),('a',(4,17)),('a',(4,18)),('a',(5,5)),('a',(6,21)),('a',(6,22)),('a',(7,17)),('a',(7,23)),('a',(8,2)),('a',(10,19)),('a',(11,9)),('a',(11,11)),('a',(12,13)),('a',(13,22)),('a',(15,16)),('a',(16,13)),('a',(20,21))] [] 0 [1,5,7,8,9,13,16,17,22]
 
test20 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22] ['a'] [('a',(3,21)),('a',(4,9)),('a',(5,15)),('a',(6,17)),('a',(8,20)),('a',(8,22)),('a',(11,9)),('a',(11,19)),('a',(12,2)),('a',(12,12)),('a',(13,10)),('a',(14,7)),('a',(15,14)),('a',(15,16)),('a',(16,6)),('a',(17,1)),('a',(17,14)),('a',(18,13)),('a',(18,15)),('a',(20,3)),('a',(20,20)),('a',(21,6))] [] 0 [1,3,4,6,8,11,14,15,16,18,19,22]
 
test21 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20] ['a'] [('a',(1,18)),('a',(2,5)),('a',(4,14)),('a',(6,8)),('a',(6,18)),('a',(8,1)),('a',(9,3)),('a',(10,2)),('a',(10,20)),('a',(11,7)),('a',(12,3)),('a',(12,16)),('a',(13,6)),('a',(14,1)),('a',(14,15)),('a',(16,17)),('a',(16,20)),('a',(17,1)),('a',(17,3)),('a',(18,1)),('a',(19,19)),('a',(19,20)),('a',(20,9))] [] 0 [0,1,2,6,9,10,11,14,16,17,18,20]
 
test22 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11] ['a'] [('a',(1,7)),('a',(2,0)),('a',(6,5)),('a',(6,10)),('a',(7,1)),('a',(7,7)),('a',(10,2)),('a',(10,3))] [] 0 [1,4,6,7,8,9]
 
test23 = NFAe [0,1,2,3,4,5,6,7] ['a'] [('a',(1,5)),('a',(1,6)),('a',(2,5)),('a',(2,7)),('a',(5,5)),('a',(5,6)),('a',(6,4)),('a',(6,7)),('a',(7,6))] [] 0 [0,1,2,4,7]
 
test24 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24] ['a'] [('a',(0,21)),('a',(1,17)),('a',(3,16)),('a',(3,21)),('a',(4,11)),('a',(4,23)),('a',(5,11)),('a',(8,3)),('a',(9,15)),('a',(9,24)),('a',(13,6)),('a',(14,21)),('a',(14,22)),('a',(15,2)),('a',(15,4)),('a',(16,19)),('a',(16,20)),('a',(17,21)),('a',(17,23)),('a',(18,22)),('a',(18,24)),('a',(19,17)),('a',(20,10)),('a',(21,10)),('a',(22,7)),('a',(23,2))] [] 0 [2,3,4,5,7,8,10,12,15,16,17,20,24]
 
test25 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12] ['a'] [('a',(0,3)),('a',(1,11)),('a',(2,1)),('a',(3,0)),('a',(3,6)),('a',(4,7)),('a',(5,10)),('a',(6,8)),('a',(6,10)),('a',(7,5)),('a',(8,7)),('a',(9,7)),('a',(10,9)),('a',(10,10)),('a',(11,5)),('a',(11,7))] [] 0 [0,1,2,5,8,9,11]
 
test26 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22] ['a'] [('a',(1,4)),('a',(1,8)),('a',(2,10)),('a',(2,11)),('a',(6,4)),('a',(9,5)),('a',(9,21)),('a',(10,12)),('a',(10,18)),('a',(12,17)),('a',(12,21)),('a',(13,12)),('a',(13,19)),('a',(15,16)),('a',(17,17)),('a',(17,21)),('a',(18,16)),('a',(19,17)),('a',(20,10)),('a',(21,8)),('a',(22,7)),('a',(22,9))] [] 0 [0,4,7,12,14,15,16,17,18]
 
test27 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23] ['a'] [('a',(0,21)),('a',(3,20)),('a',(3,22)),('a',(5,6)),('a',(7,7)),('a',(7,9)),('a',(8,17)),('a',(8,18)),('a',(9,19)),('a',(9,23)),('a',(11,2)),('a',(11,6)),('a',(15,4)),('a',(17,0)),('a',(19,8)),('a',(19,23)),('a',(21,2)),('a',(21,23)),('a',(23,2)),('a',(23,9))] [] 0 [0,3,4,8,10,12,13,15,16,17,19,20,21,23]
 
test28 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18] ['a'] [('a',(2,16)),('a',(3,11)),('a',(9,14)),('a',(11,4)),('a',(11,8)),('a',(12,12)),('a',(12,17)),('a',(15,16)),('a',(15,17)),('a',(17,11)),('a',(18,1))] [] 0 [2,3,4,6,7,8,9,10,12]
 
test29 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24] ['a'] [('a',(0,23)),('a',(0,24)),('a',(1,2)),('a',(2,7)),('a',(2,9)),('a',(4,24)),('a',(5,7)),('a',(5,19)),('a',(9,2)),('a',(11,5)),('a',(12,11)),('a',(12,14)),('a',(13,20)),('a',(14,14)),('a',(14,15)),('a',(15,10)),('a',(16,2)),('a',(17,1)),('a',(17,9)),('a',(18,20)),('a',(18,24)),('a',(20,3)),('a',(21,21)),('a',(21,24)),('a',(22,19)),('a',(23,16)),('a',(23,21)),('a',(24,20)),('a',(24,24))] [] 0 [1,3,4,8,10,11,12,13,16,17,19,21,23,24]
 
test30 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23] ['a'] [('a',(0,16)),('a',(1,15)),('a',(3,10)),('a',(4,18)),('a',(6,13)),('a',(7,21)),('a',(8,5)),('a',(9,22)),('a',(9,23)),('a',(10,21)),('a',(10,23)),('a',(11,2)),('a',(14,23)),('a',(17,7)),('a',(17,22)),('a',(18,2)),('a',(18,5)),('a',(19,8)),('a',(19,10)),('a',(20,5)),('a',(20,11)),('a',(21,17)),('a',(23,15)),('a',(23,19))] [] 0 [0,4,9,10,11,12,13,16,17,18,21,23]
 
test31 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11] ['a'] [('a',(0,10)),('a',(3,3)),('a',(3,4)),('a',(7,6)),('a',(7,10)),('a',(9,1)),('a',(10,7)),('a',(10,11)),('a',(11,5)),('a',(11,7))] [] 0 [0,2,4,5,9,11]
 
test32 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26] ['a'] [('a',(0,26)),('a',(1,12)),('a',(2,7)),('a',(2,19)),('a',(5,15)),('a',(6,9)),('a',(7,21)),('a',(7,24)),('a',(8,13)),('a',(9,19)),('a',(9,23)),('a',(11,7)),('a',(16,22)),('a',(17,8)),('a',(20,23)),('a',(21,11)),('a',(21,17)),('a',(22,6)),('a',(23,20)),('a',(23,21)),('a',(26,19))] [] 0 [0,1,4,5,6,11,13,16,17,21,22,24,25]
 
test33 = NFAe [0,1,2,3,4] ['a'] [('a',(0,1)),('a',(0,2)),('a',(1,3)),('a',(2,1)),('a',(2,3)),('a',(3,1)),('a',(3,2)),('a',(4,0))] [] 0 [1,4]
 
test34 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21] ['a'] [('a',(1,8)),('a',(1,17)),('a',(2,8)),('a',(4,3)),('a',(4,21)),('a',(6,8)),('a',(7,12)),('a',(7,21)),('a',(9,5)),('a',(10,5)),('a',(11,13)),('a',(12,14)),('a',(13,12)),('a',(13,14)),('a',(14,20)),('a',(14,21)),('a',(16,2)),('a',(16,16)),('a',(18,16)),('a',(19,14)),('a',(19,17)),('a',(20,9)),('a',(21,3))] [] 0 [0,1,2,5,6,7,10,12,19]
 
test35 = NFAe [0,1,2,3,4,5,6,7,8,9] ['a'] [('a',(0,0)),('a',(1,9)),('a',(2,7)),('a',(5,0)),('a',(5,5)),('a',(6,5)),('a',(8,5))] [] 0 [2,6,7,9]
 
test36 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17] ['a'] [('a',(0,4)),('a',(1,12)),('a',(1,15)),('a',(2,8)),('a',(2,13)),('a',(3,2)),('a',(3,14)),('a',(5,4)),('a',(5,14)),('a',(6,3)),('a',(8,16)),('a',(11,4)),('a',(11,15)),('a',(13,7)),('a',(14,4)),('a',(15,11)),('a',(15,12)),('a',(17,12)),('a',(17,15))] [] 0 [0,1,3,4,9,11,13,17]
 
test37 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25] ['a'] [('a',(2,20)),('a',(2,24)),('a',(4,14)),('a',(6,16)),('a',(7,20)),('a',(8,15)),('a',(8,20)),('a',(9,18)),('a',(9,19)),('a',(11,4)),('a',(13,2)),('a',(13,21)),('a',(14,25)),('a',(15,3)),('a',(20,5)),('a',(20,14)),('a',(22,13)),('a',(22,19)),('a',(23,18)),('a',(23,25))] [] 0 [0,2,3,6,10,11,12,13,16,17,19,20]
 
test38 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23] ['a'] [('a',(0,20)),('a',(0,23)),('a',(2,6)),('a',(2,9)),('a',(3,2)),('a',(4,0)),('a',(4,9)),('a',(9,0)),('a',(10,9)),('a',(11,3)),('a',(11,6)),('a',(13,11)),('a',(13,17)),('a',(14,19)),('a',(14,20)),('a',(15,20)),('a',(16,7)),('a',(16,8)),('a',(17,1)),('a',(17,17)),('a',(18,15)),('a',(19,19)),('a',(20,21)),('a',(20,22)),('a',(21,5)),('a',(21,12)),('a',(22,22)),('a',(22,23)),('a',(23,10))] [] 0 [0,4,8,9,12,16,19,20,21,22]
 
test39 = NFAe [0,1,2,3,4,5,6] ['a'] [('a',(0,0)),('a',(5,1)),('a',(5,2))] [] 0 [0,1,2,5,6]
 
test40 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15] ['a'] [('a',(0,0)),('a',(0,14)),('a',(2,9)),('a',(2,11)),('a',(4,7)),('a',(5,7)),('a',(5,10)),('a',(6,13)),('a',(8,6)),('a',(10,4)),('a',(11,13)),('a',(12,2)),('a',(12,9)),('a',(13,0))] [] 0 [0,2,5,7,8,9,11,13,14]
 
test41 = NFAe [0,1,2,3,4,5] ['a'] [('a',(2,3)),('a',(2,4)),('a',(3,0)),('a',(3,1))] [] 0 [2,3,4,5]
 
test42 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25] ['a'] [('a',(0,21)),('a',(0,22)),('a',(1,6)),('a',(1,21)),('a',(3,19)),('a',(3,22)),('a',(4,21)),('a',(5,3)),('a',(5,18)),('a',(7,24)),('a',(8,3)),('a',(8,10)),('a',(9,17)),('a',(9,18)),('a',(10,11)),('a',(12,23)),('a',(12,24)),('a',(15,17)),('a',(16,10)),('a',(20,21)),('a',(21,2)),('a',(21,11)),('a',(22,16)),('a',(23,4)),('a',(24,10))] [] 0 [1,2,7,9,10,11,13,14,16,17,18,24]
 
test43 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21] ['a'] [('a',(2,1)),('a',(3,9)),('a',(6,9)),('a',(11,1)),('a',(11,13)),('a',(12,15)),('a',(13,8)),('a',(14,0)),('a',(15,1)),('a',(15,10)),('a',(16,19)),('a',(19,4)),('a',(19,20)),('a',(20,20)),('a',(20,21)),('a',(21,10))] [] 0 [0,1,2,3,10,11,13,15,16,19,20]
 
test44 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23] ['a'] [('a',(1,16)),('a',(1,23)),('a',(3,4)),('a',(8,12)),('a',(10,4)),('a',(10,23)),('a',(15,22)),('a',(15,23)),('a',(17,14)),('a',(17,15)),('a',(19,20)),('a',(19,23)),('a',(20,16)),('a',(20,17)),('a',(22,11)),('a',(22,20)),('a',(23,17))] [] 0 [0,1,2,4,10,15,16,19,20,21]
 
test45 = NFAe [0,1,2,3,4,5,6,7,8,9,10] ['a'] [('a',(0,6)),('a',(0,7)),('a',(3,8)),('a',(3,10)),('a',(4,0)),('a',(4,5)),('a',(5,9)),('a',(5,10)),('a',(6,3)),('a',(8,7)),('a',(8,8)),('a',(9,1)),('a',(9,9)),('a',(10,10))] [] 0 [2,3,4,8]
 
test46 = NFAe [0,1,2,3,4,5,6,7,8] ['a'] [('a',(0,7)),('a',(0,8)),('a',(1,4)),('a',(2,6)),('a',(5,4)),('a',(5,8)),('a',(6,8)),('a',(7,4)),('a',(7,6)),('a',(8,2)),('a',(8,8))] [] 0 [0,1,8]
 
test47 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18] ['a'] [('a',(0,1)),('a',(1,3)),('a',(1,9)),('a',(2,14)),('a',(2,18)),('a',(3,9)),('a',(3,13)),('a',(4,10)),('a',(5,16)),('a',(7,0)),('a',(8,3)),('a',(8,14)),('a',(10,8)),('a',(13,2)),('a',(13,4)),('a',(14,4)),('a',(14,12)),('a',(15,4)),('a',(15,13)),('a',(17,9)),('a',(18,4)),('a',(18,15))] [] 0 [0,1,3,4,7,9,10,13,14,17,18]
 
test48 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25] ['a'] [('a',(2,8)),('a',(2,10)),('a',(4,6)),('a',(5,15)),('a',(5,24)),('a',(6,2)),('a',(7,14)),('a',(8,10)),('a',(9,13)),('a',(9,22)),('a',(10,23)),('a',(10,24)),('a',(11,6)),('a',(12,22)),('a',(12,24)),('a',(13,20)),('a',(13,23)),('a',(14,12)),('a',(15,6)),('a',(16,11)),('a',(19,4)),('a',(19,15)),('a',(20,25)),('a',(21,4)),('a',(21,8)),('a',(22,6)),('a',(22,20)),('a',(23,16)),('a',(23,19)),('a',(25,20)),('a',(25,25))] [] 0 [3,4,6,8,10,11,12,16,20,23,25]
 
test49 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24] ['a'] [('a',(1,23)),('a',(1,24)),('a',(2,18)),('a',(2,23)),('a',(4,1)),('a',(7,0)),('a',(8,18)),('a',(11,11)),('a',(11,22)),('a',(12,2)),('a',(13,24)),('a',(16,6)),('a',(17,21)),('a',(17,24)),('a',(19,20)),('a',(20,17)),('a',(22,11)),('a',(23,0)),('a',(23,18)),('a',(24,13))] [] 0 [0,1,2,4,5,6,8,11,15,24]
 
test50 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13] ['a'] [('a',(1,12)),('a',(3,3)),('a',(4,4)),('a',(5,10)),('a',(6,7)),('a',(6,10)),('a',(7,2)),('a',(8,1)),('a',(8,3)),('a',(10,4)),('a',(11,13)),('a',(12,1)),('a',(12,4)),('a',(13,10))] [] 0 [0,1,5,7,9,10,11,13]
 
test51 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26] ['a'] [('a',(0,26)),('a',(2,9)),('a',(3,12)),('a',(3,18)),('a',(5,15)),('a',(5,19)),('a',(6,25)),('a',(6,26)),('a',(7,2)),('a',(7,26)),('a',(9,22)),('a',(9,26)),('a',(10,16)),('a',(12,4)),('a',(13,23)),('a',(14,12)),('a',(15,25)),('a',(15,26)),('a',(16,1)),('a',(16,8)),('a',(17,14)),('a',(18,26)),('a',(19,21)),('a',(19,26)),('a',(21,3)),('a',(22,18)),('a',(23,4)),('a',(26,7))] [] 0 [3,6,7,8,10,13,17,18,20,22,24]
 
test52 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17] ['a'] [('a',(0,5)),('a',(0,10)),('a',(1,8)),('a',(2,7)),('a',(2,11)),('a',(6,4)),('a',(6,14)),('a',(7,15)),('a',(8,6)),('a',(8,17)),('a',(9,7)),('a',(10,2)),('a',(11,1)),('a',(13,6)),('a',(13,8)),('a',(15,4)),('a',(16,3)),('a',(17,16)),('a',(17,17))] [] 0 [0,3,7,9,12,13,15,16]
 
test53 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19] ['a'] [('a',(0,10)),('a',(0,17)),('a',(2,10)),('a',(2,14)),('a',(3,13)),('a',(3,19)),('a',(4,15)),('a',(7,7)),('a',(9,1)),('a',(11,18)),('a',(12,13)),('a',(13,13)),('a',(13,15)),('a',(14,4)),('a',(15,10)),('a',(16,0)),('a',(16,3)),('a',(17,15)),('a',(17,17)),('a',(18,13)),('a',(19,15))] [] 0 [0,5,8,10,14,15,16,19]
 
test54 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24] ['a'] [('a',(4,21)),('a',(5,21)),('a',(5,24)),('a',(6,10)),('a',(7,4)),('a',(8,8)),('a',(9,7)),('a',(10,20)),('a',(10,24)),('a',(12,9)),('a',(12,17)),('a',(15,14)),('a',(16,16)),('a',(16,18)),('a',(18,14)),('a',(20,17)),('a',(20,20)),('a',(21,15)),('a',(21,18)),('a',(22,8)),('a',(24,14))] [] 0 [4,5,9,10,14,15,22]
 
test55 = NFAe [0,1,2,3,4,5,6,7,8,9] ['a'] [('a',(4,6)),('a',(4,7)),('a',(6,2)),('a',(7,3)),('a',(8,6)),('a',(9,3)),('a',(9,8))] [] 0 [0,2,6,7,8,9]
 
test56 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11] ['a'] [('a',(1,3)),('a',(2,1)),('a',(2,11)),('a',(3,9)),('a',(3,10)),('a',(4,2)),('a',(4,4)),('a',(5,0)),('a',(5,2)),('a',(7,6)),('a',(8,4)),('a',(8,11)),('a',(11,6)),('a',(11,11))] [] 0 [1,2,3,4,5,7,11]
 
test57 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12] ['a'] [('a',(0,0)),('a',(0,12)),('a',(1,12)),('a',(2,8)),('a',(2,10)),('a',(3,1)),('a',(4,10)),('a',(5,8)),('a',(6,8)),('a',(6,10)),('a',(7,6)),('a',(7,8)),('a',(10,7)),('a',(12,1))] [] 0 [3,4,7,8,9,10]
 
test58 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27] ['a'] [('a',(1,25)),('a',(2,17)),('a',(2,23)),('a',(3,1)),('a',(7,5)),('a',(9,4)),('a',(9,24)),('a',(10,8)),('a',(10,23)),('a',(11,20)),('a',(13,4)),('a',(13,22)),('a',(14,10)),('a',(18,22)),('a',(19,15)),('a',(19,19)),('a',(20,16)),('a',(24,25)),('a',(27,4))] [] 0 [0,3,5,6,11,14,16,17,19,20,23,24,26]
 
test59 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13] ['a'] [('a',(1,0)),('a',(1,13)),('a',(3,1)),('a',(3,11)),('a',(5,4)),('a',(5,9)),('a',(6,4)),('a',(6,11)),('a',(7,8)),('a',(9,11)),('a',(9,13)),('a',(10,7)),('a',(10,8)),('a',(12,12)),('a',(13,1)),('a',(13,8))] [] 0 [0,3,4,7]
 
test60 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21] ['a'] [('a',(2,17)),('a',(3,6)),('a',(3,13)),('a',(4,20)),('a',(7,20)),('a',(8,18)),('a',(8,19)),('a',(10,20)),('a',(10,21)),('a',(12,20)),('a',(12,21)),('a',(15,19)),('a',(16,21)),('a',(18,8)),('a',(19,7)),('a',(20,16)),('a',(20,21))] [] 0 [0,2,3,6,10,11,13,15,16,17,18,20,21]
 
test61 = NFAe [0,1,2,3,4,5,6] ['a'] [('a',(0,2)),('a',(0,6)),('a',(1,2)),('a',(2,3)),('a',(5,4))] [] 0 [0,3,5,6]
 
test62 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11] ['a'] [('a',(1,2)),('a',(2,1)),('a',(2,2)),('a',(3,2)),('a',(4,0)),('a',(7,0)),('a',(8,4)),('a',(9,1)),('a',(9,4)),('a',(11,10)),('a',(11,11))] [] 0 [0,10,11]
 
test63 = NFAe [0,1,2,3,4,5,6] ['a'] [('a',(0,0)),('a',(0,1)),('a',(1,0)),('a',(1,4)),('a',(3,5)),('a',(3,6)),('a',(4,1)),('a',(4,6))] [] 0 [0,2,3]
 
test64 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18] ['a'] [('a',(0,10)),('a',(0,13)),('a',(1,3)),('a',(3,9)),('a',(5,11)),('a',(5,14)),('a',(6,7)),('a',(6,10)),('a',(7,15)),('a',(7,18)),('a',(8,13)),('a',(8,14)),('a',(9,12)),('a',(9,14)),('a',(13,15)),('a',(14,16)),('a',(15,1)),('a',(16,4)),('a',(17,8)),('a',(17,9)),('a',(18,11)),('a',(18,17))] [] 0 [7,8,9,11,12,16,17,18]
 
test65 = NFAe [0,1,2,3,4,5,6,7,8] ['a'] [('a',(0,5)),('a',(1,6)),('a',(1,8)),('a',(2,1)),('a',(2,2)),('a',(3,0)),('a',(4,7)),('a',(4,8)),('a',(5,3)),('a',(5,6)),('a',(8,6)),('a',(8,7))] [] 0 [0,1,3,4,6,7,8]
 
test66 = NFAe [0,1,2,3,4,5,6,7,8] ['a'] [('a',(0,7)),('a',(0,8)),('a',(1,8)),('a',(2,4)),('a',(3,2)),('a',(3,3)),('a',(4,1)),('a',(4,4)),('a',(5,7)),('a',(5,8)),('a',(6,1)),('a',(7,5)),('a',(7,6))] [] 0 [0,1,2,3,4,6,7,8]
 
test67 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23] ['a'] [('a',(1,23)),('a',(2,0)),('a',(2,17)),('a',(6,23)),('a',(7,10)),('a',(7,19)),('a',(8,1)),('a',(8,11)),('a',(10,14)),('a',(10,17)),('a',(11,8)),('a',(11,18)),('a',(13,16)),('a',(13,17)),('a',(14,13)),('a',(14,23)),('a',(15,16)),('a',(15,17)),('a',(17,0)),('a',(17,6)),('a',(18,12)),('a',(18,15)),('a',(19,19)),('a',(20,11)),('a',(20,22)),('a',(22,23))] [] 0 [0,1,3,5,6,9,10,11,12,13,17,20]
 
test68 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24] ['a'] [('a',(2,11)),('a',(4,17)),('a',(4,19)),('a',(6,2)),('a',(8,4)),('a',(8,5)),('a',(10,0)),('a',(10,7)),('a',(12,0)),('a',(13,12)),('a',(14,13)),('a',(14,23)),('a',(15,20)),('a',(15,23)),('a',(17,5)),('a',(18,5)),('a',(18,21)),('a',(20,6)),('a',(20,16)),('a',(23,3)),('a',(24,24))] [] 0 [0,1,3,4,6,8,10,11,12,13,14,15,17,22,23,24]
 
test69 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25] ['a'] [('a',(0,22)),('a',(0,25)),('a',(2,11)),('a',(2,23)),('a',(3,24)),('a',(4,6)),('a',(4,22)),('a',(5,10)),('a',(5,23)),('a',(6,0)),('a',(6,25)),('a',(9,12)),('a',(11,6)),('a',(11,21)),('a',(12,13)),('a',(13,22)),('a',(16,11)),('a',(17,21)),('a',(18,9)),('a',(19,2)),('a',(20,7)),('a',(20,9)),('a',(22,6)),('a',(23,22)),('a',(24,16)),('a',(25,17))] [] 0 [0,1,2,4,5,6,11,14,18,21,22,23,25]
 
test70 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24] ['a'] [('a',(0,7)),('a',(1,6)),('a',(1,10)),('a',(2,11)),('a',(2,17)),('a',(3,9)),('a',(4,8)),('a',(5,8)),('a',(6,6)),('a',(7,8)),('a',(8,7)),('a',(13,22)),('a',(13,23)),('a',(14,22)),('a',(14,23)),('a',(15,20)),('a',(15,22)),('a',(16,23)),('a',(16,24)),('a',(17,5)),('a',(18,2)),('a',(18,24)),('a',(19,6)),('a',(21,23)),('a',(23,14))] [] 0 [3,4,6,7,12,13,14,15,23]
 
test71 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20] ['a'] [('a',(1,7)),('a',(1,20)),('a',(2,17)),('a',(2,18)),('a',(3,1)),('a',(4,17)),('a',(4,18)),('a',(5,18)),('a',(6,12)),('a',(7,3)),('a',(8,19)),('a',(11,18)),('a',(12,9)),('a',(12,17)),('a',(13,3)),('a',(13,18)),('a',(15,8)),('a',(16,10)),('a',(17,14)),('a',(17,15)),('a',(18,17)),('a',(18,19)),('a',(19,6))] [] 0 [0,1,3,5,6,8,9,10,13,14,15,17,18,19]
 
test72 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26] ['a'] [('a',(2,18)),('a',(2,19)),('a',(3,11)),('a',(4,15)),('a',(4,19)),('a',(5,15)),('a',(6,5)),('a',(6,22)),('a',(7,0)),('a',(8,21)),('a',(8,25)),('a',(9,6)),('a',(10,15)),('a',(10,23)),('a',(12,12)),('a',(14,1)),('a',(14,7)),('a',(15,25)),('a',(17,16)),('a',(18,7)),('a',(19,22)),('a',(19,26)),('a',(20,16)),('a',(21,13)),('a',(22,1)),('a',(22,6)),('a',(26,24))] [] 0 [1,3,4,6,9,10,13,14,16,18,20,21,23,24,25]
 
test73 = NFAe [0,1,2,3,4,5,6,7,8,9] ['a'] [('a',(0,1)),('a',(4,6)),('a',(4,9)),('a',(5,6)),('a',(5,9)),('a',(6,8)),('a',(7,8)),('a',(7,9))] [] 0 [0,1,2,4,7,8,9]
 
test74 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25] ['a'] [('a',(0,13)),('a',(2,11)),('a',(2,13)),('a',(3,12)),('a',(3,13)),('a',(5,20)),('a',(6,4)),('a',(6,23)),('a',(7,15)),('a',(8,17)),('a',(8,21)),('a',(9,14)),('a',(10,7)),('a',(10,12)),('a',(14,13)),('a',(15,21)),('a',(16,11)),('a',(16,18)),('a',(18,9)),('a',(19,4)),('a',(19,16)),('a',(20,9)),('a',(20,12)),('a',(21,0)),('a',(21,6)),('a',(22,16)),('a',(22,25)),('a',(23,16)),('a',(23,20)),('a',(24,2)),('a',(24,17)),('a',(25,18))] [] 0 [0,1,3,4,6,10,13,17,18,19,21,22,23,24]
 
test75 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19] ['a'] [('a',(0,2)),('a',(1,4)),('a',(2,18)),('a',(2,19)),('a',(3,5)),('a',(4,11)),('a',(4,18)),('a',(7,5)),('a',(7,18)),('a',(8,14)),('a',(10,5)),('a',(10,11)),('a',(11,2)),('a',(13,3)),('a',(13,17)),('a',(14,10)),('a',(15,10)),('a',(15,16)),('a',(17,10)),('a',(19,8))] [] 0 [3,4,7,8,12,13,14,15,16]
 
test76 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21] ['a'] [('a',(0,11)),('a',(1,11)),('a',(2,10)),('a',(2,15)),('a',(7,1)),('a',(9,7)),('a',(10,10)),('a',(10,18)),('a',(11,20)),('a',(12,12)),('a',(13,13)),('a',(17,7)),('a',(19,12)),('a',(19,18)),('a',(20,1)),('a',(20,11)),('a',(21,9)),('a',(21,19))] [] 0 [0,1,3,9,11,15,19,21]
 
test77 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25] ['a'] [('a',(0,5)),('a',(1,3)),('a',(2,19)),('a',(7,18)),('a',(8,9)),('a',(8,23)),('a',(10,11)),('a',(11,22)),('a',(12,2)),('a',(12,21)),('a',(13,16)),('a',(15,23)),('a',(16,2)),('a',(17,23)),('a',(18,14)),('a',(19,20)),('a',(19,24)),('a',(21,3)),('a',(21,15)),('a',(22,8)),('a',(22,24))] [] 0 [4,10,11,14,15,21,22]
 
test78 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12] ['a'] [('a',(0,12)),('a',(2,6)),('a',(2,10)),('a',(3,10)),('a',(3,12)),('a',(5,1)),('a',(7,9)),('a',(9,10)),('a',(9,12)),('a',(11,3))] [] 0 [1,2,3,6,8,9,10,11,12]
 
test79 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19] ['a'] [('a',(2,9)),('a',(2,11)),('a',(4,3)),('a',(4,5)),('a',(5,14)),('a',(5,15)),('a',(10,17)),('a',(11,10)),('a',(12,4)),('a',(12,12)),('a',(13,12)),('a',(13,16)),('a',(14,1)),('a',(15,10)),('a',(15,19)),('a',(16,11)),('a',(17,10)),('a',(17,12)),('a',(19,12))] [] 0 [4,8,9,11,12,15,19]
 
test80 = NFAe [0,1,2,3,4] ['a'] [('a',(2,3)),('a',(3,0)),('a',(3,2)),('a',(4,4))] [] 0 [2,3]
 
test81 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22] ['a'] [('a',(0,13)),('a',(0,14)),('a',(1,9)),('a',(1,15)),('a',(2,13)),('a',(2,22)),('a',(3,11)),('a',(4,5)),('a',(5,9)),('a',(7,6)),('a',(7,9)),('a',(8,1)),('a',(8,10)),('a',(9,21)),('a',(9,22)),('a',(10,0)),('a',(13,10)),('a',(13,11)),('a',(14,17)),('a',(14,19)),('a',(15,11)),('a',(15,20)),('a',(17,22)),('a',(18,6)),('a',(18,18)),('a',(19,19)),('a',(19,21)),('a',(22,18))] [] 0 [0,1,3,5,8,10,11,12,13,14,15,16,18,19,20,22]
 
test82 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16] ['a'] [('a',(0,14)),('a',(0,15)),('a',(1,1)),('a',(1,4)),('a',(2,8)),('a',(3,8)),('a',(3,16)),('a',(4,12)),('a',(4,13)),('a',(5,8)),('a',(7,4)),('a',(7,15)),('a',(8,2)),('a',(8,8)),('a',(9,3)),('a',(11,5)),('a',(13,13)),('a',(13,14)),('a',(14,2)),('a',(14,13)),('a',(15,14)),('a',(16,6))] [] 0 [4,5,6]
 
test83 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22] ['a'] [('a',(0,11)),('a',(5,5)),('a',(6,21)),('a',(6,22)),('a',(9,2)),('a',(10,4)),('a',(10,14)),('a',(12,4)),('a',(12,9)),('a',(13,22)),('a',(14,14)),('a',(17,3)),('a',(17,10)),('a',(18,16)),('a',(18,22)),('a',(20,13)),('a',(20,20))] [] 0 [0,1,3,6,7,9,10,12,13,16,17,18,21]
 
test84 = NFAe [0,1,2,3,4,5,6,7,8,9,10] ['a'] [('a',(1,5)),('a',(3,2)),('a',(3,10)),('a',(4,5)),('a',(4,9)),('a',(5,0)),('a',(5,2)),('a',(6,2)),('a',(6,7)),('a',(7,0)),('a',(8,3)),('a',(8,5)),('a',(9,1)),('a',(10,1))] [] 0 [0,1,3,4,6,7,10]
 
test85 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26] ['a'] [('a',(2,7)),('a',(3,4)),('a',(4,26)),('a',(5,3)),('a',(7,19)),('a',(8,21)),('a',(9,16)),('a',(9,23)),('a',(11,21)),('a',(12,17)),('a',(13,24)),('a',(13,26)),('a',(15,12)),('a',(15,23)),('a',(16,23)),('a',(17,23)),('a',(19,18)),('a',(19,19)),('a',(20,18)),('a',(21,19)),('a',(21,21)),('a',(22,22)),('a',(25,19))] [] 0 [1,2,6,9,11,12,16,17,18,19,20,22,23,24,25]
 
test86 = NFAe [0,1,2,3,4,5,6] ['a'] [('a',(0,1)),('a',(2,4)),('a',(3,1)),('a',(5,2)),('a',(6,1))] [] 0 [0,1,4]
 
test87 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14] ['a'] [('a',(1,14)),('a',(2,2)),('a',(5,2)),('a',(5,5)),('a',(6,5)),('a',(6,11)),('a',(8,12)),('a',(8,14)),('a',(9,0)),('a',(10,10)),('a',(10,11)),('a',(11,1)),('a',(11,12)),('a',(12,1)),('a',(12,14)),('a',(13,6))] [] 0 [0,2,6,10,13]
 
test88 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28] ['a'] [('a',(1,8)),('a',(1,17)),('a',(3,15)),('a',(4,17)),('a',(4,25)),('a',(6,24)),('a',(6,26)),('a',(7,17)),('a',(8,28)),('a',(12,13)),('a',(13,26)),('a',(15,7)),('a',(15,23)),('a',(16,18)),('a',(16,27)),('a',(17,18)),('a',(17,21)),('a',(18,10)),('a',(19,8)),('a',(21,0)),('a',(21,20)),('a',(22,7)),('a',(22,25)),('a',(24,16)),('a',(24,19)),('a',(26,4)),('a',(26,21)),('a',(27,22)),('a',(27,27)),('a',(28,17)),('a',(28,28))] [] 0 [2,6,7,13,16,19,20,21,23,25,27,28]
 
test89 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20] ['a'] [('a',(2,8)),('a',(3,6)),('a',(4,6)),('a',(5,7)),('a',(6,11)),('a',(8,1)),('a',(8,16)),('a',(9,16)),('a',(10,6)),('a',(10,10)),('a',(14,6)),('a',(14,15)),('a',(15,11)),('a',(15,12)),('a',(16,3)),('a',(17,18)),('a',(17,20)),('a',(18,7)),('a',(19,19)),('a',(20,3))] [] 0 [0,1,2,4,5,6,8,9,10,12,13,14,16,18,19]
 
test90 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12] ['a'] [('a',(0,12)),('a',(1,0)),('a',(2,10)),('a',(2,11)),('a',(3,10)),('a',(4,7)),('a',(4,12)),('a',(5,6)),('a',(5,11)),('a',(7,9)),('a',(7,12)),('a',(10,7)),('a',(10,9)),('a',(11,11))] [] 0 [1,6,8,10]
 
test91 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11] ['a'] [('a',(0,0)),('a',(1,5)),('a',(1,6)),('a',(3,6)),('a',(3,7)),('a',(4,3)),('a',(5,10)),('a',(7,1)),('a',(7,2)),('a',(8,3)),('a',(8,6)),('a',(10,2)),('a',(11,5)),('a',(11,6))] [] 0 [2,3,6,7,8,10,11]
 
test92 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15] ['a'] [('a',(0,6)),('a',(0,7)),('a',(1,4)),('a',(1,11)),('a',(3,1)),('a',(4,4)),('a',(5,13)),('a',(7,1)),('a',(7,13)),('a',(8,14)),('a',(8,15)),('a',(9,0)),('a',(9,9)),('a',(10,1)),('a',(10,6)),('a',(11,3)),('a',(11,7)),('a',(12,9)),('a',(13,7)),('a',(14,12)),('a',(14,14))] [] 0 [1,2,4,6,7,11,12,14,15]
 
test93 = NFAe [0,1,2,3,4,5,6,7,8,9,10] ['a'] [('a',(0,7)),('a',(0,9)),('a',(1,0)),('a',(5,9)),('a',(6,9)),('a',(6,10)),('a',(7,9)),('a',(8,0)),('a',(8,9)),('a',(10,1))] [] 0 [1,2,9,10]
 
test94 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11] ['a'] [('a',(0,7)),('a',(0,11)),('a',(1,10)),('a',(1,11)),('a',(3,3)),('a',(4,5)),('a',(7,2)),('a',(7,3)),('a',(8,8)),('a',(9,11))] [] 0 [0,4,5,6,8]
 
test95 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28] ['a'] [('a',(1,12)),('a',(4,1)),('a',(5,1)),('a',(5,24)),('a',(6,26)),('a',(6,28)),('a',(7,25)),('a',(7,27)),('a',(9,4)),('a',(9,12)),('a',(10,13)),('a',(11,7)),('a',(13,5)),('a',(15,3)),('a',(16,11)),('a',(22,18)),('a',(22,27)),('a',(23,5)),('a',(24,21)),('a',(25,7))] [] 0 [0,1,2,6,11,14,15,20,21,23,24]
 
test96 = NFAe [0,1,2,3,4] ['a'] [('a',(0,0)),('a',(1,1)),('a',(1,4)),('a',(3,3)),('a',(3,4)),('a',(4,3)),('a',(4,4))] [] 0 [1,2]
 
test97 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18] ['a'] [('a',(0,7)),('a',(0,15)),('a',(2,11)),('a',(2,18)),('a',(5,17)),('a',(5,18)),('a',(6,8)),('a',(6,10)),('a',(7,7)),('a',(9,1)),('a',(10,15)),('a',(10,16)),('a',(11,13)),('a',(11,18)),('a',(12,2)),('a',(12,6)),('a',(14,5)),('a',(14,17)),('a',(15,16)),('a',(15,17)),('a',(16,4)),('a',(16,18)),('a',(17,6)),('a',(17,12)),('a',(18,0)),('a',(18,10))] [] 0 [0,1,2,5,6,7,9,10,13,15,17]
 
test98 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27] ['a'] [('a',(0,12)),('a',(0,27)),('a',(1,8)),('a',(2,18)),('a',(4,0)),('a',(4,20)),('a',(5,10)),('a',(6,24)),('a',(6,27)),('a',(7,3)),('a',(10,8)),('a',(11,5)),('a',(11,24)),('a',(13,22)),('a',(13,27)),('a',(14,2)),('a',(15,3)),('a',(15,6)),('a',(16,25)),('a',(17,24)),('a',(17,26)),('a',(18,11)),('a',(18,25)),('a',(19,13)),('a',(19,21)),('a',(21,6)),('a',(22,16)),('a',(22,17)),('a',(23,3)),('a',(24,5)),('a',(25,5)),('a',(26,12)),('a',(27,10))] [] 0 [0,1,2,3,4,7,14,16,17,18,19,20,21,23,25,27]
 
test99 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16] ['a'] [('a',(2,0)),('a',(3,9)),('a',(6,16)),('a',(8,14)),('a',(10,1)),('a',(10,15)),('a',(12,2)),('a',(12,13)),('a',(13,12)),('a',(13,14)),('a',(14,6)),('a',(14,9)),('a',(15,13)),('a',(15,16)),('a',(16,3))] [] 0 [0,3,5,6,8,10,11,12,13,14,16]
 
test100 = NFAe [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26] ['a'] [('a',(0,15)),('a',(3,11)),('a',(3,26)),('a',(5,25)),('a',(6,19)),('a',(6,26)),('a',(7,6)),('a',(8,15)),('a',(9,24)),('a',(10,7)),('a',(10,21)),('a',(11,3)),('a',(12,24)),('a',(12,26)),('a',(13,2)),('a',(16,3)),('a',(18,3)),('a',(21,2)),('a',(23,18)),('a',(26,13))] [] 0 [2,3,5,6,7,9,10,12,13,14,15,19,21,22,24,26]
 
tests = [test1,test2,test3,test5,test6,test7,test8,test9,test10,test11,test12,test13,test14,test15,test16,test17,test18,test19,test20,test21,test22,test23,test24,test25,test26,test27,test28,test29,test30,test31,test32,test33,test34,test35,test36,test37,test38,test39,test40,test41,test42,test43,test44,test45,test46,test47,test48,test49,test50,test51,test52,test53,test54,test55,test56,test57,test58,test59,test60,test61,test62,test63,test64,test65,test66,test67,test68,test69,test70,test71,test72,test73,test74,test75,test76,test77,test78,test79,test80,test81,test82,test83,test84,test85,test86,test87,test88,test89,test90,test91,test92,test93,test94,test95,test96,test97,test98,test99,test100]