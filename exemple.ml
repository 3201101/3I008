let aaa = 
[
	Noeud(0,0, 
	[
		Noeud(1,0, 
		[
			Noeud(3 ,0 ,[]); 
			Noeud(4 ,0 ,[]);
			Noeud(5 ,0 ,[])
		]); 
		Noeud(2 ,0 ,[]); 
	])
]

let a = clean_tree (aaa,3) in affiche a;; 
(* a vaut : 013 014 015 *)

let aaa = contruire_arbre [0;3;5] in affiche aaa;;

(* aaa vaut : 0,4,5,5 0,4,5,7 0,4,5,9 0,4,5,10 0,4,7,5 0,4,7,7 0,4,7,9 0,4,7,10 0,4,8,5 0,4,8,7 0,4,8,9 0,4,8,10 0,5,5,5 0,5,5,7 0,5,5,9 0,5,5,10 0,5,7,5 0,5,7,7 0,5,7,9 0,5,7,10 0,5,8,5 0,5,8,7 0,5,8,9 0,5,8,10 0,5,10,5 0,5,10,7 0,5,10,9 0,5,10,10 0,7,5,5 0,7,5,7 0,7,5,9 0,7,5,10 0,7,7,5 0,7,7,7 0,7,7,9 0,7,7,10 0,7,8,5 0,7,8,7 0,7,8,9 0,7,8,10 0,7,10,5 0,7,10,7 0,7,10,9 0,7,10,10 *)