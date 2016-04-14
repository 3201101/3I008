module Holy_Tree : TREESTRUCTURE =
  struct
    type holy_tree = Node of (int * int) * holy_tree list
    let noeuds_possibles cant cont = 
      let l = ref [] in
      let harm = [0; 2; 4; 5; 7; 9; 11] in
      let mel = [0; 1; 2; 3; 4; 5; 7] in
	for i = 4 to 11 do
	  if (List.mem (abs(cant - i)) harm) && (List.mem (abs(cont - i)) mel)
	  then l := i::(!l)
	  else ()
	done;
	!l
    let init_tree = 
