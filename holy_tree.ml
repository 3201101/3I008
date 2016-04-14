module type TREESTRUCTURE =
  sig
    type t
    val init_tree : t
    val construire_arbre : int list -> t
    val parcours_arbre : t -> int list
  end

module type INPUT =
  sig
    val open_in : string -> in_channel
    val input_line : in_channel -> string
  end
      
module type READSCORE =
  sig
	  exception NonValidFile
    val lecture : string -> int list
  end

module Holy_Tree : TREESTRUCTURE =
  struct
    type t = Noeud of int * int * t list

    let noeuds_possibles cant cont = 
      let harm = [0; 2; 4; 5; 7; 9; 11] in
      let mel = [0; 1; 2; 3; 4; 5; 7] in
      let check x = (List.mem (abs(cant - x)) harm) 
	  && (List.mem (abs(cont - x)) mel) in
      List.map (fun x -> Noeud(x,cant,[])) 
	(List.filter check [4;5;6;7;8;9;10;11])


    let init_tree = Noeud(0,0,[])

    let construire_arbre l =
      let rec loop l (Noeud(cont, cant, nlist)) =
	match l with
	|[] -> Noeud(cont, cant, [])
	|h::t -> 
	    Noeud(cont, cant,(List.rev_map (loop t) (noeuds_possibles h cont)))
      in loop l init_tree

    let rec hauteur (Noeud(cant, cont, nlist)) =
      match nlist with
      |[] -> 0
      |_ -> List.fold_left (fun x y -> (max x ((hauteur y)+1))) 1 nlist
	    
    let netoyer_arbre n =
      let rec pross h (Noeud(cont, cant, nlist)) =
	Noeud(cont, cant,((List.filter (fun x -> h = (hauteur x)+1) nlist) 
      |> (List.map (pross (h-1))))) in
      pross (hauteur n) n

    let rec parcours_arbre (Noeud(_, _, nlist)) =
      match nlist with
      |[] -> []
      |_ -> let (Noeud(cont, _, _) as n)  = 
	  (List.length nlist) |> Random.int |> (List.nth nlist) in
	cont::(parcours_arbre n)
  end
    
module FCantusFirmus (R : READSCORE) (T : TREESTRUCTURE) =
  struct
    type t = {mutable cantus_firmus : int list; mutable tree : T.t} 
    let lire_partition = R.lecture
    let construire_arbre = T.construire_arbre
    let parcours_arbre = T.parcours_arbre
  end;;
