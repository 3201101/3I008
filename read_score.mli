module type INPUT =
	sig
		val open_in : string -> in_channel
		val input_line : in_channel -> string
	end
;;

module type READSCORE =
	sig
		exception NonValidFile
		val lecture : string -> int list
	end
;;

module type TREESTRUCTURE =
	sig
		type holy_tree
		val init_tree : holy_tree
		val construire_arbre : int list -> holy_tree
		val parcours_arbre : holy_tree -> int list
	end
;;
