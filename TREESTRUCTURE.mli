module type TREESTRUCTURE =
  sig
    type t
    val init_tree : t
    val construire_arbre : int list -> t
    val parcours_arbre : t -> int list
  end;;
