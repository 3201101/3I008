let cant = FCantusFirmus.lire_partition "part.csv";;
let abr = FCantusFirmus.construire_arbre cant;;
let cont = FCantusFirmus.parcours_arbre abr;;
List.iter print_int cont;;
