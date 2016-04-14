module ReadMidi : READSCORE = struct
	let lire_partition fic =
		let map_from_mid c =
			match c with
			| 62 -> 0
			| 64 -> 1
			| 65 -> 2
			| 67 -> 3
			| 69 -> 4
			| 71 -> 5
			| 72 -> 6
			| 74 -> 7
			| 76 -> 8
			| 77 -> 9
			| 79 -> 10
			| 81 -> 11
			| _  -> raise (AmbitusWrong c)
		in
		let loop tr l =
			let (_, (_, _, event)) = List.hd tr in
			match event with
			| NoteOn(_, 0) -> l
			| NoteOn(n, _) -> (map_from_mid n)::l
			| _ -> l
			if List.length tr = 1 then loop (List.tl tr) l else l
		in
		let (_, tracks) = MIDI.read c
		in
		in List.rev (loop tracks [])
	end
;;

module WriteMidi : WRITESCORE = struct
	let ecrire_partition cantus contre fic =
		let map_to_mid c =
			match c with
			| 0 -> 62
			| 1 -> 64
			| 2 -> 65
			| 3 -> 67
			| 4 -> 69
			| 5 -> 71
			| 6 -> 72
			| 7 -> 74
			| 8 -> 76
			| 9 -> 77
			| 10 -> 79
			| 11 -> 81
			| _  -> raise (ListeIncorrecte "map")
		in
		let loop l a b =
			(0, 0, NoteON((map_to_mid (List.hd b)), 0))::l
			(300, 0, NoteON((map_to_mid (List.hd a)), 0))::l
			(0, 0, NoteON((map_to_mid (List.hd b)), 80))::l
			(0, 0, NoteON((map_to_mid (List.hd a)), 80))::l
		in
		let tracks = List.fold_right2 loop [] cantus contre
		in
		MIDI.write (100, tracks)
	end
;;

module FCantusFirmus (R : READSCORE) (T : TREESTRUCTURE) (W : WRITESCORE) =
sig
    type t = {mutable cantus_firmus : int list; mutable tree : T.t} 
    let lire_partition = R.lecture
    let construire_arbre = T.construire_arbre
    let parcours_arbre = T.parcours_arbre
    let read_midi = W.lire_partition
    let write_midi = W.write_midi
end;;