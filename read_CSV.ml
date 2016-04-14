module ReadCSV : READSCORE =
  struct
    exception NonValidFile
    let lecture fic =
      let f = open_in fic ic
	try
	  input_line f |> Str.split(Str.regexp ",") |> List.map int_of_string
	with
	| _ -> raise NonValidFile
  end;;
