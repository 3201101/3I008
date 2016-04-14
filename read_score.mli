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
      
