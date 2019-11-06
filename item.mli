(**This module represents items in the game*)
type t= {name:string; value:int}

(**constructor of items *)
val make_item :  string -> int ->t
