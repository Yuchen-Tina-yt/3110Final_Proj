(**This module represents items in the game*)
type t = {item_name: string; item_value: int}

(**constructor of items *)
val make_item :  string -> int ->t
