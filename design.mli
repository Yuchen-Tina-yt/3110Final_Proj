(**This module represents the design of the dice, country, and frame separator*)

(**[get_dice_design number] is the game board with [number] on the die *)
val get_dice_design : int -> unit

(**[get_country_design num] is the ASCII art of the country corresponding to 
   num*)
val get_country_design : int -> unit

(**[frame_seperater] is the ASCII art of the frame separator*)
val frame_seperater : string