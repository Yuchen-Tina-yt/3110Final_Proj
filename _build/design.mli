(**This module represents the design of the dice,country, and frame_seperater*)

(**[get_dice_design number] is teh function that takes in the [num] and 
   returns the game board with that number on the board*)
val get_dice_design : int -> unit

(**[get_country_design num] is teh function that takes in the [num] that 
   represents the country and prints the ASCII art of that country*)
val get_country_design : int -> unit

(**[frame_seperater] is teh function that takes in nothing and prints 
   the ASCII art of that country*)
val frame_seperater : string