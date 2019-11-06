module type Place = sig 

  type t 

  val change_ownership : t -> string -> t 

  val change_land_value: t-> float-> t 
end 