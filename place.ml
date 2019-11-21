open Player

type t = {name:string; country: int ; value: float; ownerid: int; 
          num_houses: int; rent_payment: float; 
          pass_payment: float}

let make_place name country value rent_payment pass_payment =
  {name = name; country = country; value = value; ownerid = -1; 
   num_houses = 0; rent_payment = rent_payment;
   pass_payment=pass_payment}

let get_ownership t =
  t.ownerid

let change_ownership t playerid =
  {name= t.name; country =t.country; value=t.value; ownerid = playerid; 
   num_houses=t.num_houses; rent_payment=
                              t.rent_payment; pass_payment=t.pass_payment}

let change_land_value t new_val = 
  {name= t.name; country=t.country; value = new_val; ownerid =t.ownerid; 
   num_houses=t.num_houses; rent_payment = 
                              t.rent_payment; pass_payment=t.pass_payment}

let change_rent t new_val =
  {name= t.name; country=t.country; value = t.value; ownerid =t.ownerid; 
   num_houses=t.num_houses; rent_payment = 
                              new_val; pass_payment=t.pass_payment}

let get_rent t =
  t.rent_payment

let get_value t =
  t.value

let get_place_name t =
  t.name