open Place 

module Place = struct 

  type t = {name:string; country: string; value: float; owner: Player option; 
            num_houses: int; rent_payment: int option; pass_payment: int option}

  let change_ownership t player =
    {name= t.name; country =t.country; value=t.value; owner=player; 
     num_houses=t.num_houses; rent_payment=
                                t.rent_payment; pass_payment=t.pass_payment}

  let change_land_value t new_val = 
    {name= t.name; country=t.country; value = new_val; owner =t.owner; 
     num_houses=t.num_houses; rent_payment = 
                                t.rent_payment; pass_payment=t.pass_payment}

end 