
type t = {
  reserve: float;
  circulation: float;
}

exception UnknownBank of t 

let make_bank reserve circulation = 
  {
    reserve = reserve;
    circulation = circulation;
  }

let deposit money bank = 
  {
    reserve = bank.reserve +. money;
    circulation = bank.circulation -. money;
  }  

let withdraw money bank = 
  {
    reserve = bank.reserve -. money;
    circulation = bank.circulation +. money;
  }  

