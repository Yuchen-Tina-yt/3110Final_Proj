
type money = int

type bank = {
  reserve: money;
  circulation: money;
}

exception UnknownBank of bank

let make_bank reserve circulation = 
  {
    reserve = reserve;
    circulation = circulation;
  }

let deposit money bank = 
  {
    reserve = bank.reserve + money;
    circulation = bank.circulation - money;
  }  

let withdraw money bank = 
  {
    reserve = bank.reserve - money;
    circulation = bank.circulation + money;
  }  

