
type money = int

type bank = {
  reserve: money;
  circulation: money;
}

exception UnknownBank of bank

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

