(* Note: You may introduce new code anywhere in this file. *) 
open Yojson.Basic.Util

type money = int

(* Record that holds money in reserves, total money in circulation, 
   and total money in game. It will also hold the currency in USD. Items as a string
   Functions to allow players to withdraw or pay the bank. 
   One for money going into the bank and one for out. 
*)

(** [room] represents an exit *)
type bank = {
  reserve: money;
  circulation: money;
}

exception UnknownRoom of bank

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

