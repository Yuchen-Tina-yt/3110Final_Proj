
(** Record that holds money in reserves, total money in circulation, 
    and total money in game. It will also hold the currency in USD. 
    Functions to allow players to withdraw or pay the bank. 
    One for money going into the bank and one for out. 
*)

(** [money] represents the money in USD *)

(** [t] represents a bank *)
type t

(** Raised when an unknown bank is encountered. *)
exception UnknownBank of t

(** [make_bank reserve circulation] creates a bank.*)
val make_bank : float -> float -> t

(** [deposit money bank] deposits the money into the bank.*)
val deposit : float -> t -> t

(** [withdraw money bank] withdraws the money into the bank.*)
val withdraw : float -> t -> t 



