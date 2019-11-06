(** 
   Representation of static adventure data.

   This module represents the data stored in adventure files, including
   the rooms and exits.  It handles loading of that data from JSON as well
   as querying the data.
*)

(* You are free to add more code here. *)

(**********************************************************************
 * DO NOT CHANGE THIS CODE
 * It is part of the interface the course staff will use to test your 
 * submission.
*)

(** The abstract type of values representing adventures. *)
type money
type bank


exception UnknownPerfume of money



val deposit : money -> bank -> bank 


val withdraw : money -> bank -> bank 


(* END DO NOT CHANGE
 **********************************************************************)

(* You are free to add more code here. *)
