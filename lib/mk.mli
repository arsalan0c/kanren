open Base

exception Failure of string

type var_counter = int
type var = int
type term = Var of var | Pair of var * term | Atom of int
type 'a stream = Nil | Immature of (unit -> 'a stream) | Cons of 'a * ('a stream)

type substitution = (var, term, Int.comparator_witness) Map.t
type state = substitution * var_counter
type goal = state -> state stream

(* 
(* Goals *)
val (===) : term -> term -> state -> state stream
val call_fresh : (term -> goal) -> state -> stream state
val call_empty_state : goal -> stream state
(* val call_initial_state : goal ->  *)
val disj : goal -> goal -> state -> stream state
val conj : goal -> goal -> state -> stream state

val disj_plus : List goal -> state -> stream state
val conj_plus : List goal -> state -> stream state

(* Functions for convenience *)

val pull : stream state -> stream state
val take : int -> stream state -> stream state
val take_all : stream state -> stream state

val once : stream state -> stream state
val ifte : goal -> goal -> goal -> state -> stream state

(* Functions for converting to string *)

val term_to_string : term -> string
val subst_to_string : subtitution -> string
val state_to_string : state -> string
val stream_to_string : stream 'a -> string 
*)

