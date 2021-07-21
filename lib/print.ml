(* conversions to string *)

open Base
open Micro

let rec atom_print = function
  | Int x -> Int.to_string x
  | Float x -> Float.to_string x
  | Bool x -> Bool.to_string x
  | Str x -> x
  | Lst x -> "[" ^ (String.concat ~sep:", " (List.map x ~f:atom_print)) ^ "]"

let rec term_print = function
  | Var x -> Int.to_string x
  | Pair (x, y) -> "(" ^ term_print x ^ ", " ^ term_print y ^ ")"
  | Atom x -> atom_print x

let subst_print subst =
  let f ~key:_ ~data:v acc = acc ^ (term_print v) ^ ", "
  in Map.fold subst ~init:"[" ~f ^ "]"

let state_print (subst, _) = subst_print subst

let rec stream_print_h = function
  | Nil -> ""
  | Immature f -> stream_print_h (f ())
  | Cons (a, r) -> state_print a ^ "\n" ^ stream_print_h r

let stream_print s = stream_print_h s
