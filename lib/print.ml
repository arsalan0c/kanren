(* conversions to string *)

open Base
open Micro

let atom_print a =
  match a with
  | Int x -> Int.to_string x
  | Float x -> Float.to_string x
  | Bool x -> Bool.to_string x
  | Str x -> x

let rec term_print t = 
  match t with
  | Var(x) -> Int.to_string x
  | Pair(x, y) -> "(" ^ (Int.to_string x) ^ ", " ^ (term_print y) ^ ")"
  | Atom(x) -> atom_print x

let subst_print subst =
  "substitution: [" ^ (Map.fold subst ~init:"" ~f:(fun ~key:k ~data:v acc -> 
      let k_printing = Int.to_string k in
      let v_printing = term_print v in
      acc ^ "(" ^ k_printing ^ ", " ^ v_printing ^ ")" ^ ",  " 
  )) ^ "]"

let state_print (subst, counter) =
  "{ " ^ (subst_print subst) ^ ", counter: " ^ (Int.to_string counter) ^ " }"

let rec stream_print_h s = 
  match s with
  | Nil -> ""
  | Immature(f) -> stream_print_h (f())
  | Cons(a, r) -> state_print a ^ "\n" ^ stream_print_h r

let stream_print s = 
  "\nStates:\n" ^ (stream_print_h s) ^ "\n"
  