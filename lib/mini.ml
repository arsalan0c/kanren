(* additions from miniKanren and elsewhere *)

open Base
open Micro

let call_empty_state g = g empty_state

let disj_plus gs =
  match List.reduce gs ~f:disj with
  | Some g -> fun sc -> g sc
  | None -> fun _ -> mZero

let conj_plus gs =
  match List.reduce gs ~f:conj with
  | Some g -> fun sc -> g sc
  | None -> fun _ -> mZero

(* force evaluation of a stream *)
let rec pull s = match s with Immature f -> pull (f ()) | _ -> s

(* get up to the first n results of a stream *)
let rec take n s =
  if n <= 0 then mZero
  else match pull s with Cons (a, r) -> Cons (a, take (n - 1) r) | _ -> mZero

(* get all results of a stream *)
let rec take_all s =
  match pull s with Cons (a, r) -> Cons (a, take_all r) | _ -> mZero

let call_initial_state n g = take n (pull (call_empty_state g))

(* trivial reifier *)
let rec length s = match pull s with Cons (_, r) -> 1 + length r | _ -> 0

(* committed choice *)
let once g sc =
  let rec loop s =
    match s with
    | Nil -> mZero
    | Immature f -> Immature (fun () -> loop (f ()))
    | Cons (a, _) -> Cons (a, Nil)
  in
  loop (g sc)

(* soft-cut/if-then-else *)
let ifte g1 g2 g3 sc =
  let rec loop s g2 g3 =
    match s with
    | Nil -> g3 sc
    | Immature f -> Immature (fun () -> loop (f ()) g2 g3)
    | Cons (a, aa) -> bind (Cons (a, aa)) g2
  in
  loop (g1 sc) g2 g3

(* disjunction of conjunctions *)
let conde (gss : goal list list) sc = disj_plus (List.map gss ~f:conj_plus) sc

(* conjunction of disjunctions *)
let ednoc (gss : goal list list) sc = conj_plus (List.map gss ~f:disj_plus) sc

let fresh2 (f : term * term -> goal) sc =
  let counter = snd sc in
  f (Var counter, Var (counter + 1)) (fst sc, counter + 2)

let fresh3 (f : term * term * term -> goal) sc =
  let counter = snd sc in
  f (Var counter, Var (counter + 1), Var (counter + 2)) (fst sc, counter + 3)

let freshN n (f : term list -> goal) sc =
  let counter = snd sc in
  let vars = List.map (List.range counter (counter + n)) ~f:(fun i -> Var i) in
  f vars (fst sc, counter + n)
