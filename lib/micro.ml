(* μKanren implementation *)

open Base

exception Failure of string
let[@inline] failwith msg = raise (Failure ("Failure: " ^ msg))

type var = int
type var_counter = int
type 'a stream =
  | Nil
  | Immature of (unit -> 'a stream)
  | Cons of 'a * 'a stream
type atom = Int of int | Float of float | Bool of bool | Str of string
type term = Atom of atom | Var of var | Pair of var * term
type substitution = (int, term, Int.comparator_witness) Map.t
type state = substitution * var_counter
type goal = state -> state stream

let mZero = Nil
let empty_state = (Map.empty (module Int), 0)

let eqv (u : atom) (v : atom) =
  let atom_compare u v =
    match (u, v) with
    | Int x, Int y -> Int.compare x y
    | Float x, Float y -> Float.compare x y
    | Bool x, Bool y -> Bool.compare x y
    | Str x, Str y -> String.compare x y
    | _, _ -> -1
  in
  atom_compare u v = 0

let rec walk t s =
  match t with
  | Var v -> ( match Map.find s v with Some x -> walk x s | None -> t )
  | _ -> t

let rec occurs v t s =
  match walk t s with
  | Var x -> x = v
  | Pair (e1, e2) -> occurs v (Var e1) s || occurs v e2 s
  | _ -> false

let ext_s v t s =
  if occurs v t s then
    failwith ("circularity in substitution with variable:" ^ Int.to_string v)
  else Map.set s ~key:v ~data:t

let rec unify u v s =
  match (walk u s, walk v s) with
  | Var e1, Var e2 when e1 = e2 -> Some s
  | Var e, z -> Some (ext_s e z s)
  | z, Var e -> Some (ext_s e z s)
  | Pair (x1, y1), Pair (x2, y2) -> (
      match unify (Var x1) (Var x2) s with
      | Some s2 -> unify y1 y2 s2
      | None -> None )
  | Atom e1, Atom e2 when eqv e1 e2 -> Some s
  | _, _ -> None

(* Core μKanren goals *)

let ( === ) u v sc =
  match unify u v (fst sc) with
  | Some sub -> Immature (fun () -> Cons ((sub, snd sc), mZero))
  | None -> mZero

let call_fresh (f : term -> goal) sc = f (Var (snd sc)) (fst sc, snd sc + 1)

(* fair interleaving: order of the streams is switched in order to hand off control to the other stream *)
let rec mplus s1 s2 =
  match (s1, s2) with
  | Nil, _ -> s2
  | Immature f1, Immature f2 -> Immature (fun () -> mplus (f1 ()) (f2 ()))
  | Immature f, _ -> Immature (fun () -> mplus s2 (f ()))
  | Cons (a, r), _ -> Cons (a, mplus r s2)

let disj g1 g2 sc = mplus (g1 sc) (g2 sc)

let rec bind s g =
  match s with
  | Nil -> mZero
  | Immature f -> Immature (fun () -> bind (f ()) g)
  | Cons (a, r) -> mplus (g a) (bind r g)

let conj g1 g2 sc = bind (g1 sc) g2
