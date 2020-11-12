open Core

exception Failure of string
let[@inline] failwith msg = raise (Failure ("Failure: " ^ msg))

type var_counter = int

type var = int

type term = Var of var | Pair of var * term | Atom of int
type 'a stream = Nil | Immature of (unit -> 'a stream) | Cons of 'a * ('a stream) 

type substitution = (var, term, Int.comparator_witness) Map.t
type state = substitution * var_counter
type goal = state -> state stream

let mZero = Nil
let empty_state = (Map.empty (module Int), 0)

let rec walk t (s: substitution) = match t with
    | Var v -> begin
        match Map.find s v with
        | Some x -> walk x s
        | None -> t
        end 
    | _ -> t

(* check if there are circuluarities in a substitution *)
let rec occurs v t s = 
    match walk t s with
    | Var(x) -> x = v
    | Pair(e1, e2) -> (occurs v (Var e1) s) || (occurs v e2 s)
    | _ -> false 

let ext_s (v, t) s = if occurs v t s then failwith ("circularity in substitution with variable:" ^ (Int.to_string v)) else Map.set s ~key:v ~data:t

let rec unify u v = 
    fun (s: substitution) -> 
        match walk u s, walk v s with
        | Var e1, Var e2 when e1 = e2 -> Some s
        | Var(e), z -> Some (ext_s (e, z) s)
        | z, Var(e) -> Some (ext_s (e, z) s)
        | Pair(x1, y1), Pair(x2, y2) -> begin 
            match unify (Var x1) (Var x2) s with
            | Some s2 -> unify y1 y2 s2
            | None -> None
            end
        | Atom e1, Atom e2 when e1 = e2 -> Some s
        | _, _ -> None

(* Goals *)
let (===) u v = 
    fun sc ->
        match unify u v (fst sc) with
        | Some sub -> Immature (fun () -> Cons((sub, snd sc), mZero))
        | None -> mZero

let call_fresh (f: term -> goal) =
    fun sc -> f (Var (snd sc)) (fst sc,(snd sc)+1)

(* let rec fresh = *)

let call_empty_state g = g empty_state 



(* Interleave results for complete search *)
let rec mplus s1 s2 = match s1 with
    | Nil -> s2
    | Immature(f) -> Immature(fun () -> mplus s2 (f()))
    | Cons(a, ss) -> Cons(a, mplus s2 ss)

let disj g1 g2 =
    fun sc -> mplus (g1 sc) (g2 sc)

let rec bind s g = match s with
    | Nil -> mZero
    | Immature(f) -> Immature(fun () -> bind (f()) g)
    | Cons(a, ss) -> mplus (g a) (bind ss g)

let conj g1 g2 =
    fun sc -> bind (g1 sc) g2   

let rec conj_plus gs = match List.hd gs, List.tl gs with
    | Some(g), Some(t) -> conj (fun sc -> Immature(fun () -> g sc)) (conj_plus t)
    | Some(g), _ -> fun sc -> Immature(fun () -> g sc)
    | _, _ -> fun _ -> mZero


let rec disj_plus gs = match List.hd gs, List.tl gs with
    | Some(g), Some(t) -> disj (fun sc -> Immature(fun () -> g sc)) (disj_plus t)
    | Some(g), _ -> fun sc -> Immature(fun () -> g sc)
    | _, _ -> fun _ -> mZero 

let state_to_string (sc: state) = match (Map.find (fst sc) 0) with
    | Some (Atom s) -> Int.to_string s
    | None -> "none"
    | _ -> "nil"

let rec pull s = match s with
    | Immature(f) -> pull(f())
    | _ -> s

let rec take n s = match n, s with
    | 0, _ -> mZero
    | _, s -> match pull s with
        | Cons(a, s) -> Cons(a, take (n - 1) s)
        | _ -> mZero

let rec take_all s = match pull s with
    | Cons(a, s) -> Cons(a, take_all s)
    | _ -> mZero

let call_initial_state n g =
    take n (pull (call_empty_state g))

let rec pretty_print s = match s with
    | Nil -> ""
    | Immature(f) -> pretty_print (f())
    | Cons(a, s) -> state_to_string a ^ "\n" ^ pretty_print s



let fives x = disj_plus [((===) x (Atom 13)); ((===) x (Atom 13))]
let test_fives = call_fresh (fun y -> fives y) empty_state
