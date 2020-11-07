open Core

exception Failure of string
let[@inline] failwith msg = raise (Failure ("Failure: " ^ msg))

type var_counter = int

type var = int

type term = Var of var | Pair of var * term | Atom of int

type substitution = (var, term, Int.comparator_witness) Map.t
type state = substitution * var_counter
type goal = state -> (state) Sequence.t

type 'a stream = Nil | Immature of ('a stream) | Cons of 'a * ('a stream) 

let mZero = Sequence.empty
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
        | Var e, z -> Some (ext_s (e, z) s)
        | z, Var e -> Some (ext_s (e, z) s)
        | Pair(x1, y1), Pair(x2, y2) -> begin 
            match unify (Var x1) (Var x2) s with
            | Some s2 -> unify y1 y2 s2
            | None -> None
            end
        | Atom e1, Atom e2 when e1 = e2 -> Some s
        | _, _ -> None

(* Goals *)
let eqv u v = 
    fun sc ->
        match unify u v (fst sc) with
        | Some sub -> Sequence.singleton (sub, snd sc)
        | None -> mZero

let call_fresh (f: term -> goal) =
    fun sc -> f (Var (snd sc)) (fst sc,(snd sc)+1)

let call_empty_state g =
    g empty_state 

(* get all results that match a certain predicate *)
(* let take_all_pred p =  *)

(* Interleave results for complete search *)
let disj g1 g2 =
    fun sc -> let r1 = g1 sc in
        let r2 = g2 sc in
        Sequence.round_robin [r1; r2]

let conj g1 g2 =
    fun sc -> 
        let g1_states = g1 sc in 
        if Sequence.is_empty g1_states then mZero else
        Sequence.map g1_states ~f:(fun g1_state -> g2 g1_state)


(* inverse-n-delay *)
let zzz g = fun sc -> g sc
(* 
let disj_plus goals = fun sc -> Sequence.round_robin (List.map ~f:(fun g -> g sc) goals)
let conj_plus goals = fun sc ->  match Sequence.reduce goals ~f:conj with
    | Some combined_goal -> combined_goal s
    | None -> mZero  *)

(* let reify = fun sc ->  *)


let state_to_string sc = Int.to_string (snd sc) 

(* let f a = Mk.call_fresh (fun x -> eqv x (Mk.Atom "5")) in *)
(* let rec fives x = disj (eqv x (Atom 5)) (fun sc -> fives x sc) *)
let rec fives x = disj (eqv x (Atom 5)) (fun sc -> fun () -> fives x sc)
let test_fives = call_fresh (fun x -> fives x)




(* 
    list comprehensions with monads

*)