open Base

exception Failure of string
let[@inline] failwith msg = raise (Failure ("Failure: " ^ msg))

(* 
    It seems type declarations have to be duplicated from the .mli by default which isn't ideal,
    although there are workarounds.
    See https://discuss.ocaml.org/t/why-do-i-need-to-repeat-type-declarations-between-interfaces-and-implementations-or-how-do-i-get-around-this/3350/7
*)

type var = int
type var_counter = int
type atom = Int of int | Float of float | Bool of bool | String of string 
type term = Var of var | Pair of var * term | Atom of atom
type 'a stream = Nil | Immature of (unit -> 'a stream) | Cons of 'a * ('a stream)

type substitution = (int, term, Int.comparator_witness) Map.t 
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

let rec occurs v t s = 
    match walk t s with
    | Var(x) -> x = v
    | Pair(e1, e2) -> (occurs v (Var e1) s) || (occurs v e2 s)
    | _ -> false 

let ext_s v t s = if occurs v t s 
    then failwith ("circularity in substitution with variable:" ^ (Int.to_string v)) 
    else Map.set s ~key:v ~data:t

let rec unify u v s = 
    match walk u s, walk v s with
    | Var(e1), Var(e2) when e1 = e2 -> Some s
    | Var(e), z -> Some (ext_s e z s)
    | z, Var(e) -> Some (ext_s e z s)
    | Pair(x1, y1), Pair(x2, y2) -> begin 
        match unify (Var x1) (Var x2) s with
        | Some s2 -> unify y1 y2 s2
        | None -> None
        end
    | _, _ -> None


(* Goals *)

let (===) u v = 
    fun sc ->
        match unify u v (fst sc) with
        | Some sub -> Immature (fun () -> Cons((sub, snd sc), mZero))
        | None -> mZero

let call_fresh (f: term -> goal) =
    fun sc -> f (Var (snd sc)) (fst sc,(snd sc)+1)

let call_empty_state g = g empty_state 

let rec mplus s1 s2 = match s1, s2 with
    | Nil, _ -> s2
    | Immature(f1), Immature(f2) -> Immature(fun () -> mplus (f1()) (f2()))
    | Immature(f), _ -> Immature(fun () -> mplus s2 (f())) 
    | Cons(a, ss), _ -> Cons(a, mplus s2 ss)

let disj g1 g2 =
    fun sc -> mplus (g1 sc) (g2 sc)

let rec bind s g = match s with
    | Nil -> mZero
    | Immature(f) -> Immature(fun () -> bind (f()) g)
    | Cons(a, ss) -> mplus (g a) (bind ss g)

let conj g1 g2 =
    fun sc -> bind (g1 sc) g2   

(* Not a must, but delay each goal so that if a user is only using disj_plus or conj_plus, they don't have to manually delay *)
let rec disj_plus gs = match List.hd gs, List.tl gs with
    | Some(g), Some(t) -> disj (fun sc -> Immature(fun () -> g sc)) (disj_plus t)
    | Some(g), _ -> fun sc -> Immature(fun () -> g sc)
    | _, _ -> fun _ -> mZero 

let rec conj_plus gs = match List.hd gs, List.tl gs with
    | Some(g), Some(t) -> conj (fun sc -> Immature(fun () -> g sc)) (conj_plus t)
    | Some(g), _ -> fun sc -> Immature(fun () -> g sc)
    | _, _ -> fun _ -> mZero

let conde gss = disj_plus (List.map gss ~f:conj_plus)


(* Functions for convenience *)

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

let once g =
    fun sc -> 
        let rec loop s = begin 
            match s with 
            | Nil -> mZero
            | Immature(f) -> Immature(fun () -> loop (f()))
            | Cons(a, _) -> Cons(a, Nil)
        end in
        loop (g sc)

let ifte g1 g2 g3 = 
    fun sc -> 
        let rec loop s g2 g3 = begin 
            match s with
            | Nil -> g3 sc
            | Immature(f) -> Immature(fun () -> loop (f()) g2 g3)
            | Cons(a, aa) -> bind (Cons(a, aa)) g2
            end in
        loop (g1 sc) g2 g3

let call_initial_state n g =
    take n (pull (call_empty_state g))


(* Functions for converting to string *)

let atom_str a =
    match a with
    | Int(x) -> Int.to_string x
    | Float(x) -> Float.to_string x
    | Bool(x) -> Bool.to_string x
    | String(x) -> x

let rec term_str t = 
    match t with
    | Var(x) -> Int.to_string x
    | Pair(x, y) -> "(" ^ (Int.to_string x) ^ ", " ^ (term_str y) ^ ")"
    | Atom(x) -> atom_str x

let subst_str subst =
    "[" ^ (Map.fold subst ~init:"" ~f:(fun ~key:k ~data:v acc -> 
        let k_string = Int.to_string k in
        let v_string = term_str v in
        acc ^ "(" ^ k_string ^ ", " ^ v_string ^ ")" ^ ",  " 
    )) ^ "]"

let state_str (subst, counter) =
    "{" ^ (subst_str subst) ^ ", " ^ (Int.to_string counter) ^ "}"

let rec stream_str s = match s with
    | Nil -> ""
    | Immature(f) -> stream_str (f())
    | Cons(a, s) -> state_str a ^ "\n" ^ stream_str s
