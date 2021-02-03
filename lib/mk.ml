open Base

exception Failure of string
let[@inline] failwith msg = raise (Failure ("Failure: " ^ msg))

type var = int
type var_counter = int
type 'a stream = Nil | Immature of (unit -> 'a stream) | Cons of 'a * ('a stream)
type atom = Int of int | Float of float | Bool of bool | Str of string
type term = Atom of atom | Var of var | Pair of var * term

type substitution = (int, term, Int.comparator_witness) Map.t 
type state = substitution * var_counter
type goal = state -> state stream

let mZero = Nil
let empty_state = (Map.empty (module Int), 0)

let eqv (u: atom) (v: atom) = 
    let atom_compare u v =
        match u, v with
        | Int x, Int y -> Int.compare x y
        | Float x, Float y -> Float.compare x y
        | Bool x, Bool y -> Bool.compare x y
        | Str x, Str y -> String.compare x y
        | _, _ -> -1
    in
    atom_compare u v = 0

let rec walk t s = match t with
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

let ext_s v t s = 
    if occurs v t s 
    then failwith ("circularity in substitution with variable:" ^ (Int.to_string v)) 
    else Map.set s ~key:v ~data:t

let rec unify u v s = 
    match walk u s, walk v s with
    | Var e1, Var e2 when e1 = e2 -> Some s
    | Var e, z -> Some (ext_s e z s)
    | z, Var e -> Some (ext_s e z s)
    | Pair(x1, y1), Pair(x2, y2) -> begin 
        match unify (Var x1) (Var x2) s with
        | Some s2 -> unify y1 y2 s2
        | None -> None
        end
    | Atom e1, Atom e2 when eqv e1 e2 -> Some s
    | _, _ -> None

    

(* Core μKanren goals *)

let (===) u v = 
    fun sc ->
        match unify u v (fst sc) with
        | Some sub -> Immature (fun () -> Cons((sub, snd sc), mZero))
        | None -> mZero

let call_fresh (f: term -> goal) =
    fun sc -> f (Var (snd sc)) (fst sc,(snd sc)+1)
        
let rec mplus s1 s2 = match s1, s2 with
    | Nil, _ -> s2
    | Immature f1, Immature f2 -> Immature(fun () -> mplus (f1()) (f2()))
    | Immature f, _ -> Immature(fun () -> mplus s2 (f())) 
    | Cons(a, r), _ -> Cons(a, mplus s2 r)

let disj g1 g2 =
    fun sc -> mplus (g1 sc) (g2 sc)

let rec bind s g = match s with
    | Nil -> mZero
    | Immature f -> Immature(fun () -> bind (f()) g)
    | Cons(a, r) -> mplus (g a) (bind r g)

let conj g1 g2 =
    fun sc -> bind (g1 sc) g2   



(* Additions *)

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
let rec pull s = match s with
    | Immature f -> pull(f())
    | _ -> s

let rec take n s =
    if n <= 0 then mZero else 
        begin
            match pull s with
            | Cons(a, r) -> Cons(a, take (n - 1) r)
            | _ -> mZero
        end

let rec take_all s = match pull s with
    | Cons(a, r) -> Cons(a, take_all r)
    | _ -> mZero

let call_initial_state n g =
    take n (pull (call_empty_state g))

(* trivial reifier *)
let rec length s = match pull s with
    | Cons(_, r) -> 1 + length r
    | _ -> 0

(* committed choice *)
let once g =
    fun sc -> 
        let rec loop s = begin 
            match s with 
            | Nil -> mZero
            | Immature(f) -> Immature(fun () -> loop (f()))
            | Cons(a, _) -> Cons(a, Nil)
        end in
        loop (g sc)

(* soft-cut *)
let ifte g1 g2 g3 =  
    fun sc -> 
        let rec loop s g2 g3 = begin 
            match s with
            | Nil -> g3 sc
            | Immature(f) -> Immature(fun () -> loop (f()) g2 g3)
            | Cons(a, aa) -> bind (Cons(a, aa)) g2
            end in
        loop (g1 sc) g2 g3

(* disjunction of conjunctions *)
let conde (gss: (goal list) list) = 
    fun sc -> 
        disj_plus (List.map gss ~f:conj_plus) sc

let fresh2 (f: term * term -> goal) =
    fun sc ->
        let counter = snd sc in
        f ((Var counter), (Var (counter+1)))
            (fst sc, counter+2) 
    
let fresh3 (f: term * term * term -> goal) =
    fun sc ->
        let counter = snd sc in
        f ((Var counter), (Var (counter+1)), (Var (counter+2)))
            (fst sc, counter+3) 

let freshN n (f: term list -> goal) =
        fun sc ->
            let counter = snd sc in
            let vars = List.map (List.range counter (counter + n)) ~f:(fun i -> Var i) in
            f vars (fst sc, counter+n) 

            
(* Functions for converting to string *)

let atom_str a =
    match a with
    | Int x -> Int.to_string x
    | Float x -> Float.to_string x
    | Bool x -> Bool.to_string x
    | Str x -> x

let rec term_str t = 
    match t with
    | Var(x) -> Int.to_string x
    | Pair(x, y) -> "(" ^ (Int.to_string x) ^ ", " ^ (term_str y) ^ ")"
    | Atom(x) -> atom_str x

let subst_str subst =
    "substitution: [" ^ (Map.fold subst ~init:"" ~f:(fun ~key:k ~data:v acc -> 
        let k_string = Int.to_string k in
        let v_string = term_str v in
        acc ^ "(" ^ k_string ^ ", " ^ v_string ^ ")" ^ ",  " 
    )) ^ "]"

let state_str (subst, counter) =
    "{ " ^ (subst_str subst) ^ ", counter: " ^ (Int.to_string counter) ^ " }"

let rec stream_str_h s = 
    match s with
    | Nil -> ""
    | Immature(f) -> stream_str_h (f())
    | Cons(a, r) -> state_str a ^ "\n" ^ stream_str_h r

let stream_str s = 
    "States:\n" ^ (stream_str_h s)
            