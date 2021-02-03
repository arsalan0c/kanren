open Mk.Micro
open Mk.Mini
open Mk.Print

let printf = Stdlib.Printf.printf

(* A recursive goal to provide an infinite number of 5's *)
(* To prevent a stack overflow, invocation of the recursive goal is delayed by using an immature stream *)
let rec fives x = disj ((===) x (Atom (Int 5))) (fun sc -> Immature (fun () -> fives x sc))

let three_fives = 
  (* obtain the conjunction of fives applied with three variables *)
  let thrice = fun (x,y,z) -> conj_plus [fives x; fives y; fives z] in
  (* form the goal by introducing 3 new logic variables *)
  let g = fresh3 thrice in
  (* obtain the result stream by calling the goal in the empty state *)
  let s = call_empty_state g in
  (* obtain 3 states from the stream *)
  let s3 = take 3 s in 
  printf "%s" (stream_print s3)

let one_five = 
  (* use the committed choice operator to obtain just the first result from the stream *)
  let s = once (call_fresh (fun x -> fives x)) empty_state in
  printf "%s" (stream_print s)

let fivesix_or_seven = 
    (* use the soft-cut operator to unify two variables with 5 and 6 or just the first with 7 *)
    let cond = fun (x, y) -> ifte ((===) x (Atom (Int 5))) ((===) y (Atom (Int 6))) ((===) x (Atom (Int 7))) in
    (* form the goal by introducing the 2 logic variables used in the goal *)
    let g = fresh2 cond in
    (* obtain the result stream by calling the goal in the empty state *)
    let s = call_empty_state g in
    printf "%s" (stream_print s)

(* Solve the formula: (P \/ !Q \/ R) && (!P \/ Q \/ S) && (Q \/ !S) && (R \/ S) && (P \/ !R) *)
let sat = 
  (* helper function to unify a variable with a boolean *)
  let boolean x b = ((===) x (Atom (Bool b))) in  
  (* map each variable to a choice of true or false boolean *) 
  let choices vars = List.map (fun x -> (disj (boolean x true) (boolean x false))) vars in 
  
  (* helper function to unify the ith variable in a list with a boolean *)
  let boolean_l i lst b = boolean (List.nth lst i) b in
  (* define the clauses of the formula *)
  let disjunctions vars = [
    disj_plus [boolean_l 0 vars true; boolean_l 1 vars false; boolean_l 2 vars true]; (* P \/ !Q \/ R*) 
    disj_plus [boolean_l 0 vars false; boolean_l 1 vars true; boolean_l 3 vars true]; (* !P \/ Q \/ S *)
    disj_plus [boolean_l 1 vars true; boolean_l 3 vars false]; (* Q \/ !S *)
    disj_plus [boolean_l 2 vars true; boolean_l 3 vars true]; (* R \/ S *)
    disj_plus [boolean_l 0 vars true; boolean_l 2 vars false]; (* P \/ !R *)
  ] in

  (* form the formula in CNF form by taking the conjunction of the disjunctions of the choices and disjunctions *)
  let formula vars = conj_plus ((choices vars)@(disjunctions vars)) in
  (* form the goal by introducing the 4 logic variables used in the goal *)
  let g = freshN 4 formula in
  (* obtain the result stream by calling the goal in the empty state *)
  let s = call_empty_state g in
  (* pretty print the stream *)
  printf "%s" (stream_print s)
