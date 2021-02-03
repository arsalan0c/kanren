open Mk.Micro
open Mk.Mini
open Mk.Print

let printf = Stdlib.Printf.printf

let rec fives x = disj ((===) x (Atom (Int 5))) (fun sc -> Immature (fun () -> fives x sc))
let three_fives = take 3 (call_fresh (fun x -> fives x) empty_state)

let one_five = (once ((===) (Atom (Int 6)) (Atom (Int 5)))) empty_state
let fivesix_or_seven x y = ifte ((===) x (Atom (Int 5))) ((===) y (Atom (Int 6))) ((===) x (Atom (Int 7)))

(* 
  if resulting stream has no states, the formula is unsatisfiable
*)

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

  (* form the goal by introducing the 4 logic variables used in the formula *)
  let g = freshN 4 formula in

  (* obtain the result stream by calling the goal in the empty state *)
  let s = call_empty_state g in

  (* pretty print the stream *)
  printf "%s" (stream_print s)
