open Mk

let printf = Stdlib.Printf.printf

let rec fives x = disj ((===) x (Atom (Int 5))) (fun sc -> Immature (fun () -> fives x sc))
let three_fives = take 3 (call_fresh (fun x -> fives x) empty_state)

let one_five = (once ((===) (Atom (Int 6)) (Atom (Int 5)))) empty_state

let a_and_b = conj (call_fresh (fun x -> (===) x (Atom (Int 7)))) (call_fresh (fun x -> disj ((===) x (Atom (Int 5))) ((===) x (Atom (Int 6))))) empty_state

let fivesix_or_seven x y = ifte ((===) x (Atom (Int 5))) ((===) y (Atom (Int 6))) ((===) x (Atom (Int 7)))
let els x = ifte ((===) (Atom (Int 5)) (Atom (Int 6))) ((===) x (Atom (Int 6))) ((===) x (Atom (Int 7)))


(* if resulting stream has no states, the formula is unsatisfiable
  otherwise, return one result after another
*)
let p = call_fresh (fun x -> conj ((===) x (Atom (Int 1))) ((===) x (Atom (Int 0))))
let q = call_fresh (fun x -> disj ((===) x (Atom (Int 1))) ((===) x (Atom (Int 0))))
let r = call_fresh (fun x -> disj ((===) x (Atom (Int 1))) ((===) x (Atom (Int 0))))

let f1 = disj_plus [p; q]
let f2 = conj p q
let f3 = disj_plus [(call_fresh (fun x -> fives x)); q; r]

(* (===) (Atom (Int 1)) (Atom (Int 1)) *)
(* (conj_plus [(disj p q); r]) *)

let g = call_empty_state f1
let g2 = call_empty_state (call_fresh (fun x -> call_fresh (fun y -> fivesix_or_seven x y)))
let g3 = call_empty_state (call_fresh (fun x -> els x))

let g4 = call_empty_state (fresh3 (fun (x, y, z) -> disj_plus [((===) x (Atom (Str "first"))); ((===) y (Atom (Str "sec"))); ((===) z (Atom (Str "third")))]))

let at i lst = List.nth lst i 
let g5 = call_empty_state (freshN 3 (fun vars -> conj_plus [((===) (at 0 vars) (Atom (Int 1))); ((===) (at 1 vars) (Atom (Int 2))); ((===) (at 2 vars) (Atom (Int 3)))]))



let () = begin
  (* let f a = Mk.eqv a (Atom 2) in
    let l = Mk.call_fresh f Mk.empty_state in *)
    (* let l = fives 5 empty_state in *)
    (* let s = Sequence.hd l in *)
      (* match s with
      | Some x -> printf "\n%s\n" (state_to_string x)
      | None -> printf "\n%s\n" "No state" *)
   
    printf "\n%s\n" (stream_str g3)
      (* match res with 
      | Cons(a, _) -> printf "\n%s\n" (state_to_string a)
      | _ -> printf "\n%s\n" "other" *)

  (* let l = test5 in  *)
  (* let s = Sequence.hd l in *)
  (* let r = stream_to_string l in *)
  (* printf "\n%s\n" r  *)

end
