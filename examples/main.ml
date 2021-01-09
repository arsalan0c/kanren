open Mk

let printf = Stdlib.Printf.printf

let rec fives_or_sixes x = disj ((===) x (Atom 5)) (fun sc -> Immature (fun () -> fives_or_sixes x sc))
let two_fives = take 2 (call_fresh (fun x -> fives x) empty_state)

let one_five x = (once ((===) (Atom 5) (Atom 5))) empty_state

let a_and_b = conj (call_fresh (fun x -> (===) x (Atom 7))) (call_fresh (fun x -> disj ((===) x (Atom 5)) ((===) x (Atom 6)))) empty_state

let fivesix_or_seven x y =  ifte ((===) x (Atom 5)) ((===) y (Atom 6))  ((===) x (Atom 7))


let () = begin
  (* let f a = Mk.eqv a (Atom 2) in
    let l = Mk.call_fresh f Mk.empty_state in *)
    (* let l = fives 5 empty_state in *)
    (* let s = Sequence.hd l in *)
      (* match s with
      | Some x -> printf "\n%s\n" (state_to_string x)
      | None -> printf "\n%s\n" "No state" *)
    let res = two_fives in
      printf "\n%s\n" (stream_to_string res)
      (* match res with 
      | Cons(a, _) -> printf "\n%s\n" (state_to_string a)
      | _ -> printf "\n%s\n" "other" *)

  (* let l = test5 in  *)
  (* let s = Sequence.hd l in *)
  (* let r = stream_to_string l in *)
  (* printf "\n%s\n" r  *)

end
