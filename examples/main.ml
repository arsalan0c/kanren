open Mkanren.Mk
(* open Core *)

let printf = Stdlib.Printf.printf

let () = begin
  (* let f a = Mk.eqv a (Atom 2) in
    let l = Mk.call_fresh f Mk.empty_state in *)
    (* let l = fives 5 empty_state in *)
    (* let s = Sequence.hd l in *)
      (* match s with
      | Some x -> printf "\n%s\n" (state_to_string x)
      | None -> printf "\n%s\n" "No state" *)
    let res = test in
      printf "\n%s\n" (stream_to_string res)
      (* match res with 
      | Cons(a, _) -> printf "\n%s\n" (state_to_string a)
      | _ -> printf "\n%s\n" "other" *)

  (* let l = test5 in  *)
  (* let s = Sequence.hd l in *)
  (* let r = stream_to_string l in *)
  (* printf "\n%s\n" r  *)

end
