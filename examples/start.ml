open Mkanren

let fives x = disj_plus [((===) x (Atom 13)); ((===) x (Atom 13))]
let test_fives = call_fresh (fun y -> fives y) empty_state

let test = ((===) (Atom 1) (Atom 2)) empty_state