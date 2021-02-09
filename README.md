# µKanren in OCaml
  
[µKanren](http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf) is an embedded DSL for logic programming. It is part of family of such languages known as [*kanrens*](http://minikanren.org), with a focus on being minimalist and easy to understand. While this is at the expense of expressiveness and ease of use, the core system can be augmented. This implementation includes several such suggestions from the paper, including complete search and the `occurs` check to eliminate circularities in the substitution. It also includes other functionality such as `once` for commited choice and `ifte` as a [soft-cut](https://www.swi-prolog.org/pldoc/doc_for?object=(*-%3E)/2).

A note on the complete search mechanism:

Depth-first interleaving search is used. Control is handed off to another stream only in so far as it is necessary to achieve complete search. This is in contrast with breadth-first search which would perform the interleaving with every step.

## Examples

The examples can be run with 

```
dune exec examples/main.exe
```

### Turtles

A recursive goal to provide an infinite number of turtles:

```OCaml
let rec turtles x = disj (x === (Atom (Str "turtle"))) (fun sc -> Immature (fun () -> turtles x sc))
```
The recursive goal is wrapped in a nullary function and designated as an *immature* stream. The new goal is then formed as a unary function which takes in a state and returns the stream. Despite OCaml being call-by-value, this ensures the recursive goal is not evaluated when passed as an argument to `disj` (which would lead to stack overflow).

Here's an example obtaining three states, each with three turtles, from the infinite stream of turtles:

```OCaml
let three_turtles = 
  (* obtain the conjunction of turtles applied with three variables *)
  let thrice = fun (x,y,z) -> conj_plus [turtles x; turtles y; turtles z] in
  (* form the goal by introducing 3 new logic variables *)
  let g = fresh3 thrice in
  (* obtain the result stream by calling the goal in the empty state *)
  let s = call_empty_state g in
  (* obtain 3 states from the stream *)
  let s3 = take 3 s in 
  (* pretty print the stream *)
  printf "three_turtles\n%s" (stream_print s3)
```

This gives the following result where each line corresponds to a state which maps variables, represented by naturals, to their values:

```
States:
{ substitution: [(0, turtle),  (1, turtle),  (2, turtle),  ], counter: 3 }
{ substitution: [(0, turtle),  (1, turtle),  (2, turtle),  ], counter: 3 }
{ substitution: [(0, turtle),  (1, turtle),  (2, turtle),  ], counter: 3 }
```

### If-then-else (soft-cut)
`ifte` corresponds to an if-then-else construct. It takes in 3 goals and if the first succeeds (at least one resulting state), results of the conjunction of the first and second goals is returned. Otherwise, the result of the third goal is returned.

Unifying two variables with 5 and 6 or just the first with 7: 

```OCaml
let fivesix_or_seven = 
    let cond = fun (x, y) -> ifte (x === (Atom (Int 5))) (y === (Atom (Int 6))) (x === (Atom (Int 7))) in
    (* form the goal by introducing the 2 logic variables used in the goal *)
    let g = fresh2 cond in
    (* obtain the result stream by calling the goal in the empty state *)
    let s = call_empty_state g in
    (* pretty print the stream *)
    printf "%s" (stream_print s)
```

Since the first goal succeeds, the result is a single state with two variables having the values 5 and 6:

```
States:
{ substitution: [(0, 5),  (1, 6),  ], counter: 2 }
```

This can be useful in committing to a heuristic but only after testing to see whether it applies.

### A SAT problem

Solving the formula `(P \/ !Q \/ R) /\ (!P \/ Q \/ S) /\ (Q \/ !S) /\ (R \/ S) /\ (P \/ !R)`:

```OCaml
let sat = 
  (* helper function to unify a variable with a boolean *)
  let boolean x b = (x === (Atom (Bool b))) in  
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

  (* form the formula in CNF form by taking the conjunction of the disjunctions *)
  let formula vars = conj_plus ((choices vars)@(disjunctions vars)) in
  (* form the goal by introducing the 4 logic variables used in the goal *)
  let g = freshN 4 formula in
  (* obtain the result stream by calling the goal in the empty state *)
  let s = call_empty_state g in
  (* pretty print the stream *)
  printf "%s" (stream_print s)
```

## See Also
- *The Reasoned Schemer* by Daniel Friedman, William Byrd, Oleg Kiselyov and Jason Hemann
- [A Framework for Extending microKanren with constraints](https://arxiv.org/pdf/1701.00633.pdf)
- [cKanren](http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=49F95FE0FF32701C1FDA65333597DE1C?doi=10.1.1.231.3635&rep=rep1&type=pdf)
- [Temporal Logic, μKanren, and a Time-Traveling RDF Database](http://www.schemeworkshop.org/2018/Rudavsky-Brody.pdf)
- https://aphyr.com/posts/354-unifying-the-technical-interview
- [Differences between miniKanren and Prolog](http://minikanren.org/minikanren-and-prolog.html)
- [Backtracking, interleaving, and terminating monad transformers](https://dl.acm.org/doi/10.1145/1086365.1086390)
- [miniKanren: an interactive tutorial](http://io.livecode.ch/learn/webyrd/webmk)
- [A Unified Approach to Solving Seven Programming Problems](http://io.livecode.ch/learn/gregr/icfp2017-artifact-auas7pp)
