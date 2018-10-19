(* import List modules *)
open List;;

(* define symbol type, which is used in filter_reachable function *)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal;;

(* subset returns true if a is a subset of b and false otherwise *)
let subset a b =
  for_all (fun x -> exists (fun y -> y = x) b) a;;

(* equal_sets returns true if a and b are equal sets and false otherwise.
  duplicates count as a single instance *)
let equal_sets a b =
  subset a b && subset b a;;

(* set_intersection returns a intersection of sets a and b *)
let set_intersection a b = 
  filter (fun x -> exists (fun y -> y = x) b) a;;

(* set_diff returns a set of all elements present in a and not present in b *)
let set_diff a b = 
  filter (fun x -> not (exists (fun y -> y = x) b)) a;;

(* set_union returns the union of sets a and b without duplicates *)
let rec set_union a b = match b with
  x when subset x a -> sort_uniq compare a
| _ -> set_union ( hd (set_diff b a) :: a) b;;

(* computed_fixed_point begins at x and performs f(x), f(f(x), f(f(f(x), … recursively
  until it finds a computed fixed point where input = f(input) *)
let rec computed_fixed_point eq f x = 
  let output = f x in
  match x with
  a when eq a output -> a
| _ -> computed_fixed_point eq f (output);;

(* filter_reachable takes in a grammar, which is a pair of a starting point and a list of rules.
  the rules are pairs composed of a terminal symbol on the left side and a list of terminal and nonterminal symbols on the right.
  the starting point of the list of rules defines which rules to begin with.
  filter_reachable returns an identical grammar structure with all unreachable rules removed.
  a rule is unreachable if it’s left side symbol is not included in any rules stemming from the starting point *)
let filter_reachable g = 
  let start = fst g in
  let rules = snd g in
  let symbols = fst (split rules) in
  let final_set = computed_fixed_point
  (equal_sets)
  (fun x -> 
    let reachable_rules = filter (fun rule -> exists (fun z -> fst rule = z) x) rules in
    let new_symbols = filter (fun symbol -> exists (fun rule -> exists (
      fun z -> match z with 
       N w -> w = symbol 
     | T w -> false) 
       (snd rule)) reachable_rules) symbols in
    set_union new_symbols x)
  [start] in       
  let final_rules = filter (fun x -> exists (fun y -> y = fst x) final_set) rules in
start, final_rules;;
