(* import List modules *)
open List;;

(* define symbol for nonterminals and terminals *)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal;;

(* define symbol for None and Some *)
type 'a option = None | Some of 'a

(* convert_grammar takes in a grammar using syntax from HW1 and returns
a grammar using syntax from HW2. HW2 grammar uses a tuple with the first
term as the starting term and the second term as a function mapping each
nonterminal symbol to a list of rules *)
let convert_grammar gram1 =
  (fst gram1, function nt ->
    snd (split (filter (fun x -> fst x = nt) (snd gram1))));;

(* parse_prefix takes in a gramar and returns a matcher function for the
grammar. When applied to an acceptor and fragment, it either returns None
if the no grammar matches the fragment, or Some list of rules which matches
with some prefix of the fragment *)
let parse_prefix gram accept frag =
  let start = fst gram in
  let deriv = snd gram in
  let rec matcher accept frag derivation first beginning = match beginning with
    [] -> None
    | nextrule :: remainingrules ->
      let rec secondmatcher rule derivation frag = match rule with
        [] -> accept derivation frag
        | _ -> match frag with
          [] -> None
          | pre :: remaining -> match rule with
            [] -> None
            | N nont :: rest -> matcher (secondmatcher rest) frag derivation nont (deriv nont)
            | T t :: rest -> match pre with
              x when x = t -> (secondmatcher rest derivation remaining)
              | _ -> None in
      let newderivation = derivation @ [first, nextrule] in
      match (secondmatcher nextrule newderivation frag) with
        None -> matcher accept frag derivation first remainingrules
        | x -> x in
  matcher accept frag [] start (deriv start);;
