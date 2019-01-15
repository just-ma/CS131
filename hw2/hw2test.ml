(* Test cases for parse_prefix. Uses two grammars using HW2 syntax *)

(* First test case *)
let accept_all derivation string = Some (derivation, string)

type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num

let awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
	 [[N Num];
	  [N Lvalue];
	  [N Incrop; N Lvalue];
	  [N Lvalue; N Incrop];
	  [T"("; N Expr; T")"]]
     | Lvalue ->
	 [[T"$"; N Expr]]
     | Incrop ->
	 [[T"++"];
	  [T"--"]]
     | Binop ->
	 [[T"+"];
	  [T"-"]]
     | Num ->
	 [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
	  [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])

let test_1 =
  ((parse_prefix awkish_grammar accept_all ["$"; "$"; "$"; "("; "0"; ")"; "+"; "++"; "$"; "$"; "9"; "8"; "("; "("; ")"; "+"])
  = Some
 ([(Expr, [N Term; N Binop; N Expr]); (Term, [N Lvalue]);
   (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue]);
   (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue]);
   (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
   (Term, [T "("; N Expr; T ")"]); (Expr, [N Term]); (Term, [N Num]);
   (Num, [T "0"]); (Binop, [T "+"]); (Expr, [N Term]);
   (Term, [N Incrop; N Lvalue]); (Incrop, [T "++"]);
   (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue]);
   (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Num]);
   (Num, [T "9"])],
  ["8"; "("; "("; ")"; "+"]))

(* Second test case *)
type awksub_nonterminals =
  | Speech | Pause | Phrase | Quote | Joke

let toast =
  (Speech,
   function
     | Speech ->
         [[N Joke; N Pause; N Phrase];
          [N Quote; N Phrase];
          [N Pause]]
     | Pause ->
	 [[N Phrase];
          [T "..."; N Pause];
	  [T "..."]]
     | Phrase ->
	 [[T "Hello all"];
          [T "Uh"; N Speech]]
     | Quote ->
	 [[T "To be or not to be"]]
     | Joke ->
	 [[T "Haha"];
	  [T "lol"]])

let test_2 =
  ((parse_prefix toast accept_all ["To be or not to be"; "Uh"; "Haha"; "..."; "..."; "..."; "Hello all"; "To be or not to be"; "Hello all"; "lol"; "..."; "..."; "Uh"])
  = Some
 ([(Speech, [N Quote; N Phrase]); (Quote, [T "To be or not to be"]);
   (Phrase, [T "Uh"; N Speech]); (Speech, [N Joke; N Pause; N Phrase]);
   (Joke, [T "Haha"]); (Pause, [T "..."; N Pause]);
   (Pause, [T "..."; N Pause]); (Pause, [T "..."]);
   (Phrase, [T "Hello all"])],
  ["To be or not to be"; "Hello all"; "lol"; "..."; "..."; "Uh"]))
