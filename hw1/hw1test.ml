(* test cases for subset *)
let my_subset_test0 = subset [1;2;3] [4;5;6;1;3;2]
let my_subset_test1 = subset [] [4;5;6]
let my_subset_test2 = not (subset [4;5;6] [4;5])

(* test cases for equal_sets *)
let my_equal_sets_test0 = equal_sets [1;2;3] [1;2;3]
let my_equal_sets_test1 = equal_sets [1;3] [1;3;3]
let my_equal_sets_test2 = not (equal_sets [1;3;2] [1;3;3])

(* test cases for set_union *)
let my_set_union_test0 = equal_sets (set_union [1;2;3] [4;5]) [1;2;3;4;5]
let my_set_union_test1 = equal_sets (set_union [1;2;3] [2;3;4;5]) [1;2;3;4;5]
let my_set_union_test2 = not (equal_sets  (set_union [2;3] [2;3;4;5]) [1;2;3;4;5])
let my_set_union_test3 = equal_sets (set_union [] [4;5]) [4;5]

(* test cases for set_intersection *)
let my_set_intersection_test0 = equal_sets (set_intersection [1;2;3] [4;5]) []
let my_set_intersection_test1 = equal_sets (set_intersection [1;2;3] [1;3;2;2]) [1;2;3]
let my_set_intersection_test2 = not (equal_sets (set_intersection [1;2] [1;3;2;2]) [1;2;3])
let my_set_intersection_test3 = equal_sets (set_intersection [4;5;1;2;3] [1;3;6;2;7;8;2]) [1;2;3]

(* test cases for set_diff *)
let my_set_diff_test0 = equal_sets (set_diff [1;2;3] [4;5]) [1;2;3]
let my_set_diff_test1 = equal_sets (set_diff [2;3;6;4;5] [2;3;4;5]) [6]
let my_set_diff_test3 = equal_sets (set_diff [2;2;1;3;4] []) [1;2;3;4]
let my_set_diff_test4 = equal_sets (set_diff [5;2;2;1;3;4;6] [2;1;3;6;4;5;5]) []

(* test cases for computed_fixed_point *)
let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x *. 2.) 4. = infinity
let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> x /. 2.) 4. = 0.
let my_computed_fixed_point_test2 = computed_fixed_point (=) (fun x -> 5) 4 = 5
let my_computed_fixed_point_test3 = computed_fixed_point (fun x y -> x > 10) (fun x -> x * 2) 4 = 16

type day_activities = Eat | Sleep | Work | Play

let day_rules = 
[Play, [N Play; N Sleep; N Eat];
Sleep, [N Play];
Sleep, [N Sleep];
Sleep, [T "Get up"; N Eat; N Work; N Sleep];
Eat, [T "In-N-Out"];
Eat, [T "B plate"];
Work, [N Sleep];
Work, [T "CS131"]]

(* test cases for filter_reachable. Uses the day_activities and day_rules variables above *)
let my_filter_reachable_test0 = filter_reachable (Sleep, day_rules) = (Sleep, day_rules)
let my_filter_reachable_test1 = filter_reachable (Play, day_rules) = (Play, day_rules)
let my_filter_reachable_test2 = filter_reachable (Eat, day_rules) = (Eat, [Eat, [T "In-N-Out"]; Eat, [T "B plate"]])
let my_filter_reachable_test3 = filter_reachable (Sleep, (Sleep, [N Play]) :: tl (tl (day_rules))) = (Sleep, tl day_rules)
