open Core

let test_planner planner =
  Planner.sexp_of_t planner |> Sexp.to_string_hum |> print_endline
;;

let sa : Planner.state = { name = "a"; param = [] }
let sb : Planner.state = { name = "b"; param = [] }
let sc : Planner.state = { name = "c"; param = [] }
let sd : Planner.state = { name = "d"; param = [] }
let op1 : Planner.op = { name = "op1"; inputs = [ "a"; "b" ]; outputs = [ "c" ] }
let op2 : Planner.op = { name = "op2"; inputs = [ "a"; "c" ]; outputs = [ "d" ] }
let op3 : Planner.op = { name = "op3"; inputs = [ "b"; "c" ]; outputs = [ "d" ] }
let planner_unit_prop = Planner.make [] ["c"] [ sa; sb; sc; sd ] [ op1; op2; op3 ]

let%expect_test "make" =
  let new_planner =
    Planner.make [ "a"; "b" ] [ "d"; "c" ] [ sa; sb; sc; sd ] [ op1; op2; op3 ]
  in
  test_planner new_planner;
  [%expect {| |}]
;;

let%expect_test "unit_prop" =
  let propped = Planner.unit_prop planner_unit_prop in
  test_planner propped;
  [%expect {| ((solution (a)) (clauses ((b))) (inputs ()) (states (a b)) (ops ())) |}]
;;
