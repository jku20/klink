open Core

let test_planner planner =
  Planner.sexp_of_t planner |> Sexp.to_string_hum |> print_endline
;;

let test_unit_prop_planner planner =
  [%sexp_of: (Planner.t, string) Result.t] planner |> Sexp.to_string_hum |> print_endline
;;

let sa : Planner.state = { name = 1; param = [] }
let sb : Planner.state = { name = 2; param = [] }
let sc : Planner.state = { name = 3; param = [] }
let sd : Planner.state = { name = 4; param = [] }
let op1 : Planner.op = { name = "op1"; inputs = [ 1; 2 ]; outputs = [ 3 ] }
let op2 : Planner.op = { name = "op2"; inputs = [ 1; 3 ]; outputs = [ 4 ] }
let op3 : Planner.op = { name = "op3"; inputs = [ 2; 3 ]; outputs = [ 4 ] }

let%expect_test "solve" =
  let new_planner = Planner.make [ 1; 2 ] [ 4; 3 ] [ sa; sb; sc; sd ] [ op1; op2; op3 ] in
  Planner.solve_for_states new_planner;
  [%expect {|deff diff|}]
;;

let%expect_test "make" =
  let new_planner = Planner.make [ 1; 2 ] [ 4; 3 ] [ sa; sb; sc; sd ] [ op1; op2; op3 ] in
  test_planner new_planner;
  [%expect
    {|
    ((solution ((1 Present) (2 Present))) (clauses (((4)) ((3)))) (inputs (1 2))
     (states
      (((name 1) (param ())) ((name 2) (param ())) ((name 3) (param ()))
       ((name 4) (param ()))))
     (ops
      (((name op1) (inputs (1 2)) (outputs (3)))
       ((name op2) (inputs (1 3)) (outputs (4)))
       ((name op3) (inputs (2 3)) (outputs (4)))))
     (deps
      ((1 Primative) (2 Primative) (3 (Composite ((1 2))))
       (4 (Composite ((2 3) (1 3)))))))
    |}]
;;

let%expect_test "unit_prop simple remaining clauses" =
  let planner = Planner.make [] [ 3 ] [ sa; sb; sc; sd ] [ op1; op2; op3 ] in
  let propped = Planner.unit_prop planner in
  test_unit_prop_planner propped;
  [%expect
    {|
    (Ok
     ((solution ((3 Present))) (clauses (((1 2)))) (inputs ())
      (states
       (((name 1) (param ())) ((name 2) (param ())) ((name 3) (param ()))
        ((name 4) (param ()))))
      (ops
       (((name op1) (inputs (1 2)) (outputs (3)))
        ((name op2) (inputs (1 3)) (outputs (4)))
        ((name op3) (inputs (2 3)) (outputs (4)))))
      (deps
       ((1 Primative) (2 Primative) (3 (Composite ((1 2))))
        (4 (Composite ((2 3) (1 3))))))))
    |}]
;;

let%expect_test "unit_prop simple no remaining clauses" =
  let planner = Planner.make [ 1; 2 ] [ 3 ] [ sa; sb; sc; sd ] [ op1; op2; op3 ] in
  let propped = Planner.unit_prop planner in
  test_unit_prop_planner propped;
  [%expect
    {|
    (Ok
     ((solution ((1 Present) (2 Present) (3 Present))) (clauses ())
      (inputs (1 2))
      (states
       (((name 1) (param ())) ((name 2) (param ())) ((name 3) (param ()))
        ((name 4) (param ()))))
      (ops
       (((name op1) (inputs (1 2)) (outputs (3)))
        ((name op2) (inputs (1 3)) (outputs (4)))
        ((name op3) (inputs (2 3)) (outputs (4)))))
      (deps
       ((1 Primative) (2 Primative) (3 (Composite ((1 2))))
        (4 (Composite ((2 3) (1 3))))))))
    |}]
;;

let%expect_test "unit_prop idempotent" =
  let open Result in
  let planner = Planner.make [ 1; 2 ] [ 3 ] [ sa; sb; sc; sd ] [ op1; op2; op3 ] in
  let propped_once = Planner.unit_prop planner in
  let propped_twice = propped_once >>= Planner.unit_prop in
  test_unit_prop_planner propped_twice;
  [%expect
    {|
    (Ok
     ((solution ((1 Present) (2 Present) (3 Present))) (clauses ())
      (inputs (1 2))
      (states
       (((name 1) (param ())) ((name 2) (param ())) ((name 3) (param ()))
        ((name 4) (param ()))))
      (ops
       (((name op1) (inputs (1 2)) (outputs (3)))
        ((name op2) (inputs (1 3)) (outputs (4)))
        ((name op3) (inputs (2 3)) (outputs (4)))))
      (deps
       ((1 Primative) (2 Primative) (3 (Composite ((1 2))))
        (4 (Composite ((2 3) (1 3))))))))
    |}]
;;

let%expect_test "unit_prop can't make state" =
  let open Result in
  let planner = Planner.make [] [ 3 ] [ sa; sb; sc; sd ] [ op1; op2; op3 ] in
  let propped_once = Planner.unit_prop planner in
  let propped_twice = propped_once >>= Planner.unit_prop in
  test_unit_prop_planner propped_twice;
  [%expect {| (Error "Cannot make the following states: 1 2") |}]
;;
