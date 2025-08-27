open Core

let test_planner planner =
  Planner.sexp_of_t planner |> Sexp.to_string_hum |> print_endline
;;

let test_unit_prop_planner planner =
  [%sexp_of: (Planner.t, string) Result.t] planner |> Sexp.to_string_hum |> print_endline
;;

let sa : Planner.state = { name = "a"; param = [] }
let sb : Planner.state = { name = "b"; param = [] }
let sc : Planner.state = { name = "c"; param = [] }
let sd : Planner.state = { name = "d"; param = [] }
let op1 : Planner.op = { name = "op1"; inputs = [ "a"; "b" ]; outputs = [ "c" ] }
let op2 : Planner.op = { name = "op2"; inputs = [ "a"; "c" ]; outputs = [ "d" ] }
let op3 : Planner.op = { name = "op3"; inputs = [ "b"; "c" ]; outputs = [ "d" ] }

let%expect_test "make" =
  let new_planner =
    Planner.make [ "a"; "b" ] [ "d"; "c" ] [ sa; sb; sc; sd ] [ op1; op2; op3 ]
  in
  test_planner new_planner;
  [%expect
    {|
    ((solution ((a Present) (b Present))) (clauses (((d)) ((c)))) (inputs (a b))
     (states
      (((name a) (param ())) ((name b) (param ())) ((name c) (param ()))
       ((name d) (param ()))))
     (ops
      (((name op1) (inputs (a b)) (outputs (c)))
       ((name op2) (inputs (a c)) (outputs (d)))
       ((name op3) (inputs (b c)) (outputs (d)))))
     (deps
      ((a Primative) (b Primative) (c (Composite ((a b))))
       (d (Composite ((b c) (a c)))))))
    |}]
;;

let%expect_test "unit_prop simple remaining clauses" =
  let planner = Planner.make [] [ "c" ] [ sa; sb; sc; sd ] [ op1; op2; op3 ] in
  let propped = Planner.unit_prop planner in
  test_unit_prop_planner propped;
  [%expect
    {|
    (Ok
     ((solution ((c Present))) (clauses (((a b)))) (inputs ())
      (states
       (((name a) (param ())) ((name b) (param ())) ((name c) (param ()))
        ((name d) (param ()))))
      (ops
       (((name op1) (inputs (a b)) (outputs (c)))
        ((name op2) (inputs (a c)) (outputs (d)))
        ((name op3) (inputs (b c)) (outputs (d)))))
      (deps
       ((a Primative) (b Primative) (c (Composite ((a b))))
        (d (Composite ((b c) (a c))))))))
    |}]
;;

let%expect_test "unit_prop simple no remaining clauses" =
  let planner = Planner.make [ "a"; "b" ] [ "c" ] [ sa; sb; sc; sd ] [ op1; op2; op3 ] in
  let propped = Planner.unit_prop planner in
  test_unit_prop_planner propped;
  [%expect
    {|
    (Ok
     ((solution ((a Present) (b Present) (c Present))) (clauses ())
      (inputs (a b))
      (states
       (((name a) (param ())) ((name b) (param ())) ((name c) (param ()))
        ((name d) (param ()))))
      (ops
       (((name op1) (inputs (a b)) (outputs (c)))
        ((name op2) (inputs (a c)) (outputs (d)))
        ((name op3) (inputs (b c)) (outputs (d)))))
      (deps
       ((a Primative) (b Primative) (c (Composite ((a b))))
        (d (Composite ((b c) (a c))))))))
    |}]
;;

let%expect_test "unit_prop idempotent" =
  let open Result in
  let planner = Planner.make [ "a"; "b" ] [ "c" ] [ sa; sb; sc; sd ] [ op1; op2; op3 ] in
  let propped_once = Planner.unit_prop planner in
  let propped_twice = propped_once >>= Planner.unit_prop in
  test_unit_prop_planner propped_twice;
  [%expect {|
    (Ok
     ((solution ((a Present) (b Present) (c Present))) (clauses ())
      (inputs (a b))
      (states
       (((name a) (param ())) ((name b) (param ())) ((name c) (param ()))
        ((name d) (param ()))))
      (ops
       (((name op1) (inputs (a b)) (outputs (c)))
        ((name op2) (inputs (a c)) (outputs (d)))
        ((name op3) (inputs (b c)) (outputs (d)))))
      (deps
       ((a Primative) (b Primative) (c (Composite ((a b))))
        (d (Composite ((b c) (a c))))))))
    |}]
;;

let%expect_test "unit_prop can't make state" =
  let open Result in
  let planner = Planner.make [] [ "c" ] [ sa; sb; sc; sd ] [ op1; op2; op3 ] in
  let propped_once = Planner.unit_prop planner in
  let propped_twice = propped_once >>= Planner.unit_prop in
  test_unit_prop_planner propped_twice;
  [%expect {| (Error "Cannot make the following states: a b") |}]
;;
