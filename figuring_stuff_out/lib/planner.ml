open Core
module Sat = Msat_sat
module E = Sat.Int_lit
module F = Msat_tseitin.Make (E)

type state_id = int [@@deriving sexp]
type op_id = string [@@deriving sexp]
type param_id = string [@@deriving sexp]

type state =
  { name : state_id
  ; param : param_id list
  }
[@@deriving sexp]

type op =
  { name : op_id
  ; inputs : state_id list
  ; outputs : state_id list
  }
[@@deriving sexp]

type decision =
  | Present
  | Absent
  | Undecided
[@@deriving sexp]

type dependancy =
  | Composite of state_id list list
  | Primative
[@@deriving sexp]

type t =
  { solution : decision Map.M(Int).t
    (* A list of clauses, each clause is a set of states which are interchangeable *)
  ; clauses : state_id list list list
  ; inputs : state_id list
  ; outputs : state_id list
  ; states : state list
  ; ops : op list
  ; deps : dependancy Map.M(Int).t
  }
[@@deriving sexp]

let make inputs outputs (states : state list) ops =
  let solution =
    List.fold
      inputs
      ~init:(Map.empty (module Int))
      ~f:(fun acc s -> Map.set acc ~key:s ~data:Present)
  in
  let clauses = List.map ~f:(fun x -> [ [ x ] ]) outputs in
  let open List in
  let deps =
    fold
      (ops : op list)
      ~init:(Map.empty (module Int))
      ~f:(fun acc op ->
        fold op.outputs ~init:acc ~f:(fun acc s ->
          let new_deps : state_id list list =
            match Map.find acc s with
            | Some (Composite l) -> op.inputs :: l
            | _ -> [ op.inputs ]
          in
          Map.set ~key:s ~data:(Composite new_deps) acc))
  in
  let deps =
    fold states ~init:deps ~f:(fun acc s ->
      if Map.mem acc s.name then acc else Map.set acc ~key:s.name ~data:Primative)
  in
  { solution; clauses; inputs; outputs; states; ops; deps }
;;

let solve_for_states planner =
  let solver = Sat.create () in
  let a = E.fresh () in
  let make_list_atoms = List.map ~f:(fun x -> F.make_atom (E.make x)) in
  let from_deps =
    Map.fold planner.deps ~init:(F.make_atom a) ~f:(fun ~key ~data acc ->
      match data with
      | Primative -> acc
      | Composite possibilities ->
        let implication =
          List.map possibilities ~f:make_list_atoms |> List.map ~f:F.make_and |> F.make_or
        in
        let cond = F.make_atom (E.make key) in
        F.make_and [ acc; F.make_or [ F.make_not cond; implication ] ])
  in
  let outputs = make_list_atoms planner.outputs |> F.make_and in
  let expression = F.make_and [ outputs; from_deps ] in
  Format.printf "%a\n" F.pp expression;
  Sat.assume solver (F.make_cnf expression) ();
  let res = Sat.solve solver in 
  match res with
  | Msat_sat.Sat t ->
    let model = t.model () in
    printf "model len: %d\n" (List.length model);
    List.iter model ~f:(fun (t, v) ->
      Format.printf "%a: " Sat.Term.pp t;
      Format.printf "%a\n" Sat.Value.pp v)
  | Msat_sat.Unsat t ->
    let assumptions = t.unsat_assumptions () in
    printf "model len: %d" (List.length assumptions);
    assumptions |> List.iter ~f:(fun a -> Format.printf "%a\n" Sat.Atom.pp a)
;;

let unit_prop planner =
  let open List in
  (* get new states *)
  let new_states_from_only_option =
    fold planner.clauses ~init:[] ~f:(fun acc l ->
      match l with
      | [ l ] -> l @ acc
      | _ -> acc)
  in
  (* get new deps from those states *)
  let cur_chosen_states =
    Map.filter planner.solution ~f:(function
      | Present -> true
      | _ -> false)
    |> Map.keys
  in
  let new_deps =
    let open Result in
    let can_make_primitive s = mem cur_chosen_states s ~equal:( = ) in
    fold new_states_from_only_option ~init:(Ok []) ~f:(fun acc s ->
      match acc with
      | Error msg ->
        if can_make_primitive s then Error msg else Error (msg ^ " " ^ Int.to_string s)
      | Ok a ->
        (match Map.find_exn planner.deps s with
         | Composite c -> Ok (c :: a)
         | Primative ->
           if can_make_primitive s
           then Ok a
           else Error ("Cannot make the following states: " ^ Int.to_string s)))
  in
  Result.bind new_deps ~f:(fun new_deps ->
    (* add those deps to the clauses *)
    let cur_chosen_states =
      Map.filter planner.solution ~f:(function
        | Present -> true
        | _ -> false)
      |> Map.keys
    in
    (* prune the new clauses for states in the solution *)
    let new_solution = new_states_from_only_option @ cur_chosen_states in
    let new_clauses =
      new_deps @ planner.clauses
      |> map ~f:(fun l ->
        map l ~f:(fun t -> filter t ~f:(Fun.negate (mem new_solution ~equal:( = )))))
      |> filter ~f:(fun l -> for_all l ~f:(Fun.negate is_empty))
      |> filter ~f:(Fun.negate is_empty)
    in
    let new_solution =
      fold new_solution ~init:planner.solution ~f:(fun acc s ->
        Map.set acc ~key:s ~data:Present)
    in
    Ok { planner with clauses = new_clauses; solution = new_solution })
;;
