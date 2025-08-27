open Core

type state_id = string [@@deriving sexp]
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
  { solution : decision Map.M(String).t
    (* A list of clauses, each clause is a set of states which are interchangeable *)
  ; clauses : state_id list list list
  ; inputs : state_id list
  ; states : state list
  ; ops : op list
  ; deps : dependancy Map.M(String).t
  }
[@@deriving sexp]

let make inputs outputs (states : state list) ops =
  let solution =
    List.fold
      inputs
      ~init:(Map.empty (module String))
      ~f:(fun acc s -> Map.set acc ~key:s ~data:Present)
  in
  let clauses = List.map ~f:(fun x -> [ [ x ] ]) outputs in
  let open List in
  let deps =
    fold
      ops
      ~init:(Map.empty (module String))
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
  { solution; clauses; inputs; states; ops; deps }
;;

let unit_prop planner =
  let open List in
  (* get new states *)
  let new_states_from_only_option =
    filter planner.clauses ~f:(fun l -> length l = 1)
    |> map ~f:(function
      | [ l ] -> l
      | _ -> assert false)
    |> fold ~init:[] ~f:(fun acc l -> l @ acc)
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
    let can_make_primitive s = mem cur_chosen_states s ~equal:String.equal in
    fold new_states_from_only_option ~init:(Ok []) ~f:(fun acc s ->
      match acc with
      | Error msg -> if can_make_primitive s then Error msg else Error (msg ^ " " ^ s)
      | Ok a ->
        (match Map.find_exn planner.deps s with
         | Composite c -> Ok (c :: a)
         | Primative ->
           if can_make_primitive s
           then Ok a
           else Error ("Cannot make the following states: " ^ s)))
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
        map l ~f:(fun t ->
          filter t ~f:(Fun.negate (mem new_solution ~equal:String.equal))))
      |> filter ~f:(fun l -> for_all l ~f:(Fun.negate is_empty))
      |> filter ~f:(Fun.negate is_empty)
    in
    let new_solution =
      fold new_solution ~init:planner.solution ~f:(fun acc s ->
        Map.set acc ~key:s ~data:Present)
    in
    Ok { planner with clauses = new_clauses; solution = new_solution })
;;
