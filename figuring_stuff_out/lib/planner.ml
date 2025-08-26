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

type t =
  { solution : decision Map.M(String).t
    (* A list of clauses, each clause is a set of states which are interchangeable *)
  ; clauses : state_id list list list
  ; inputs : state_id list
  ; states : state list
  ; ops : op list
  ; deps : state_id list list Map.M(String).t
  }
[@@deriving sexp]

let make inputs outputs states ops =
  let solution = Map.empty (module String) in
  let clauses = List.map ~f:(fun x -> [ x ]) outputs in
  let open List in
  let deps =
    fold
      ops
      ~init:(Map.empty (module String))
      ~f:(fun acc op ->
        fold op.outputs ~init:acc ~f:(fun acc s ->
          let new_deps =
            match Map.find acc s with
            | Some l -> op.inputs :: l
            | None -> [ op.inputs ]
          in
          Map.set ~key:s ~data:new_deps acc))
  in
  { solution; clauses; inputs; states; ops; deps }
;;

let unit_prop planner =
  let open List in
  let forced_clauses =
    []
    (*
       filter planner.clauses ~f:(fun l -> length l = 1)
            |> fold clauses ~init:[] ~f:(fun acc c ->
                match c with 
                | [] -> assert false 
                | [ [ s ] ] -> if mem.planner.inputs h ~equal:String.equal then h else match Map.find planner.deps h with None -> assert false | (Some d -> match d with [ l ] -> acc @ l | _ -> acc)
                | 
        )
    *)
  in
  let new_clauses =
    fold forced_states ~init:planner.clauses ~f:(fun acc s ->
      map acc ~f:(fun l -> filter l ~f:(Fun.negate (String.equal s))))
    |> filter ~f:(Fun.negate is_empty)
  in
  { planner with clauses = new_clauses }
;;
