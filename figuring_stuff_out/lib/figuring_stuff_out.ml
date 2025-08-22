open Core

type node_id = string
type op_id = string
type param_id = string

type node =
  { name : node_id
  ; param : param_id list
  }

type op =
  { name : op_id
  ; inputs : node_id list
  ; outputs : node_id list
  }

type path_error = string
type path = op list

(** if [negated], pred is satisfied if none of the nodes are present, else if it is satisfied if any node is present *)
type term =
  { negated : bool
  ; nodes : node_id list
  }

type incompatibility = term list

type assignment_term =
  | Decision of node_id
  | Derivation of node_id list

type assignment = int * assignment_term

let find_path (input : node) (outputs : node list) (ops : op list) : path =
  let find_deps (n : node_id) : node_id list list =
    let open List in
    filter ~f:(fun l -> mem l.outputs n ~equal:String.equal) ops
    |> map ~f:(fun l -> l.inputs)
  in
  let incompatibility_satisfied (i : incompatibility) (solution : assignment list) =
    let term_unsatisfied { negated; nodes } =
      let open List in
      let equal = String.equal in
      let pred_not_sat =
        for_all solution ~f:(fun (_, d) ->
          match d with
          | Decision n -> not (mem nodes n ~equal)
          | Derivation ns -> not (for_all ns ~f:(fun n -> mem nodes n ~equal)))
      in
      if negated then pred_not_sat else not pred_not_sat
    in
    let unsat = List.filter i ~f:term_unsatisfied in
    match unsat with
    | [] -> `Satisfied
    | [ t ] -> `Almost_satisfied t
    | _ -> `Unsatisfied
  in
  let rec unit_propagation
            (changed : node_id list)
            (incomps : incompatibility list)
            (solution : assignment list)
    : incompatibility list * assignment list
    =
    match changed with
    | [] -> incomps, solution
    | h :: t -> 
        let loop = 
        match incompatibility_satisfied h solution with 
        | `Unsatisfied -> unit_propagation t incomps solution
        | `Almost_satisfied t -> assert false
        | `Satisfied -> assert false
  in
  let rec loop
            (cur : node_id)
            (incomps : incompatibility list)
            (solution : assignment list)
    =
    []
  in
  let sol = loop input.name [ [ { negated = true; nodes = [ input.name ] } ] ] [] in
  []
;;
