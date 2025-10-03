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

type t [@@deriving sexp]

(** Creates a planner from input states, desired output states, and a total list of states and ops *)
val make : state_id list -> state_id list -> state list -> op list -> t

(** Performs one instance of unit propagation on the planner. This adds to the solution any states with a single
      choice and removes those states from the clauses list.*)
val unit_prop : t -> (t, string) Result.t

val solve_for_states : t -> unit
