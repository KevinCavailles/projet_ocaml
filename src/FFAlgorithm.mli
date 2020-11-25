open Graph
open Tool
open BLF


val g_to_float: string graph -> float graph

val ford_fulk_algorithm : float graph -> id -> id -> (float * string graph)

(* val only_one_edge: float graph -> float graph *)

(* for testing purpose *)

(* val rev_arcs: (id * id) list -> (id * id) list

val add_value_to_arcs: float graph -> (id * id) list -> float -> float graph

val get_final_graph: float graph -> float graph -> string graph *)