open Graph
open Tool
open BLF


val g_to_int: string graph -> int graph

val ford_fulk_algorithm : int graph -> id -> id -> (int * string graph)

(* for testing purpose *)

(* val rev_arcs: (id * id) list -> (id * id) list

val add_value_to_arcs: int graph -> (id * id) list -> int -> int graph

val get_final_graph: int graph -> int graph -> string graph *)