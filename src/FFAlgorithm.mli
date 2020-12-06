open Graph
open Tool
open BLF


val g_to_int: string graph -> (int * int) graph

val ford_fulk_algorithm : (int * int) graph -> id -> id -> ((int * int) * string graph)

(* val only_one_edge: (int * int) graph -> (int * int) graph *)

(* for testing purpose *)

(* val rev_arcs: (id * id) list -> (id * id) list

val add_value_to_arcs: (int * int) graph -> (id * id) list -> (int * int) -> (int * int) graph

val get_final_graph: (int * int) graph -> (int * int) graph -> string graph *)