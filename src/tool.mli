open Graph

(* Clone a graph by keeping only its nodes *)
val clone_nodes: 'a graph -> 'b graph

(* Apply a function f to every label of the graph's arcs *)
val gmap: 'a graph -> ('a -> 'b) -> 'b graph

val add_capa: (int * int) graph -> id -> id -> int -> (int * int) graph

val get_max_id : 'a graph -> id