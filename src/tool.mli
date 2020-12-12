open Graph

(* Clone a graph by keeping only its nodes *)
val clone_nodes: 'a graph -> 'b graph

(* Apply a function f to every label of the graph's arcs *)
val gmap: 'a graph -> ('a -> 'b) -> 'b graph

(* Add a value to the capacity of the arc id1 id2 in the graph*)
val add_arc: int graph -> id -> id -> int -> int graph