open Graph
open Tool
open BLF

(* Return a int graph from a string graph *)
val g_to_int: string graph -> int graph

(* Return a string graph after applying the ford-fulkerson algorithm on an int graph (capacity) *)
val ford_fulk_algorithm : int graph -> id -> id -> (int * string graph)
