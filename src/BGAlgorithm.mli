open Graph
open Tool
open BLF

(* Return a (int * int) graph from a (string * string) graph *)
val g_to_int: (string * string) graph -> (int * int) graph

(* Return a (string * string) graph after applying the busacker-gowen algorithm on a (int * int) graph (cost, capacity)*)
val busacker_gowen_algorithm : (int * int) graph -> id -> id -> (int * int * (string * string) graph)
