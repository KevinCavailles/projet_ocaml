open Graph
open Tool

(*Context : Maximum bipartite matching. We are trying to match as many element as possible from a first set S1 to a second set S2.
  S2 elements have a limited capacity (i.e they can only be matched by n elements, most often n < |S1|).
  We have to take into account S1 elements' preferences. Each one will represent a certain cost so that we can use our max-flow min-cost algorithm*)

(* Retrieve the id corresponding to a certain nodeName in the (string * id * int) list*)
val get_id: string -> (string * id * int) list -> id

(* Retrieve the nodeName corresponding to a certain id in the (string * id * int) list*)
val get_nodeName: id -> (string * id * int) list -> string

(* Creates n nodes in a (string * string) graph from id to id+n-1.
   Store the correspondance (nodeName, id, setNumber) in a list.
   The setNumber will be later used to identify to which set a node belongs*)
val set_lNodes : (string * string ) graph -> string list -> id -> (string * id * int) list -> int -> (int * (string * string ) graph * (string * id * int) list )

(* Creates an arc between two nodes with a certain weight (cost) and a capacity of 1 (weigth, 1) in a (string * string) graph *)
val set_preference : (string * string ) graph -> string -> string -> string -> (string * id * int) list -> (string * string ) graph

(* Creates a source with id=0 and a sink with id=n+1, with n the number of nodes in the graph.
   Then creates an arc between the source and every node with setNumber=1
   and an arc between the sink and every node with setNumber=2 with the specified capacities, either a single number or a set of capacity*)
val create_source_sink_and_link : (string * string ) graph -> (string * id * int) list -> string list -> (string * string ) graph

(* Create a new graph without : the source and its arcs, the sink and its arcs, every arc whose flow=0 *)
val remove_source_sink_zeroes : (string * string) graph -> (string * string) graph
