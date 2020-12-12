open Graph

type path = id list

type t_cost={
    mutable cout:int;
    mutable father:int 
    }

(* Execute the Bellman-Ford algorithm on a graph from the node with the specified id *)
val blf: int graph -> id -> t_cost array

(* Return a path option from a source node to a destination node *)
val get_path: int graph -> id -> id -> path option