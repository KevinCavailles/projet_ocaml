open Graph

type t_cost={
    mutable cout:int;
    mutable father:int 
    }

val blf: int graph -> t_cost array

val get_path: t_cost array -> int -> int list