open Graph

type path = id list

type t_cost={
    mutable cout:int;
    mutable father:int 
    }

val blf: int graph -> id -> id -> t_cost array

val get_path: int graph -> id -> id -> path option