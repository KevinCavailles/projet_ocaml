open Graph

type path = id list

type t_cost={
    mutable cout:float;
    mutable father:int 
    }

val blf: float graph -> id -> id -> t_cost array

val get_path: float graph -> id -> id -> path option