open Graph
open Tool


val paiement: float graph -> string -> string list -> float -> (string * id * float) list -> (float graph * (string * id * float) list)

val init_node: float graph -> string -> id -> (string * id * float) list-> (float graph * (string * id * float) list)

val get_id: string -> (string * id * float) list -> id

val get_user: id -> (string * id * float) list -> string

(* val set_val_du: string -> (string * id * float) list -> float -> string list -> (string * id * float) list *)

(* val set_val_pret: string -> float -> (string * id * float) list -> (string * id * float) list *)

val link_users : float graph -> (string * id * float) list -> float graph

val set_sink_origin : float graph -> (string * id * float) list -> float graph

val remove_ss_zeroes : string graph -> string graph