open Graph

val paiement: float graph -> string -> string list -> float -> (string * id) list -> (float graph * (string * id) list)

val init_node: float graph -> string -> id -> (string * id) list-> (float graph * (string * id) list)

val get_id: string -> (string * id) list -> id

val get_user: id -> (string * id) list -> string