open Graph

val paiement: int graphe -> string -> string list -> float -> (string,id) list -> (int graphe, (string,id) list)

val init_node: int graphe -> string ->id -> (string,id) list-> (int graphe, (string,id) list)

val get_id: string -> (string, id) list -> id

val get_user: id -> (string, id) list -> string