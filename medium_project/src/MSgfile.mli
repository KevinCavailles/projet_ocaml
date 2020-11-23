open Graph

type path = string

val from_file: path -> (string graph * (string * id) list)

val write_file: path -> string graph -> (string * id) list-> unit

val export: path -> string graph -> unit

