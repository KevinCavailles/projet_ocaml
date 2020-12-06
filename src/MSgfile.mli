open Graph
open Printf
open MoneySharing

type path = string

val from_file: path -> (float graph * (string * id * float) list)

val write_file: path -> string graph -> (string * id * float) list -> unit

val export: path -> string graph -> unit

