open Graph
open Printf
open Bp
open Str 

type path = string

(* Read a (string * string) graph (cost,capacity) from a path.
   Return the graph and a (string * id * int) list  (name, id, setNumber) *)
val from_file: path -> ((string * string) graph * (string * id * int) list)

(* Write the matching results in the file "path" *)
val write_file: path -> 'a graph -> (string * id * int) list -> unit

(* Export a (string * string) graph (cout,flow) in the file "path".dot *)
val export: path -> (string * string) graph -> unit

