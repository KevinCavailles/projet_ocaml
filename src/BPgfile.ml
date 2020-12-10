open Graph
open Printf
open Bp
open Str

type path = string

(* Format of text files:
   % 

   % 
   s a,b,c,d,e,f
   s j1,j2,j3,j4,j5,j6	

   %
   p a -> j2 : pref n°1
   p a -> j3 : pref n°1
   p c -> j1 : pref n°1
   p c -> j4 : pref n°1
   p d -> j3 : pref n°1
   p e -> j3 : pref n°1
   p e -> j4 : pref n°1
   p f -> j6 : pref n°1

*)


let write_file (path : path) (graph: 'a graph) (lId : (string * id * int) list) =

  (* Open a write-file. *)
  let ff = open_out path in
 
  fprintf ff "%% Matching results :\n\n" ;

  (* Write all arcs *)
  e_iter graph (fun id1 id2 lbl -> fprintf ff " %s -> %s \n"(get_nodeName id1 lId) (get_nodeName id2 lId)) ;

  close_out ff ;
  ()


let read_comment graph line =
  try Scanf.sscanf line " %%" graph
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line ;
    failwith "from_file"

(* Reads a line with a user. *)
let read_set graph id lId setNumber line =
  (* let regexSplit = Str.regexp "(\\[|,|\\])" in *)
  try Scanf.sscanf line "s %s" (fun set -> set_lNodes graph (String.split_on_char ',' set) id lId setNumber )
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* Reads a line with a payement. *)
let read_preference graph lId line =
  try Scanf.sscanf line "p %s -> %s : pref n°%s"
        (fun nodeSet1 nodeSet2 weight -> set_preference graph nodeSet1 nodeSet2 weight lId)
  with e ->
    Printf.printf "Cannot read arc in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"


let from_file path =

  let infile = open_in path in

  (* Read all lines until end of file. 
   * n is the current node counter. *)

  let rec loop n graph lId setNumber=
    try
      let line = input_line infile in 

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let ((n2, graph2, l2) , setNumber2) =
        (* Ignore empty lines *)
        if line = "" then ((n, graph, lId), setNumber)

        (* The first character of a line determines its content : u or p. *)
        else match line.[0] with
          | 's' -> (read_set graph n lId setNumber line, setNumber+1)
          | 'p' -> ((n, read_preference graph lId line, lId), 0)

          (* It should be a comment, otherwise we complain. *)
          | _ -> ((n, read_comment graph line, lId), 1)
      in      
      loop n2 graph2 l2 setNumber2 

    with End_of_file -> (graph, lId) (* Done *)
  in
  let (graph, lId) = loop 1 empty_graph [] 1 in

  (* Users with negative balance linked to the origin
     Users with positive balance linked to sink *)
  let graph = create_source_sink_and_link graph lId in
  (* Link users between themselves with *)
  (*let graph = link_users graph lId in *)
  close_in infile ;
  (graph, lId)

  
(* Write the graph in a .dot file*)
let export path graph =
  (* Open a write-file. *)
  let ff = open_out (path^".dot") in

  (* Write in this file. *)
  fprintf ff "digraph graphique1 {\n\tsize=\"20\"\n\tnode [shape = circle];\n";

  (* Write all arcs *)
  e_iter graph (fun id1 id2 (cout,capa) -> fprintf ff "\t%d -> %d [ label = \"%s, %s\" ];\n" id1 id2 cout capa) ;

  fprintf ff "}\n" ;

  close_out ff ;
  ()
