open Graph
open Printf
open MoneySharing

type path = string

(* Format of text files:
   % Welcome to MoneySharing, your favorite tool to ease your reimbursements !

   % Please, type the name of all users of your group:
   u Gaby
   u Flo
   u Macha

   % You can now enter your payements as it follows: p userWhoPaid [forWhichUser1; forWhichUser2 ..] amount
   p Flo Gaby,Flo,Macha 11.0
   p Gaby Flo 8.5

*)


let write_file path graph l_id=

  (* Open a write-file. *)
  let ff = open_out path in

  (* Write in this file. *)
  fprintf ff "%% Here is your MoneySharing graph.\n\n" ;

  (* Write all users *)
  n_iter_sorted graph (fun id -> fprintf ff "u %s\n" (get_user id l_id)) ;
  fprintf ff "\n" ;

  fprintf ff "%% Here are the reimbursements to be made.\n\n" ;

  (* Write all arcs *)
  e_iter graph (fun id1 id2 lbl -> fprintf ff "p %s %s %s\n" (get_user id1 l_id) (get_user id2 l_id) lbl) ;

  fprintf ff "\n%% End of reimbursements\n" ;

  close_out ff ;
  ()

let read_comment graph line l_id=
  try Scanf.sscanf line " %%" (graph, l_id)
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line ;
    failwith "from_file"

(* Reads a line with a user. *)
let read_user id graph l_id line =
  try Scanf.sscanf line "u %s" (fun user -> init_node graph user id l_id )
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* Reads a line with a payement. *)
let read_payement graph l_id line =
  try Scanf.sscanf line "p %s %s %f"
        (fun user l_user label -> paiement graph user (String.split_on_char ',' l_user) label l_id)
  with e ->
    Printf.printf "Cannot read arc in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

let from_file path =

  let infile = open_in path in

  (* Read all lines until end of file. 
   * n is the current node counter. *)
  let rec loop n graph l_id=
    try
      let line = input_line infile in 

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let (n2, (graph2, l2)) =
        (* Ignore empty lines *)
        if line = "" then (n, (graph, l_id))

        (* The first character of a line determines its content : u or p. *)
        else match line.[0] with
          | 'u' -> (n+1, read_user n graph l_id line)
          | 'p' -> (n, read_payement graph l_id line)

          (* It should be a comment, otherwise we complain. *)
          | _ -> (n, read_comment graph line l_id)
      in      
      loop n2 graph2 l2

    with End_of_file -> (graph, l_id) (* Done *)
  in
  let final_graph_lid= loop 0 empty_graph [] in

  close_in infile ;
  final_graph_lid

(* Write the graph in a .dot file*)
let export path graph =
  (* Open a write-file. *)
  let ff = open_out (path^".dot") in

  (* Write in this file. *)
  fprintf ff "digraph graphique1 {\n\tsize=\"20\"\n\tnode [shape = circle];\n";

  (* Write all arcs *)
  e_iter graph (fun id1 id2 lbl -> fprintf ff "\t%d -> %d [ label = \"%s\" ];\n" id1 id2 lbl) ;

  fprintf ff "}\n" ;

  close_out ff ;
  ()