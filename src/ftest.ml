open Gfile
open Tool
open FFAlgorithm
open BLF
open Format

let () =
 
  (*/!\ Format de la commande pour lancer le test : 
        ./ftest.native [nom_fichier_lecture] [id_source] [id_dest] [nom_fichier_ecriture]
   ex : ./ftest.native graphs/graph1 0 5 graphs/graph3 *)

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in
  let initGraph = g_to_int graph in

  (* Rewrite the graph that has been read. *)
  let () = write_file outfile graph in
  let (flow,finalGraph) = ford_fulk_algorithm initGraph _source _sink in
  let () = printf "%d\n" flow in
  let () = export outfile finalGraph in
  ()

