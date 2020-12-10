open BPgfile
open Tool
open BGAlgorithm
open BLF
open Format
open Sys
open Printf
open Bp

let () =
 
  (*/!\ Format de la commande pour lancer le test : 
        ./ftest.native [nom_fichier_lecture] [id_source] [id_dest] [nom_fichier_ecriture]
   ex : ./ftest.native graphs/graph1 0 5 graphs/graph3 *)

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 3 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2)*)



  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(2)

  in

  let () = printf "debug args\n" in

  (* These command-line arguments are not used for the moment. *)

  (* Open file *)
  let (initGraph, lId) = from_file infile in
  let initGraph = g_to_int initGraph in
  let idMax = get_max_id initGraph in
  
 
  let (flow, cost, finalGraph) = busacker_gowen_algorithm initGraph 0 idMax in
  let finalGraph = remove_source_sink_zeroes finalGraph in
  let () = printf "max flow = %d, cost = %d\n" flow cost in

  let () = write_file outfile finalGraph lId in 
  let () = export outfile finalGraph in

  
  (*Uncomment the following line if you have graphviz installed  *)
  (*let retour = command ("dot -Tsvg "^outfile^".dot > "^outfile^".svg") in *)
  ()

