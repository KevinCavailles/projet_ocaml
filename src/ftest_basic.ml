open Gfile
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
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)



  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(2)


  and _source = int_of_string Sys.argv.(3)
  and _sink = int_of_string Sys.argv.(4)

  in

  let () = printf "debug args\n" in

  (* These command-line arguments are not used for the moment. *)

  (* Open file *)
  let initGraph = from_file infile in
  let () = export outfile initGraph in 
  let initGraph = g_to_int initGraph in
  

  (* Rewrite the graph that has been read. *)
 
  let (flow, cout, finalGraph) = busacker_gowen_algorithm initGraph _source _sink in
  (*let finalGraph = remove_ss_zeroes finalGraph in*)
  let () = printf "max flow = %d, cout = %d\n" flow cout in

  let () = write_file outfile finalGraph in 
  let () = export outfile finalGraph in
  (* let () = export infile graph in *) 

  
  (*Uncomment the following line if you have graphviz installed  *)
  (*let retour = command ("dot -Tsvg "^outfile^".dot > "^outfile^".svg") in *)
  ()

