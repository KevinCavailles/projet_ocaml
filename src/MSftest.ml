open MSgfile
open Tool
open FFAlgorithm
open BLF
open Format
open Sys
open MoneySharing
open Printf

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


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(2)
  in

  (* These command-line arguments are not used for the moment. *)

  (* Open file *)
  let (initGraph, l_id) = from_file infile in
  let max_id = get_max_id initGraph in
  (* let () = export outfile initGraph in *)

  (* Rewrite the graph that has been read. *)
 
  let (flow,finalGraph) = ford_fulk_algorithm initGraph 0 max_id in
  let finalGraph = remove_ss_zeroes finalGraph in
  let () = printf "max flow = %f\n" flow in
  let () = write_file outfile finalGraph l_id in 
  let () = export outfile finalGraph in
  (* let () = export infile graph in *) 

  
  (*Uncomment the following line if you have graphviz installed  *)
  (*let retour = command ("dot -Tsvg "^outfile^".dot > "^outfile^".svg") in *)
  ()

