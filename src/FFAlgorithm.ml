open Graph
open Tool
open BLF

let g_to_string gr = gmap gr string_of_int
let g_to_int gr = gmap gr int_of_string


(* Create a list of pairs (origin,end) from a list of nodes *)
let rec create_arcs_from_nodes = function
  | [] -> []
  | a :: [] -> []
  | a :: b :: rest -> (a,b) :: (create_arcs_from_nodes (b :: rest))
  


(* Return the minimum value of a path's edge*)
let get_min_capa_from_path (graph : (int * int) graph) (path : (id * id) list) =
  let min = 999999999 in
  List.fold_left
    (
      fun acu (id1, id2) -> 
        let label = ( match find_arc graph id1 id2 with
        |None -> 999999999
        |Some (cout,capa) -> capa) in 
        if label < acu then label else acu
    ) min path


(* Add a value to every egde of a path *)
let add_capa_to_arcs (graph : (int * int) graph) (path : (id * id) list) (value : int) = 
  List.fold_left 
    (
      fun acu (id1, id2) -> 
        add_arc acu id1 id2 value    
    ) 
    graph path

(* Add a value to every egde of a path *)
let add_cost_to_arcs (graph : (int * int) graph) (path : (id * id) list) (min : int)= 
  List.fold_left 
    (
      fun acu (id1, id2) -> 
        let (cout,capa)=match find_arc graph id1 id2 with
          |None -> raise Not_Found
          |Some (cout,capa)->(cout,capa)  in
        new_arc acu id2 id1 (Int.neg cout,Int.add min capa)
    ) 
    graph path
 

(* Reverse a path and its edges 
   ex :[(a, b);(b, c)] -> [(b,a);(c, b)] *)
let rev_arcs (path : (id * id) list) =
  List.map (fun (id1, id2) -> (id2, id1)) path

   
(* Get the final graph after the FFalgorithm 
  The label of every arc becomes "x" where x 
  is the value of the opposite arc on the residual graph*)
let get_final_graph (initGraph : (int * int) graph) (residualGraph : (int * int) graph) =
  
  (* First get the initial and residual graph as string graphs *)
  let initGraphString = initGraph in
  let residualGraphString = residualGraph in
  let finalGraph = clone_nodes initGraph in
  
  (* For every arc in the initial graph we get the label of 
     the opposite arc in the residual graph. 
     If it exists then the arc of the final graph gets the label "x",
     "0" otherwise*)
  e_fold initGraph
  (
    fun acu id1 id2 (cout_x,capa_x) ->
    let label_rev_arc = match find_arc residualGraphString id2 id1 with
      |None -> 0
      |Some (cout_x,capa_x) -> (match find_arc initGraphString id2 id1 with
        |None -> capa_x
        |Some (cout_y, capa_y) -> Int.sub capa_x capa_y ) in
    let label_rev_arc = if (label_rev_arc > 0) then (string_of_int label_rev_arc) else "capa:0" in
    new_arc acu id1 id2 ((string_of_int cout_x), label_rev_arc)
  )
  finalGraph

let get_cout_total (residualGraph : (int * int) graph) =
  e_fold residualGraph (fun acu id1 id2 (l_cout, l_capa) -> if l_cout > 0 then (Int.add acu (Int.mul l_capa l_cout) else acu) 0

(*ne pas oublier d'afficher le cout*)
let ford_fulk_algorithm (graph : (int * int) graph) (origin : id) (sink : id) = 
  let flow = 0 in

  let initGraph = graph in
  let rec boucle graph origin sink flow = 
    
    let path = get_path graph origin sink in
    match path with
      |None -> (flow, graph)
      |Some x ->
        (let path = x in 
        let arcs = create_arcs_from_nodes path in

        (* Find the min capacity of the path *)
        let min = get_min_capa_from_path graph arcs in
    
        (* Substract the min capacity to every arc of the path *)
        let graph = add_capa_to_arcs graph arcs (Int.neg min) in

        (* Get the reverse path *)
        (*let reverse = rev_arcs arcs in*)
    
        (* Add the cost to every arc of the reverse path *)
        (*blf prend chemin seulement si capa non saturé, donc dans tous les cas on met un arc inverse avec -cout et un arc normal avec +cout*)
        let graph = add_cost_to_arcs graph arcs min in
    
        (* Add the min to the flow *) 
        let flow = Int.add flow min in
        boucle graph origin sink flow) in
  let (maxFlow, residualGraph) = boucle graph origin sink flow in
  let finalGraph = get_final_graph initGraph residualGraph in 
  let cout_total = get_cout_total residualGraph in
  (maxFlow, cout_total,finalGraph) 
