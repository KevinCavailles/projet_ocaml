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
let get_min_label_from_path (graph : (int * int) graph) (path : (id * id) list) =
  let min = 999999999 in
  List.fold_left
    (
      fun acu (id1, id2) -> 
        let label = ( match find_arc graph id1 id2 with
        |None -> 999999999
        |Some (cout,capa) -> cout) in 
        if label < acu then label else acu
    ) min path


(* Add a value to every egde of a path *)
let add_value_to_arcs (graph : (int * int) graph) (path : (id * id) list) (value : int) = 
  List.fold_left 
    (
      fun acu (id1, id2) -> 
        add_arc acu id1 id2 value    
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
        |None -> cout_x
        |Some (cout_y, capa_y) -> Int.sub cout_x cout_y ) in
    let label_rev_arc = if (label_rev_arc > 0) then ((string_of_int label_rev_arc),(string_of_int capa_x )) else "cout:0,capa:"^(string_of_int capa_x) in
    new_arc acu id1 id2 (label_rev_arc, capa_x)  
  )
  finalGraph


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

        (* Find the min value of the path *)
        let min = get_min_label_from_path graph arcs in
    
        (* Substract the min to every arc of the path *)
        let graph = add_value_to_arcs graph arcs (Int.neg min) in

        (* Get the reverse path *)
        let reverse = rev_arcs arcs in
    
        (* Add the min to every arc of the reverse path *)
        let graph = add_value_to_arcs graph reverse min in
    
        (* Add the min to the flow *) 
        let flow = Int.add flow min in
        boucle graph origin sink flow) in
  let (maxFlow, residualGraph) = boucle graph origin sink flow in
  let finalGraph = get_final_graph initGraph residualGraph in 
  (maxFlow, finalGraph) 
