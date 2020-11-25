open Graph
open Tool
open BLF

let g_to_string gr = gmap gr string_of_float
let g_to_float gr = gmap gr float_of_string


(* Create a list of pairs (origin,end) from a list of nodes *)
let rec create_arcs_from_nodes = function
  | [] -> []
  | a :: [] -> []
  | a :: b :: rest -> (a,b) :: (create_arcs_from_nodes (b :: rest))
  


(* Return the minimum value of a path's edge*)
let get_min_label_from_path (graph : float graph) (path : (id * id) list) =
  let min = Some 999.0 in
  let min = List.fold_left
    (
      fun acu (id1, id2) -> 
        let label = find_arc graph id1 id2 in 
        if label < acu then label else acu
    ) min path in
    match min with
    |None -> 999.0
    |Some x -> x


(* Add a value to every egde of a path *)
let add_value_to_arcs (graph : float graph) (path : (id * id) list) (value : float) = 
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
  The label of every arc becomes "x/max_capacity" where x 
  is the value of the opposite arc on the residual graph*)
let get_final_graph (initGraph : float graph) (residualGraph : float graph) =
  
  (* First get the initial and residual graph as string graphs *)
  let initGraphString = initGraph in
  let residualGraphString = residualGraph in
  let finalGraph = clone_nodes initGraph in
  
  (* For every arc in the initial graph, we get its label (aka max_capacity)
    then, we get the label of the opposite arc in the residual graph. 
    If it exists then the arc of the final graph gets the label "x/max_capacity",
    "0/max_capacity" otherwise*)
  e_fold initGraph
  (
    fun acu id1 id2 x ->
    let label_arc = (match find_arc initGraphString id1 id2 with
      |None -> 0.0
      |Some x -> x) in
    let label_rev_arc = match find_arc residualGraphString id2 id1 with
      |None -> 0.0
      |Some x -> (match find_arc initGraphString id2 id1 with
        |None -> x
        |Some y -> Float.sub x y ) in
    let label_arc = string_of_float label_arc in
    let label_rev_arc = if (label_rev_arc > 0.0) then (string_of_float label_rev_arc) else "0" in
    new_arc acu id1 id2 (label_rev_arc^"/"^label_arc)  
  )
  finalGraph



let ford_fulk_algorithm (graph : float graph) (origin : id) (sink : id) = 
  let flow = 0.0 in

  let initGraph = graph in
  let rec boucle graph origin sink flow = 
    
    let path = get_path graph origin sink in
    match path with
      |None -> (flow, graph)
      |Some x ->
        (let path = x in 
        let arcs = create_arcs_from_nodes path in
        
        (*let () = printf "dans boucle\n" in*)
        
        (* Find the min value of the path *)
        let min = get_min_label_from_path graph arcs in
    
        (* Substract the min to every arc of the path *)
        let graph = add_value_to_arcs graph arcs (Float.neg min) in


        (* Get the reverse path *)
        let reverse = rev_arcs arcs in
    
        (* Add the min to every arc of the reverse path *)
        let graph = add_value_to_arcs graph reverse min in
    
        (* Add the min to the flow *) 
        let flow = Float.add flow min in
        boucle graph origin sink flow) in
  let (maxFlow, residualGraph) = boucle graph origin sink flow in
  let finalGraph = get_final_graph initGraph residualGraph in 
  (maxFlow, finalGraph) 
