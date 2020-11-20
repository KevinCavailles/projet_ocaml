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
let get_min_label_from_path (graph : int graph) (path : (id * id) list) =
  let min = Some 999 in
  let min = List.fold_left
    (
      fun acu (id1, id2) -> 
        let label = find_arc graph id1 id2 in 
        if label < acu then label else acu
    ) min path in
    match min with
    |None -> 999
    |Some x -> x


(* Add a value to every egde of a path *)
let add_value_to_arcs (graph : int graph) (path : (id * id) list) (value : int) = 
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

  
(* Removes the edges whose label = 0 *)
let remove_zeroes (graph : int graph) = 
  let initGraph = clone_nodes graph in
  e_fold graph
  (
    fun acu id1 id2 x ->
    if x = 0 then acu else new_arc acu id1 id2 x  
  ) initGraph

(* Remove bi-directional edges between 2 nodes*)
let only_one_edge (graph : int graph) = 
  let graphWithZeroes = e_fold graph 
  (
    fun acu id1 id2 x -> 
    let path = [(id1,id2);(id2,id1)] in
    
    let label_rev = (match find_arc graph id2 id1 with
    |None -> 0
    |Some x -> x) in
    let mini = min x label_rev in
    let gr = add_value_to_arcs graph path (-mini) in
    if x = 0 || mini = 0 then acu else gr
  )
  graph in
  let graphWithoutZeroes = remove_zeroes graphWithZeroes in
  graphWithoutZeroes


(* Get the final graph after the FFalgorithm 
  The label of every arc becomes "x/max_capacity" where x 
  is the value of the opposite arc on the residual graph*)
let get_final_graph (initGraph : int graph) (residualGraph : int graph) =
  
  (* First get the initial and residual graph as string graphs *)
  let initGraphString = g_to_string initGraph in
  let residualGraphString = g_to_string residualGraph in
  let finalGraph = clone_nodes initGraph in
  
  (* For every arc in the initial graph, we get its label (aka max_capacity)
    then, we get the label of the opposite arc in the residual graph. 
    If it exists then the arc of the final graph gets the label "x/max_capacity",
    "0/max_capacity" otherwise*)
  e_fold initGraph
  (
    fun acu id1 id2 x ->
    let label_arc = (match find_arc initGraphString id1 id2 with
    |None -> "-1"
    |Some x -> x) in
    let label_rev_arc = find_arc residualGraphString id2 id1 in
    match label_rev_arc with
    |None -> new_arc acu id1 id2 ("0/"^label_arc) 
    |Some x -> new_arc acu id1 id2 (""^x^"/"^label_arc)  
  )
  finalGraph

let ford_fulk_algorithm (graph : int graph) (origin : id) (sink : id) = 
  let flow = 0 in

  let graph = only_one_edge graph in 
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
        let graph = add_value_to_arcs graph arcs (-min) in


        (* Get the reverse path *)
        let reverse = rev_arcs arcs in
    
        (* Add the min to every arc of the reverse path *)
        let graph = add_value_to_arcs graph reverse min in
    
        (* Add the min to the flow *) 
        let flow = flow + min in
        boucle graph origin sink flow) in
  let (maxFlow, residualGraph) = boucle graph origin sink flow in
  let finalGraph = get_final_graph initGraph residualGraph in 
  (maxFlow, finalGraph) 
  