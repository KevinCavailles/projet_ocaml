open Graph
open Tool
open BLF

let g_to_string gr = gmap gr (fun (a,b) -> (string_of_int a, string_of_int b) )
let g_to_int gr = gmap gr (fun (a,b) -> (int_of_string a, int_of_string b) )


(* Create a list of pairs (origin,end) from a list of nodes *)
let rec create_arcs_from_nodes = function
  | [] -> []
  | a :: [] -> []
  | a :: b :: rest -> (a,b) :: (create_arcs_from_nodes (b :: rest))
  


(* Return the minimum capacity of a path's edge*)
let get_min_capa_from_path (graph : (int * int) graph) (path : (id * id) list) =
  let min = 999999999 in
  List.fold_left
    (
      fun acu (id1, id2) -> 
        let label = ( match find_arc graph id1 id2 with
        |None -> 999999999
        |Some (_,capa) -> capa) in 
        if label < acu then label else acu
    ) min path

(* Return the total cost of a path in the graph*)
let get_cost_from_path (graph : (int * int) graph) (path : (id * id) list) = 
  List.fold_left
    (
      fun acu (id1, id2) ->
        match find_arc graph id1 id2 with
        |None -> acu
        |Some (cost,_) -> acu + cost
    ) 0 path

(* Add a value to the capacity of every egde of a path in the graph*)
let add_capa_to_arcs (graph : (int * int) graph) (path : (id * id) list) (capa : int) = 
  List.fold_left 
    (
      fun acu (id1, id2) -> 
        add_capa acu id1 id2 capa    
    ) 
    graph path

(* Add a value to every egde of a path *)
let add_cost_to_arcs (graph : (int * int) graph) (path : (id * id) list) (min : int)= 
  List.fold_left 
    (
      fun acu (id1, id2) -> 
        let (cost,capa)=match find_arc graph id1 id2 with
          |None -> raise Not_found
          |Some (cost,capa)->(cost,capa)  in
        new_arc acu id2 id1 (Int.neg cost,Int.add min capa)
    ) 
    graph path
 

(* Reverse a path and its edges 
   ex :[(a, b);(b, c)] -> [(b, a);(c, b)] *)
let rev_arcs (path : (id * id) list) =
  List.map (fun (id1, id2) -> (id2, id1)) path

   
(* Get the final graph after the FFalgorithm 
  The label of every arc becomes "flow:x" where x 
  is the value of the flow of the opposite arc in the residual graph*)
let get_final_graph (initGraph : (int * int) graph) (residualGraph : (int * int) graph) =
  
  (* First get the initial and residual graph as string graphs *)
  let finalGraph = clone_nodes initGraph in
  
  (* For every arc in the initial graph we get the label of 
     the opposite arc in the residual graph. 
     If it exists then the arc of the final graph gets the label (cost,"flow:x"),
     (cost,"flow:0") otherwise*)
  e_fold initGraph
  (
    fun acu id1 id2 (cost_x,capa_x) ->
    let label_rev_arc = match find_arc residualGraph id2 id1 with
      |None -> 0
      |Some (cost_x,capa_x) -> (match find_arc initGraph id2 id1 with
        |None -> capa_x
        |Some (cost_y, capa_y) -> Int.sub capa_x capa_y ) in
    let label_rev_arc = if (label_rev_arc > 0) then "flow:"^(string_of_int label_rev_arc) else "flow:0" in
    new_arc acu id1 id2 ((string_of_int cost_x), label_rev_arc)
  )
  finalGraph


let busacker_gowen_algorithm (graph : (int * int) graph) (origin : id) (sink : id) = 
  let flow = 0 in
  let totalCost = 0 in
  
  let initGraph = graph in
  let rec boucle graph origin sink flow totalCost = 
   
    let path = get_path graph origin sink in
    match path with
      |None -> (flow, totalCost, graph)
      |Some x -> begin
        let path = x in 
        let arcs = create_arcs_from_nodes path in

        (* Find the min capacity of the path *)
        let minPath = get_min_capa_from_path graph arcs in
        let costPath = get_cost_from_path graph arcs in
    
        (* Substract the min capacity to every arc of the path *)
        let graph = add_capa_to_arcs graph arcs (Int.neg minPath) in

        (* Get the reverse path *)
    
        (* Add the cost to every arc of the reverse path *)
        (*blf prend chemin seulement si capa non saturé, donc dans tous les cas on met un arc inverse avec -cout et un arc normal avec +cout*)
        let graph = add_cost_to_arcs graph arcs minPath in 
    
        (* Add the min to the flow *) 
        let flow = Int.add flow minPath in
        let totalCost = Int.add totalCost (costPath * minPath) in

        boucle graph origin sink flow totalCost
        end in

  let (maxFlow, totalCost, residualGraph) = boucle graph origin sink flow totalCost in

  let finalGraph = get_final_graph initGraph residualGraph in 
  (maxFlow, totalCost , finalGraph) 
