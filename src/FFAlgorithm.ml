open Graph
open Tool

type path = id list

(* Create a list of pairs (origin,end) from a list of nodes *)
let rec create_arcs_from_nodes = function
  | [] -> []
  | a :: [] -> []
  | a :: b :: rest -> (a,b) :: (create_arcs_from_nodes (b :: rest))
  


(* Return the minimum value of a path's arcs*)
let get_min_label_from_path (graph : int graph) (path : (id * id) list) =
  let min = Some 999 in
  List.fold_left
    (
      fun acu (id1, id2) -> 
        let label = find_arc graph id1 id2 in 
        if label < acu then label else acu
    )
    min path


(* Add a value to every arc of a path *)
let add_value_to_arcs (graph : int graph) (path : (id * id) list) (value : int) = 
  List.fold_left 
    (
      fun acu (id1, id2) -> 
        add_arc acu id1 id2 value    
    ) 
    graph path


(* Reverse a path and its arc 
   ex :[(a, b);(b, c)] -> [(b,a);(c, b)] *)
let rev_arcs (path : (id * id) list) =
  List.map (fun (id1, id2) -> (id2, id1)) path


(* Get the final graph after the FFalgorithm 
  The label of every arc becomes "x/max_capacity" where x 
  is the value of the opposite arc on the residual graph*)
let get_final_graph (initGraph : int graph) (residualGraph : int graph) =
  
  (* First get the initial and residual graph as string graphs *)
  let initGraphString = gmap initGraph string_of_int in
  let residualGraphString = gmap residualGraph string_of_int in
  let finalGraph = clone_nodes initGraph in
  
  (* For every arc in the initial graph, we get its label (aka max_capacity)
    then, we get the label of the opposite arc in the residual graph. 
    If it exists then the arc of the final graph gets the label "x/max_capacity",
    "0/max_capacity" otherwise*)
  e_fold initGraph
  (
    fun acu id1 id2 x ->
    let label_arc = find_arc initGraphString id1 id2 in
    let label_arc = (match label_arc with
    |None -> "-1"
    |Some x -> x) in
    let label_rev_arc = find_arc residualGraphString id2 id1 in
    match label_rev_arc with
    |None -> new_arc acu id1 id2 ("0/"^label_arc) 
    |Some x -> new_arc acu id1 id2 (""^x^"/"^label_arc)  

  )
  finalGraph



(*let fordFulkAlgorithm graph origin end = assert false *)
(* 
  let flow = 0 in
  While there's a path
    Find a path
    let path = xxxxx graph origin end [] in
    let arcs = create_arcs_from_nodes path in

    Find the min value of the path
    let min = get_min_label_from_path graph arcs in

    Substract the min to every arc of the path
    graph = add_value_to_arcs graph arcs (-min) in

    Get the reverse path
    let reverse = rev_arcs arcs in

    Add the min to every arc of the reverse path
    graph = add_value_to_arcs graph reverse min in

    Add the min to the flow
    flow = flow + min

  return the flow 
  *)