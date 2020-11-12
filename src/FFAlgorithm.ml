open Graph
open Tool


(* Create a list of pairs (origin,end) from a list of nodes *)
let rec createArcFromNodes = function
  | a -> []
  | a :: b :: rest -> (a,b) :: (createArcsFromNodes (b :: rest))


(* Return the minimum value of a path's arcs*)
let getMinLabelFromPath (graph : int graph) (path : (id * id) list) =
  let min = 999 in
  List.foldleft
    (
      fun acu (id1, id2) -> 
        let label = int_of_string (find_arc graph id1 id2) in 
        if label < acu then acu = label
    )
    min path


(* Add a value to every arc of a path *)
let addValueToArcs (graph : int graph) (path : (id * id) list) (value : int) = 
  List.foldleft 
    (
      fun acu (id1, id2) -> 
        let vArc = find_arc graph id1 id2 in
        add_arc acu (vArc + value) 
    ) 
    graph path


(* Reverse a path and its arc 
   ex :[(a, b);(b, c)] -> [(b,a);(c, b)] *)
let revArcs (path : (id * id) list) =
  List.map (fun (id1, id2) -> (id2, id1)) path


let fordFulkAlgorithm graph origin end = 
(* 
  let flow = 0 in
  While there's a path
    Find a path
    let path = findPath graph origin end [] in
    let arcs = createArcFromNodes path in

    Find the min value of the path
    let min = getMinLabelFromPath graph arcs in

    Substract the min to every arc of the path
    graph = addValueToArcs graph arcs (-min) in

    Get the reverse path
    let reverse = revArcs arcs in

    Add the min to every arc of the reverse path
    graph = addValueToArcs graph reverse min in

    Add the min to the flow
    flow = flow + min

  return the flow 
  *)