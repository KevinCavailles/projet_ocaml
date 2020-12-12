open Graph
open Tool


let set_node_get_lId graph nodeName id lId setNumber =
  let graph = new_node graph id in
  (graph, (nodeName,id,setNumber)::lId)


let rec set_lNodes graph (lNodes : string list) (id : int) lId setNumber = match lNodes with
  |[] -> (id, graph, lId)
  |nodeName :: rest -> 
    begin 
    let (graph, lId) = set_node_get_lId graph nodeName id lId setNumber in
    set_lNodes graph rest (id+1) lId setNumber
    end


let rec get_id nodeName lId = match lId with   
    |[]-> raise Not_found
    |(nom,id,_)::rest-> if nom = nodeName then id else get_id nodeName rest


let rec get_nodeName idNode lId = match lId with   
    |[]-> raise Not_found
    |(nom,id,_)::rest-> if id = idNode then nom else get_nodeName idNode rest

let set_preference graph (nodeNameSet1 : string) (nodeNameSet2 : string) (weight : string) (lId : (string * id * int) list) =
  let idS1 = get_id nodeNameSet1 lId in
  let idS2 = get_id nodeNameSet2 lId in
  new_arc graph idS1 idS2 (weight, "1")


(* Link the source to every node with "setNumber"=1 with cost=capacity=1 *)
let link_node_to_set graph lId nodeId = 
	List.fold_left (
		fun acu (_,id,setNumber) ->
		if setNumber = 1 
		then new_arc acu nodeId id ("1", "1")
		else acu
	) graph lId


(* Link the every node with "setNumber"=2 to the sink with cost=1 and capacity=capacitySet2 
    Corresponds to the case where every node should have the same capacity*)
let rec link_set_to_node_single graph lId nodeId capacitySet2 = 
  List.fold_left (
  fun acu (_,id,setNumber) ->
    if setNumber = 2 
    then new_arc acu id nodeId  ("1",List.hd capacitySet2)
    else acu
  ) graph lId
    
(* Link the every node with "setNumber"=2 to the sink with cost=1 and different capacities 
   Corresponds to the case where every node should have a different capacity*)
let rec link_set_to_node_multiple graph lId nodeId capacitySet2 = match lId with
   |[] -> graph
   |(_,id,setNumber) :: rest1 -> if setNumber = 2 then
    begin 
    match capacitySet2 with
    |[]-> graph
    |e :: rest2 -> link_set_to_node_multiple (new_arc graph id nodeId ("1",e)) rest1 nodeId rest2
    end
    else link_set_to_node_multiple graph rest1 nodeId capacitySet2


let create_source_sink_and_link graph lId capacitySet2 = 
	let graph = new_node graph 0 in
    let sinkId = (get_max_id graph)+1 in
    let graph = new_node graph sinkId in
  let graph = link_node_to_set graph lId 0 in
  if List.length capacitySet2 = 1 
  then link_set_to_node_single graph lId sinkId capacitySet2
  else link_set_to_node_multiple graph lId sinkId capacitySet2
  
  
  let remove_source_sink_zeroes graph =
    let max_id = (get_max_id graph) in
    let trimedGraph = n_fold graph (fun acu id -> if id > 0 && id < max_id then new_node acu id else acu) empty_graph in
    e_fold graph ( fun acu id1 id2 (cout,capa) ->
        if capa <> "flow:0" && node_exists acu id1 && node_exists acu id2 
        then new_arc acu id1 id2 (cout,capa)
        else acu
    ) trimedGraph