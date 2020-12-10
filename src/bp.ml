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


let link_node_to_set graph lId nodeId = 
	List.fold_left (
		fun acu (_,id,setNumber) ->
		if setNumber = 1 
		then new_arc acu nodeId id ("1", "1")
		else acu
	) graph lId


let link_set_to_node graph lId nodeId = 
	List.fold_left (
		fun acu (_,id,setNumber) ->
		if setNumber = 2 
		then new_arc acu id nodeId ("1", "1")
		else acu
	) graph lId


let create_source_sink_and_link graph lId = 
	let graph = new_node graph 0 in
    let sinkId = (get_max_id graph)+1 in
    let graph = new_node graph sinkId in
	let graph = link_node_to_set graph lId 0 in
	link_set_to_node graph lId sinkId
  
  let remove_source_sink_zeroes graph =
    let max_id = (get_max_id graph) in
    let trimedGraph = n_fold graph (fun acu id -> if id > 0 && id < max_id then new_node acu id else acu) empty_graph in
    e_fold graph ( fun acu id1 id2 (cout,capa) ->
        if capa <> "flow:0" && node_exists acu id1 && node_exists acu id2 
        then new_arc acu id1 id2 (cout,capa)
        else acu
    ) trimedGraph