open Graph
open Tool


(*fonction qui créé le noeud associé à un utilisateur et rentre la correspondance dans la table des id*)
let init_node graph nom id l_id =
    ( (new_node graph id), ((nom,id,0.0)::l_id) )


(*fonction qui renvoie l'id d'un utilisateur*)   
let rec get_id utilisateur l_id= match l_id with   
    |[]-> raise Not_found
    |(nom,id,_)::rest-> if nom = utilisateur then id else get_id utilisateur rest

(*fonction qui renvoie le nom correspondant à un id*)   
let rec get_user id1 l_id = match l_id with   
    |[]-> raise Not_found
    |(nom,id,_)::rest-> if id = id1 then nom else get_user id1 rest

let set_val_du utilisateur l_id montant l_utilisateurs =
    let length = List.length l_utilisateurs in
    List.map (fun (nom,id,value)-> if nom = utilisateur 
                                 then (nom,id,(Float.sub value (Float.div montant (Float.of_int(length)))))
                                 else (nom,id,value)
             ) l_id

let set_val_pret utilisateur montant l_id = 
    List.map (fun (nom,id,value)-> if nom=utilisateur 
                                 then (nom,id,(Float.add value montant))
                                 else (nom,id,value)
             ) l_id
    
(*fonction qui rentre les paiements réalisés*)
let paiement graph utilisateur l_utilisateurs montant l_id = 
    let l_id = set_val_pret utilisateur montant l_id in
    let length = List.length l_utilisateurs in
    let l_id = List.map (fun (nom,id,value) -> if List.mem nom l_utilisateurs
                                    then (nom,id, (Float.sub value (Float.div montant (Float.of_int(length)))) )
                                    else (nom, id, value)
             ) l_id in
    (graph, l_id) 
    (* let rec paye graph utilisateur l_utilisateurs montant l_id = match l_utilisateurs with
        |[]-> (graph, l_id)
        |x::rest -> paye graph utilisateur rest montant (set_val_du x l_id montant l_utilisateurs) in
    paye graph utilisateur l_utilisateurs montant l_id *)


let link_users_helper graph user_id l_id =
    List.fold_left (fun acu (nom, id, value) -> if id <> user_id then add_arc acu user_id id 999999999.0 else acu) graph l_id 

let link_users graph l_id = 
    List.fold_left (fun acu (_, id, _) -> (link_users_helper acu id l_id) ) graph l_id

let link_users_sink_origin graph l_id source sink = 
    List.fold_left (
            fun acu (_,id,value) -> 
            if value > 0.0 then 
            add_arc acu id sink value
            else 
                if value < 0.0 then
                add_arc acu source id (Float.neg value)
                else acu 
        ) graph l_id


let set_sink_origin graph l_id =
    let graph = new_node graph 0 in
    let sink_id = (get_max_id graph)+1 in
    let graph = new_node graph sink_id in
    link_users_sink_origin graph l_id 0 sink_id 

 
let remove_ss_zeroes graph =
    let max_id = (get_max_id graph) in
    let trimedGraph = n_fold graph (fun acu id -> if id > 0 && id < max_id then new_node acu id else acu) empty_graph in
    e_fold graph ( fun acu id1 id2 label ->
        if label <> "0" && node_exists acu id1 && node_exists acu id2 
        then new_arc acu id1 id2 label
        else acu
    ) trimedGraph
