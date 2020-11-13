open Graph

(*type record avec id noeud et son cout*)
type t_cost={
    mutable cout:int;
    mutable father:int 
    }

let blf gr=
    (*je compte le nb de noeuds dans le graphe pour instancier mon tableau*)
    let nb_n=n_fold gr (fun acu id->acu+1) 0 in
    
    let cost ={cout=max_int; father=0} in

    let acu =Array.make nb_n cost in
    (*je fais un fold_left pour pouvoir individualiser au niveau de la mémoire les cases de la table*)
    let blf_tab=n_fold gr (fun acu id->acu.(id)<-{cout=max_int; father=0}; acu ) acu in

    let file_id=[0] in

    let rec blf_rec gr file_id = match file_id with
        |[]-> blf_tab
        |a::b-> 
        let l_out_arc=out_arcs gr a in
            let rec loop_suc file_id l_out_arc blf_tab=match l_out_arc with
                |[]-> blf_rec gr b
                |(id,label)::d-> 
                    if (blf_tab.(a).cout+label)<blf_tab.(id).cout then
                    begin
                        blf_tab.(id).cout<-(blf_tab.(a).cout+label);
                        blf_tab.(id).father<-a; 
                        if not (List.mem id file_id ) then loop_suc (List.append file_id [id]) d blf_tab else loop_suc file_id d blf_tab
                    end
                    else loop_suc file_id d blf_tab in
        loop_suc file_id l_out_arc blf_tab in
    blf_rec gr file_id

(*avec blf_tab, on retrace chemin avec les pères*)
let get_path blf_tab id_dest=
    let path=[id_dest] in
    let rec loop path blf_tab id_dest= 
        let father_id=blf_tab.(id_dest).father in match father_id with
           |0->(0::path)
           |a->loop (a::path) blf_tab a in
    loop path blf_tab id_dest
