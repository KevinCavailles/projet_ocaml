open Graph

type path = id list

(*type record avec id noeud et son cout*)
type t_cost={
    mutable cout:int;
    mutable father:int 
    }

let blf gr id_src id_dest=
    (*je compte le nb de noeuds dans le graphe pour instancier mon tableau*)
    let nb_n=n_fold gr (fun acu id->acu+1) 0 in
    
    let cost ={cout=max_int; father=(-1)} in

    let acu =Array.make nb_n cost in
    (*je fais un fold_left pour pouvoir individualiser au niveau de la mémoire les cases de la table*)
    let blf_tab=n_fold gr (fun acu id->acu.(id)<-{cout=max_int; father=(-1)}; acu ) acu in
    blf_tab.(id_src).cout<-0;
    let file_id=[id_src] in
    let file_marque =[] in

    let rec blf_rec gr file_id file_marque= match file_id with
        |[]-> blf_tab
        |a::b-> 
        let l_out_arc=out_arcs gr a in
            let rec loop_suc l_out_arc blf_tab file =
                match l_out_arc with
                |[]-> blf_rec gr file (a::file_marque)
                |(id,(lcout,lcapa))::d-> 
                    if lcapa <> 0 && (Int.add blf_tab.(a).cout lcout)<(blf_tab.(id).cout) then
                    begin
                        blf_tab.(id).cout<-(Int.add blf_tab.(a).cout lcout);
                        blf_tab.(id).father<-a; 
                        if not (List.mem id file_marque) then loop_suc d blf_tab (id::file) else loop_suc d blf_tab file
                    end
                    else loop_suc d blf_tab file in
        loop_suc l_out_arc blf_tab b in
    blf_rec gr file_id file_marque 

(*avec blf_tab, on retrace chemin avec les pères*)
let get_path gr id_src id_dest=
    let blf_tab=blf gr id_src id_dest in
    let path=[id_dest] in
    let rec loop path blf_tab id_src id_dest= 
        let father_id=blf_tab.(id_dest).father in match father_id with
           |(-1)->None 
           |a->if a == id_src then Some (id_src::path) else loop (a::path) blf_tab id_src a in
    loop path blf_tab id_src id_dest
