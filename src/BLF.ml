open Graph

type path = id list

(*type record avec id noeud et son cout*)
type t_cost = {
    mutable cout:int;
    mutable father:int 
    }

let blf graph idSrc =
    (*je compte le nb de noeuds dans le graphe pour instancier mon tableau*)
    let nb_n = n_fold graph (fun acu id->acu+1) 0 in
    
    let cost = {cout=max_int; father=(-1)} in

    let acu = Array.make nb_n cost in
    (*je fais un fold_left pour pouvoir individualiser au niveau de la mémoire les cases de la table*)
    let blfTab = n_fold graph (fun acu id->acu.(id)<-{cout = max_int; father = (-1) }; acu ) acu in
    blfTab.(idSrc).cout <- 0;
    let fileId = [idSrc] in
    let fileMarquee = [] in

    let rec blf_rec graph fileId fileMarquee= match fileId with
        |[]-> blfTab
        |a::b-> 
        let lOutArcs = out_arcs graph a in
            let rec loop_suc lOutArcs blfTab file =
                match lOutArcs with
                |[] -> blf_rec graph file (a::fileMarquee)
                |(id,label)::d -> 
                    if label != 0 && (blfTab.(a).cout+label) < blfTab.(id).cout then
                    begin
                        blfTab.(id).cout <- (blfTab.(a).cout+label);
                        blfTab.(id).father <- a; 
                        if not (List.mem id fileMarquee) then loop_suc d blfTab (id::file) else loop_suc d blfTab file
                    end
                    else loop_suc d blfTab file in
        loop_suc lOutArcs blfTab b in
    blf_rec graph fileId fileMarquee 

(*avec blfTab, on retrace chemin avec les pères*)
let get_path graph idSrc idDst =
    let blfTab = blf graph idSrc in
    let path = [idDst] in
    let rec loop path blfTab idSrc idDst = 
        let fatherId = blfTab.(idDst).father in match fatherId with
           |(-1) -> None 
           |a -> if a == idSrc then Some (idSrc::path) else loop (a::path) blfTab idSrc a in
    loop path blfTab idSrc idDst
