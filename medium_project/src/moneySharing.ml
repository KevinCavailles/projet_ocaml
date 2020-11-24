open Graph
open Tool


(*fonction qui créé le noeud associé à un utilisateur et rentre la correspondance dans la table des id*)
let init_node g user id l_id=
    ((new_node g id), ((user,id,0)::l_id))


(*fonction qui renvoie l'id d'un utilisateur*)   
let rec get_id utilisateur l_id= match l_id with   
    |[]-> raise Not_found
    |(a,id1,val)::b-> if a=utilisateur then id1 else get_id utilisateur b

(*fonction qui renvoie le nom correspondant à un id*)   
let rec get_user id1 l_id= match l_id with   
    |[]-> raise Not_found
    |(nom,a,val)::b-> if a=id1 then nom else get_user id1 b

let set_val_du a l_id montant l_utilisateurs=
    List.map (fun (nom,id,val)-> if nom=a 
                                 then (nom,id,(Float.sub val (Float.div montant (Float.of_int(List.length l_utilisateurs)))))
                                 else (nom,id,val)
                                 ) l_id

let set_val_pret utilisateur montant l_id= 
    List.map (fun (nom,id,val)-> if nom=utilisateur 
                                 then (nom,id,(Float.add val montant))
                                 else (nom,id,val)
                                 ) l_id
    
(*fonction qui rentre les paiements réalisés*)
let rec paiement g utilisateur l_utilisateurs montant l_id= 
    set_val_pret utilisateur montant l_id
    match l_utilisateurs with
    |[]-> (g, l_id)
    |a::b-> paiement g utilisateur b montant (set_val_du a l_id montant l_utilisateurs)



