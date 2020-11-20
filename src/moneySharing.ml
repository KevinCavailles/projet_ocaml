open Graph


(*fonction qui créé le noeud associé à un utilisateur et rentre la correspondance dans la table des id*)
let init_node g user id l_id=
    ((new_node g id), ((user,id)::l_id))


(*fonction qui renvoie l'id d'un utilisateur*)   
let get_id utilisateur l_id= match l_id with   
    |[]-> raise Not_found
    |(a,id1)::b-> if a=utlisateur then id1 else get_id utilisateur b

(*fonction qui renvoie le nom correspondant à un id*)   
let get_user id1 l_id= match l_id with   
    |[]-> raise Not_found
    |(nom,a)::b-> if a=id1 then nom else get_user id1 b

(*fonction qui rentre les paiements réalisés*)
let rec paiement g utilisateur l_utilisateurs montant l_id=
    let id1=(get_id utilisateur l_id) in
    match l_utilisateurs with
    |[]-> (g, l_id)
    |a::b-> paiement (add_arc g id1 (get_id a l_id) montant) id1 b montant l_id




