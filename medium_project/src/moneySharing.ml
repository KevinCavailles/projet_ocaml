open Graph
open Tool


(*fonction qui créé le noeud associé à un utilisateur et rentre la correspondance dans la table des id*)
let init_node g user id l_id=
    ((new_node g id), ((user,id)::l_id))


(*fonction qui renvoie l'id d'un utilisateur*)   
let rec get_id utilisateur l_id= match l_id with   
    |[]-> raise Not_found
    |(a,id1)::b-> if a=utilisateur then id1 else get_id utilisateur b

(*fonction qui renvoie le nom correspondant à un id*)   
let rec get_user id1 l_id= match l_id with   
    |[]-> raise Not_found
    |(nom,a)::b-> if a=id1 then nom else get_user id1 b

(*fonction qui rentre les paiements réalisés*)
let rec paiement g utilisateur l_utilisateurs montant l_id= match l_utilisateurs with
    |[]-> (g, l_id)
    |a::b-> if not(a=utilisateur) 
            then paiement (add_arc g (get_id utilisateur l_id) (get_id a l_id) (Float.div montant (Float.of_int(List.length l_utilisateurs)))) utilisateur b montant l_id 
            else paiement g utilisateur b montant l_id




