(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)


let clone_nodes gr = n_fold gr new_node empty_graph 


(* Clone the nodes first then clone every arc but change their label by applying f*)
let gmap gr f = 
  let new_graph = clone_nodes gr in
  e_fold gr (fun acu id1 id2 x -> new_arc acu id1 id2 (f x)) new_graph 

let add_arc g id1 id2 n =
    let f=find_arc id1 id2 g in
    match f with
   |None->new_arc g id1 id2 n
   |Some x->new_arc g id1 id2 (n+x)
