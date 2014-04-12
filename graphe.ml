type id_num = int ;;

type 'arete noeud =
{
  id : id_num ;
  peres : (id_num * 'arete) list ;
  fils : (id_num * 'arete) list
} ;;

type ('noeud, 'arete) graphe = 
{
  structure : 'arete noeud array ;
  noeuds : 'noeud array ;
}

let creer_nuage noeuds =
  {
    structure = Array.init (Array.length noeuds) (fun i -> {id = i ; peres = [] ; fils = []}) ;
    noeuds = noeuds
  }
;;

let relier_unsafe graphe depart arrivee liaison =
  let nouveau_depart =
    {
      id = depart ;
      peres = graphe.structure.(depart).peres ;
      fils = (arrivee, liaison)::(graphe.structure.(depart).fils)
    }
  in
  let nouvelle_arrivee =
    {
      id = arrivee ;
      peres = (depart, liaison)::(graphe.structure.(arrivee).peres) ;
      fils = graphe.structure.(arrivee).fils
    }
  in
  graphe.structure.(depart) <- nouveau_depart ;
  graphe.structure.(arrivee) <- nouvelle_arrivee
;;

let relier_s_unsafe graphe p1 p2 liaison =
  relier_unsafe graphe p1 p2 liaison ;
  relier_unsafe graphe p2 p1 liaison
;;

let extraire_valeur = function 
  |None -> failwith "extraire_valeur" 
  |Some(i) -> i
;;

let decomposer graphe =
  if Array.length graphe.structure = 0 then []
  else 
    begin
      let n : int = Array.length graphe.structure in
      let nouveaux_numeros : int option array = Array.make n None in
      let rec composantes_pseudo_connexes (graphe : ('a, 'b)graphe) (graine : id_num) : ('a, 'b) graphe list =
	let id_libre : int ref = ref 0 in
	let rec inscrire (graine : id_num) : unit =
	  match nouveaux_numeros.(graine) with
	  | None ->
	    begin
	      let noeud = graphe.structure.(graine) in
	      nouveaux_numeros.(graine) <- Some(!id_libre) ;
	      incr id_libre ;
	      List.iter (fun (fils, _) -> inscrire fils) noeud.fils ;
	      List.iter (fun (pere, _) -> inscrire pere) noeud.peres
	    end
	  | Some(_) -> ()
	in
	inscrire graine ;
	let nouvelle_structure = Array.make !id_libre {id = -1 ; peres = [] ; fils = []} in
	let nouveaux_noeuds = if !id_libre = 0 then [||] else Array.make !id_libre graphe.noeuds.(0) in
	let rec transformer_voisins = function
	  |[] -> []
	  |(v, a)::t ->
	    match nouveaux_numeros.(v) with
	    | None -> failwith "tagada"
	    | Some(i) -> (i, a)::(transformer_voisins t)
	in
	let rec construire graine =
	  if nouvelle_structure.(extraire_valeur nouveaux_numeros.(graine)).id < 0 then
	    begin
	      let noeud = graphe.structure.(graine) in
	      let nouveau_noeud =
		match nouveaux_numeros.(graine) with
		| None -> failwith "tagadatsointsoin"
		| Some(i) -> {id = i ; peres = transformer_voisins noeud.peres ; fils = transformer_voisins noeud.fils}
	      in
	      nouvelle_structure.(nouveau_noeud.id) <- nouveau_noeud ;
	      nouveaux_noeuds.(nouveau_noeud.id) <- graphe.noeuds.(graine) ;
	      List.iter (fun (fils, _) -> construire fils) noeud.fils ;
	      List.iter (fun (pere, _) -> construire pere) noeud.peres ;
	    end
	  else ()
	in
	construire graine ;
	let graine_suivante = ref graine in
	while !graine_suivante < n && 
	  begin
	    match nouveaux_numeros.(!graine_suivante) with
	    | None -> false
	    | _ -> true
	  end
	do
	  incr graine_suivante
	done ;
	let cc = 
	  {
	    structure = nouvelle_structure ;
	    noeuds = nouveaux_noeuds
	  }
	in
	if !graine_suivante < n then
	  begin
	    cc::((composantes_pseudo_connexes graphe !graine_suivante))
	  end
	else
	  [cc]
      in
      composantes_pseudo_connexes graphe 0
    end
;;

let fusionner liste =
  let rec compter_noeuds = function
    |[] -> 0
    |graphe::t -> (Array.length graphe.structure) + compter_noeuds t
  in
  let n = compter_noeuds liste in
  let nouvelle_structure = Array.make n {id = -1 ; peres = [] ; fils = []} in
  let nouveaux_noeuds = Array.make n None in
  let rec translater_voisins debut = function
    |[] -> []
    |(v, a)::t ->
      (v + debut, a)::(translater_voisins debut t)
  in
  let rec copier_translater debut = function 
    |[] -> ()
    |graphe::t ->
      begin
	let taille_graphe = Array.length graphe.structure in
	for i=0 to taille_graphe - 1 do
	  nouvelle_structure.(debut + i) <-
	    { 
	      id = debut + i ;
	      peres = translater_voisins debut graphe.structure.(i).peres ;
	      fils = translater_voisins debut graphe.structure.(i).fils
	    } ;
	  nouveaux_noeuds.(debut + i) <- Some(graphe.noeuds.(i))
	done ;
	copier_translater (debut + taille_graphe) t
      end
  in
  copier_translater 0 liste ;
  {
    structure = nouvelle_structure ;
    noeuds = Array.map (extraire_valeur) nouveaux_noeuds
  }
;;

let relier = relier_unsafe ;;
let relier_s = relier_unsafe ;;

let sauver_texte out str =
  let taille = String.length str in
  output_binary_int out taille ;
  output_string out str
;;

let charger_texte input =
  let taille = input_binary_int input in
  let str = String.make taille '0' in
  really_input input str 0 taille ;
  str
;;

let sauver_liste out liste fonction =
  output_binary_int out (List.length liste) ;
  let rec aux = function
    |[] -> ()
    |a::b ->
      begin
	fonction out a ;
	aux b
      end
  in
  aux liste
;;

let charger_liste input fonction =
  let taille = input_binary_int input in
  let rec charger restant =
    if restant = 0 then []
    else 
      let element = fonction input in
      (element)::(charger (restant - 1))
  in
  charger taille
;;

let sauver_structure out graphe sauver_arete =
  let sauver_voisins liste =
    sauver_liste out liste (fun out (v, arete) -> output_binary_int out v ; sauver_arete out arete)
  in
  let sauver_noeud n =
    sauver_voisins n.peres ;
    sauver_voisins n.fils 
  in
  output_binary_int out (Array.length graphe.structure) ;
  Array.iter (sauver_noeud) graphe.structure
;;

let charger_structure input contenu_noeuds charger_arete =
  let charger_voisins () =
    charger_liste input (fun input -> 
      let voisin = input_binary_int input in
      let arete = charger_arete input in
      (voisin, arete))
  in
  let charger_noeud i =
    let peres = charger_voisins () in
    let fils = charger_voisins () in
    {
      id = i ;
      peres = peres ;
      fils = fils
    }
  in
  let taille = input_binary_int input in
  {
    structure = Array.init taille (charger_noeud) ;
    noeuds = contenu_noeuds
  }
;;

