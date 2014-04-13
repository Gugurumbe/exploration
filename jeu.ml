#use "graphe.ml" ;;
#use "monde.ml" ;;

type jeu =
  {
    nom_zone : unit -> string ;
    description : unit -> string ;
    lister_mouvements : unit -> (string * string * float) list ;
    dire : string -> float ; (*renvoie le délai de repos avant la prochaine action*)
    sauver_position : unit -> unit ;
    quitter : unit -> unit
  } ;;

exception Mauvaise_invocation ;;

let init nom_fichier nom_joueur =
  let fichier = open_in_bin nom_fichier in
  let monde = charger_carte fichier in
  close_in fichier ;
  let position_utilisateur = ref 0 in
  begin
    try
      let fichier_utilisateur = open_in_bin nom_joueur in
      position_utilisateur := input_binary_int fichier_utilisateur ;
      close_in fichier_utilisateur 
    with
    | Sys_error("user: No such file or directory") -> ()
  end ;
  let nom_courant () =
    monde.zones.(!position_utilisateur).nom 
  in
  let description_courante () =
    monde.zones.(!position_utilisateur).description
  in
  let mouvements_possibles () =
    let liste_fils = monde.structure.structure.(!position_utilisateur).fils in
    List.map (fun (n_n_voisin, n_porte) -> 
      let zone_arrivee = monde.zones.(monde.structure.noeuds.(n_n_voisin)) in
      let porte = monde.portes.(n_porte) in
      (porte.invocation, zone_arrivee.nom, porte.delai))
      liste_fils
  in
  let dire invocation =
    let rec comparer = function
      |[] -> raise Mauvaise_invocation
      |(n_n_voisin, n_porte)::t when monde.portes.(n_porte).invocation = invocation ->
	begin
	  position_utilisateur := monde.structure.noeuds.(n_n_voisin) ;
	  monde.portes.(n_porte).delai
	end
      |_::t -> comparer t
    in
    comparer monde.structure.structure.(!position_utilisateur).fils
  in
  let sauver_position () =
    let fichier = open_out_bin nom_joueur in
    output_binary_int fichier (!position_utilisateur) ;
    flush fichier ;
    close_out fichier ;
  in
  let quitter () =
    sauver_position () ;
    let fichier = open_out_bin nom_fichier in
    sauver_carte fichier monde ;
    flush fichier ;
    close_out fichier
  in
  {
    nom_zone = nom_courant ;
    description = description_courante ;
    lister_mouvements = mouvements_possibles ;
    dire = dire ;
    sauver_position = sauver_position ;
    quitter = quitter
  }
;;
