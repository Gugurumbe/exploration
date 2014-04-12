#use "graphe.ml" ;;
#use "monde.ml" ;;

let fichier = open_in_bin "default_map" ;;
let monde = charger_carte fichier ;;
close_in fichier ;;

let position_utilisateur = ref 0 ;;

let nom_courant () =
  monde.zones.(!position_utilisateur).nom 
;;

let description_courante () =
  monde.zones.(!position_utilisateur).description
;;

try
  let fichier_utilisateur = open_in_bin "user" in
  position_utilisateur := input_binary_int fichier_utilisateur ;
  close_in fichier_utilisateur 
with
| Sys_error("user: No such file or directory") -> ()
;;

let mouvements_possibles () =
  let liste_fils = monde.structure.structure.(!position_utilisateur).fils in
  List.map (fun (n_n_voisin, n_porte) -> (n_porte, monde.structure.noeuds.(n_n_voisin))) liste_fils
;;

let sauter_en pos = position_utilisateur := pos ;;

let quitter () =
  let fichier = open_out_bin "user" in
  output_binary_int fichier (!position_utilisateur) ;
  flush fichier ;
  close_out fichier ;
  let fichier = open_out_bin "default_map" in
  sauver_carte fichier monde ;
  flush fichier ;
  close_out fichier
;;
