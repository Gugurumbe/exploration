#use "graphe.ml" ;;
#use "monde.ml" ;;
#use "jeu.ml" ;;

let continuer = ref true ;;

let options = ref [] ;;

let commande = ref "" ;;

let rec traiter_commande texte opt =
  match opt with
  |[] -> print_endline "Je n'ai pas compris." 
  |(id_destination, id_porte)::_ when monde.portes.(id_porte).invocation = texte ->
    sauter_en id_destination
  |_::t -> traiter_commande texte t
;;

while !continuer do
  print_string "\t" ;
  print_endline (nom_courant ()) ;
  print_endline (description_courante ()) ;
  options := mouvements_possibles () ;
  List.iter (fun (id_voisin, id_porte) -> 
    print_string monde.portes.(id_porte).invocation ;
    print_string " : aller à " ;
    print_string monde.zones.(id_voisin).nom ;
    print_string " (" ;
    print_float monde.portes.(id_porte).delai ;
    print_endline ")") !options ;
  commande := read_line () ;
  traiter_commande !commande !options ;
done ;;

quitter () ;;
