#use "graphe.ml" ;;
#use "monde.ml" ;;
#use "jeu.ml" ;;
#load "unix.cma" ;;

let continuer = ref true ;;

let commande = ref "" ;;

let options = ref [] ;;

let jeu = init "default_map" "user" ;;

let sleep delai = 
  ignore (Unix.select [] [] [] delai)
;;

while !continuer do
  print_string "\t" ;
  print_endline (jeu.nom_zone ()) ;
  print_endline (jeu.description ()) ;
  options := jeu.lister_mouvements () ;
  List.iter (fun (invocation, voisin, delai) -> 
    print_string invocation ;
    print_string " : aller à " ;
    print_string voisin ;
    print_string " (" ;
    print_float delai ;
    print_endline " secondes)") !options ;
  commande := read_line () ;
  if !commande = "quit" || !commande = "exit" || !commande = "quitter" then continuer := false 
  else
    try
      sleep (jeu.dire !commande) 
    with
    | Mauvaise_invocation -> print_endline "Euuuh...?"
done ;;

jeu.quitter () ;;
