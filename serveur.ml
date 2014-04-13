#use "monde.ml" ;;
#use "jeu.ml" ;;
#load "unix.cma" ;;

let sleep = Unix.select [] [] [] ;;

let service jeu get_command out =
  let continuer = ref true in
  let commande = ref "" in
  let options = ref [] in
  while !continuer do
    output_string out "\t" ;
    output_string out (jeu.nom_zone ()) ;
    output_string out "\n" ;
    output_string out (jeu.description ()) ;
    output_string out "\n" ;
    options := jeu.lister_mouvements () ;
    List.iter (fun (invocation, voisin, delai) -> 
      output_string out invocation ;
      output_string out " : aller à " ;
      output_string out voisin ;
      output_string out " (" ;
      output_string out (string_of_float delai) ;
      output_string out " secondes)\n") !options ;
    flush out ;
    commande := get_command () ;
    if !commande = "quit" || !commande = "exit" || !commande = "quitter" then continuer := false 
    else
      try 
	ignore (sleep (jeu.dire !commande))
      with
      | Mauvaise_invocation -> output_string out "Euuuh...?\n" ;
  done ;
  jeu.quitter ()
;;

let lancer_service input output =
  let get_command () =
    String.trim (input_line input) 
  in
  output_string output "Donnez-moi le nom du monde sur lequel vous voulez jouer (exemple : default_map) : \n" ;
  flush output ;
  let nom_carte = get_command () in
  output_string output "Donnez-moi le nom du joueur que vous voulez incarner (exemple : user). En cas d'absence d'un tel joueur, je crée une nouvelle partie.\n" ;
  flush output ;
  let nom_utilisateur = get_command () in
  try 
    let jeu = init nom_carte nom_utilisateur in
    service jeu (get_command) output
  with
  | _ -> output_string output ("Impossible de trouver cette carte : \""^nom_carte^"\".\n") ;
;;

let port = 45678 ;;

Unix.establish_server (lancer_service) (Unix.ADDR_INET(Unix.inet_addr_loopback, port)) ;;
