#use "graphe.ml" ;;
#use "monde.ml" ;;

let impasse =
  {
    numero_unique = 0 ;
    nom = "Impasse des Savates jaunes" ;
    description = "Vous vous trouvez dans la fameuse impasse des Savates jaunes. \nAu nord se trouve la rue du majoral F. Brige." ;
  };;

let rue_3 =
  { 
    numero_unique = 1 ;
    nom = "Rue du majoral F. Brige (3)" ;
    description = "Vous vous trouvez dans la rue du majoral Brige, devant le numéro 3.\nAu sud, se trouve l'impasse des Savates jaunes.\nÀ l'est, la rue continue (5), mais un molosse rébarbatif vous dévisage avec de la bave aux lèvres.\nÀ l'ouest, la rue continue (1).\nAu nord se trouve le Café Jaune. Fermé à cette heure-ci."
  } ;;

let rue_1 =
  {
    numero_unique = 2 ;
    nom = "Rue du majoral F. Brige (1)" ;
    description = "Vous vous trouvez dans la rue du majoral Félix Brige, devant le numéro 1.\nAu sud, le numéro 2.\nAu nord, le numéro 1.\nÀ l'ouest, le boulevard J. Boudin, mais l'accès y est interdit.\nÀ l'est, la rue F. Brige, numéro 3." 
  } ;;

let entree_impasse = 
  {
    invocation = "sud" ;
    delai = 1.0
  } ;;

let sortie_impasse =
  {
    invocation = "nord" ;
    delai = 1.0
  } ;;

let rue_31 =
  {
    invocation = "ouest" ;
    delai = 1.0
  } ;;

let rue_13 =
  {
    invocation = "est" ;
    delai = 1.0 
  } ;;

let structure = creer_nuage [|0 ; 1 ; 2|] ;;
let zones = [|impasse ; rue_3 ; rue_1|] ;;
let portes = [|entree_impasse ; sortie_impasse ; rue_31 ; rue_13|] ;;
relier structure 0 1 1 ;;
relier structure 1 0 0 ;;
relier structure 1 2 2 ;;
relier structure 2 1 3 ;;

let carte =
{
  structure = structure ;
  zones = zones ;
  portes = portes
} ;;

let fichier = open_out_bin "default_map" ;;
sauver_carte fichier carte ;;
close_out fichier ;;
