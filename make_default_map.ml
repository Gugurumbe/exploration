#use "graphe.ml" ;;
#use "monde.ml" ;;

let impasse =
  {
    numero_unique = 0 ;
    nom = "Impasse des Savates jaunes" ;
    description = "Vous vous trouvez dans la fameuse impasse des Savates jaunes. \nAu nord se trouve la rue du majoral F. Brige." ;
  };;

let rue =
  { 
    numero_unique = 1 ;
    nom = "Rue du majoral F. Brige (3)" ;
    description = "Vous vous trouvez dans la non moins fameuse rue du majoral Brige. \nAu sud, se trouve l'impasse des Savates jaunes.\nÀ l'est, la rue continue (4), mais un molosse rébarbatif vous dévisage avec de la bave aux lèvres.\nÀ l'ouest, la rue continue (2), mais un groupe de personnes armées semble mener des transactions, nécessitant le moins de témoins possibles. Ne vous approchez pas !"
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

let structure = creer_nuage [|0 ; 1|] ;;
let zones = [|impasse ; rue|] ;;
let portes = [|entree_impasse ; sortie_impasse|] ;;
relier structure 0 1 1 ;;
relier structure 1 0 0 ;;

let carte =
{
  structure = structure ;
  zones = zones ;
  portes = portes
} ;;

let fichier = open_out_bin "default_map" ;;
sauver_carte fichier carte ;;
close_out fichier ;;
