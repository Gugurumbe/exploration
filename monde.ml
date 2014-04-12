(*open Graphe ;;*)

type zone =
{
  numero_unique : int ;
  nom : string ;
  description : string
} ;;

type porte =
{
  invocation : string ;
  delai : float ;
} ;;

type monde = 
{
  structure : (int, int) graphe ;
  zones : zone array ;
  portes : porte array ;
};;

let init_string taille f =
  let s = String.make taille '0' in
  for i=0 to taille-1 do
    s.[i] <- f i
  done ;
  s
;;

let output_binary_string flux s =
  output_binary_int flux (String.length s) ;
  output_string flux s 
;;

let input_binary_string flux =
  let taille = input_binary_int flux in
  init_string taille (fun _ -> input_char flux)
;;

let output_float flux f =
  Marshal.to_channel flux f [Marshal.Compat_32]
;;

let input_float flux =
  let f = (Marshal.from_channel flux : float) in
  f
;;

let sauver_zone flux z =
  output_binary_int flux z.numero_unique ;
  output_binary_string flux z.nom ;
  output_binary_string flux z.description 
;;

let charger_zone flux =
  let id_unique = input_binary_int flux in
  let nom = input_binary_string flux in
  let description = input_binary_string flux in
  {
    numero_unique = id_unique ;
    nom = nom ;
    description = description
  }
;;

let sauver_porte flux p =
  output_binary_string flux p.invocation ;
  output_float flux p.delai
;;

let charger_porte flux =
  let invocation = input_binary_string flux in
  let delai = input_float flux in
  {
    invocation = invocation ;
    delai = delai
  }
;;

let sauver_carte flux carte =
  output_binary_int flux (Array.length carte.zones) ;
  Array.iter (sauver_zone flux) carte.zones ;
  output_binary_int flux (Array.length carte.portes) ;
  Array.iter (sauver_porte flux) carte.portes ;
  sauver_structure flux carte.structure (output_binary_int) ;
;;

let charger_carte flux =
  let n = input_binary_int flux in
  let zones = Array.init n (fun _ -> charger_zone flux) in
  let p = input_binary_int flux in
  let portes = Array.init p (fun _ -> charger_porte flux) in
  let structure = charger_structure flux (Array.map (fun z -> z.numero_unique) zones) (input_binary_int) in
  {
    structure = structure ;
    zones = zones ;
    portes = portes
  }
;;
    
