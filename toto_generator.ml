Random.self_init () ;;

let consonnes = [| 
  "b" ;
  "c" ;
  "d" ;
  "f" ; 
  "g" ; 
  "j" ; 
  "l" ;
  "m" ;
  "n" ;
  "p" ;
  "r" ;
  "s" ;
  "t" ;
  "v" |] ;;

let voyelles = [| "a" ; "e" ; "i" ; "o" |] ;;

let syllabes =
  Array.concat (Array.to_list (Array.init (Array.length consonnes) (fun i -> Array.init (Array.length voyelles) (fun j -> consonnes.(i)^voyelles.(j))))) ;;

let syllabes = 
  [|"ba"; "be"; "bi"; "bo"; "bu"; "ca"; "co"; "da"; "de"; "di"; "do";
    "fa"; "fe"; "fi"; "fo"; "ga"; "go"; "ja"; "je"; "ji"; "jo";
    "la"; "le"; "li"; "lo"; "ma"; "me"; "mi"; "mo"; "na"; "ne"; "ni"; "no";
    "pa"; "pe"; "pi"; "po"; "ra"; "re"; "ri"; "ro"; "sa"; "se"; "si"; "so";
    "ta"; "te"; "ti"; "to"; "va"; "ve"; "vi"; "vo"|] ;;
    
let mot () =
  let s = Array.init (2 + Random.int 5) (fun _ -> syllabes.(Random.int (Array.length syllabes))) in
  String.concat "" (Array.to_list s) 
;;
