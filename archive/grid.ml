(* Le type t représente le labyrinthe.
Nous utilisons un tableau de chaînes de caractères pour stocker l'ASCII-art.*)

type t = string array 

(* --- FONCTIONS DE VERIFICATION ---*)

(* Vérifie si un caractère est autorisé dans le fichier .laby *)
let is_valid_char c = 
  match c with 
  | '+' | '-' | '|' | 'S' | 'E' | ' ' -> true 
  | _ -> false 

(* Vérifie que toutes les lignes ont la même longueur, i.e une grille rectangulaire *)

let check_dimensions lines = 
  match lines with 
  | [] -> true 
  | head :: tail -> 
    let width = String.length head in 
    List.for_all (fun s -> String.length s = width) tail 

(* Vérifie que les bords extérieurs du labyrinthe sont bien des murs (non vides)*)

let check_borders g = 
  let h = Array.length g in 
  let w = String.length g.(0) in 
  let valid = ref true in 

  (*On vérifie des lignes hautes et basses*)
  for j = 0 to w - 1 do 
    if g.(0).[j] = ' ' || g.(h-1).[j] = ' ' then valid := false 
    done; 
  (*On vérifi colonnes gauches et droites*)
  for i = 0 to h - 1 do 
    if g.(i).[0] = ' ' || g.(i).[w-1] = ' ' then valid := false
    done; 
    !valid 

(* --- FONCTIONS PRINCIPALES --- *)

let from_file c_in = 
  let rec read_all_lines acc = 
    try 
      let line = input_line c_in in 
      (* on vérifie les caractères valides *)
      String.iter (fun c -> 
        if not (is_valid_char c) 
          then failwith "Erreur : il y a un caractère invalide dans le fichier"
        ) line; 
        read_all_lines (line :: acc)
    with End_of_file -> List.rev acc
  in

  let lines = read_all_lines [] in 

  (* On vérifie que le labyrinthe non vide *)
  if lines = [] then failwith "Erreur : le fichier est vide";

  (* On vérifie si les dimensions sont correctes *)
  if not (check_dimensions lines) then failwith "Erreur : le labyrinthe n'est pas rectangulaire";

  let g = Array.of_list lines in 

  (* On vérifie les murs extérieurs *)
  if not (check_borders g) then failwith "Erreur : le labyrinthe n'est pas fermé sur les bords";

  (* Vérification : présence de S et E *)
  let s = ref false in 
  let e = ref false in 
  Array.iter (fun row -> 
    String.iter (fun c -> 
      if c = 'S' then s := true;
      if c = 'E' then e := true;
      ) row
  )g;

  if not !s then failwith "Erreur : Point de départ 'S' manquant";
  if not !e then failwith "Erreur : Point d'arrivée 'E' manquant";

  g 

(* Affiche le labyrinthe*)
let print g = 
  Array.iter (fun line -> print_endline line) g

(* Accès aux dimensions *)
let height g = Array.length g 
let width g = if Array.length g = 0 then 0 else String.length g.(0)

(* récupérer le caractère à une coordonnée (ligne, colonne) *)

let get g l c = g.(l).[c]

(*Fonction utilitaire pour savoir si une coordonnée est dans la grille*)

let is_inside g l c = 
  l >= 0 && l < height g && c >= 0 && c < width gauches

(*On renvoie la liste des voisins accessibles depuis (l, c)*)

let get_neighbors g l c =
  let neighbors = ref [] in 
  (*Si au-dessus de nous (l-1) ce n'est pas un mur*)
  if is_inside g (l-1) c && g.(l-1).[c] <> '-' then
    neighbors := (l-2, c) :: !neighbors;

  (*Si en-dessous (l+1) ce n'est pas un mur '-' *)
  if is_inside g (l+1) c && g.(l+1).[c] <> '-' then 
    neighbors := (l+2, c) :: !neighbors;

  (*Si à droite (c+1) ce n'est pas un mur '|' *)
  if is_inside g l (c+1) && g.(l).[c+1] <> '|' then 
    neighbors := (l, c+2) :: !neighbors;

  (*Si à gauche (c-1) ce n'est pas un mur '|' *)
  if is_inside g l (c-1) && g.(l).[c-1] <> '|' then 
    neighbors := (l, c-2) :: !neighbors;

  !neighbors

(*On cherche S et E*)
let find_char g target = 
  let pos = ref None in
  for l = 0 to (height g) - 1 do 
    for c = 0 to (width g) - 1 do 
      if g.(l).[c] = target then pos := Some (l, c)
    done 
  done
  match !pos with 
  | Some p -> p
  | None -> failwith "Caractère introuvable"

(*Il n'y a pas précisé dans l'énoncé que l'on doit vérifier que 
le départ S est avant l'arrivée, on peut trouver un chemin en partant de S
même si il est plus pas que E*)





