type t = string array

(* --- FONCTIONS DE VERIFICATION --- *)

let is_valid_char c = 
  match c with 
  | '+' | '-' | '|' | 'S' | 'E' | ' ' -> true 
  | _ -> false 

(** Vérifie la validité complète du labyrinthe et renvoie les positions S et E *)
let validate g =
  let h = Array.length g in
  if h = 0 then failwith "Erreur : le labyrinthe est vide";
  let w = String.length g.(0) in
  let s_found = ref None in
  let e_found = ref None in

  for i = 0 to h - 1 do
    (* 1. Vérification des dimensions (grille rectangulaire) *)
    if String.length g.(i) <> w then 
      failwith (Printf.sprintf "Erreur : ligne %d de longueur incorrecte" i);
    
    for j = 0 to w - 1 do
      let c = g.(i).[j] in
      (* 2. Vérification des caractères autorisés *)
      if not (is_valid_char c) then 
        failwith (Printf.sprintf "Erreur : caractère '%c' invalide à (%d,%d)" c i j);
      
      (* 3. Vérification des bords extérieurs (doivent être des murs) *)
      if (i = 0 || i = h - 1 || j = 0 || j = w - 1) && c = ' ' then
        failwith "Erreur : le labyrinthe n'est pas fermé sur les bords";
      
      (* 4. Repérage de S et E *)
      if c = 'S' then s_found := Some (i, j);
      if c = 'E' then e_found := Some (i, j)
    done
  done;

  (* 5. Vérification de la présence de S et E *)
  match !s_found, !e_found with
  | None, _ -> failwith "Erreur : Point de départ 'S' manquant"
  | _, None -> failwith "Erreur : Point d'arrivée 'E' manquant"
  | Some s, Some e -> (s, e)

(* --- FONCTIONS PRINCIPALES --- *)

let from_file c_in = 
  let rec read_all_lines acc = 
    try 
      let line = input_line c_in in 
      read_all_lines (line :: acc)
    with End_of_file -> List.rev acc
  in
  let lines = read_all_lines [] in 
  let g = Array.of_list lines in
  (* On valide tout d'un coup *)
  let _ = validate g in 
  g 

let print g = 
  Array.iter print_endline g

let height g = Array.length g 

let width g = 
  if Array.length g = 0 then 0 else String.length g.(0)

let get g l c = g.(l).[c]

let is_inside g l c = 
  l >= 0 && l < height g && c >= 0 && c < width g

let get_neighbors g l c =
  let neighbors = ref [] in
  (* Les directions : (delta_mur_l, delta_mur_c, delta_voisin_l, delta_voisin_c) *)
  let directions = [
    (-1, 0, -2, 0); (* Haut *)
    (1, 0, 2, 0);  (* Bas *)
    (0, 1, 0, 2);  (* Droite *)
    (0, -1, 0, -2) (* Gauche *)
  ] in
  
  List.iter (fun (dl_w, dc_w, dl_v, dc_v) ->
    let wl, wc = l + dl_w, c + dc_w in
    let vl, vc = l + dl_v, c + dc_v in
    if is_inside g wl wc && is_inside g vl vc then
      let wall = g.(wl).[wc] in
      (* On peut passer si ce n'est pas un mur '-' ou '|' *)
      if wall <> '-' && wall <> '|' then
        neighbors := (vl, vc) :: !neighbors
  ) directions;
  !neighbors

let find_char g target = 
  let h = height g in
  let w = width g in
  let res = ref None in
  for l = 0 to h - 1 do 
    for c = 0 to w - 1 do 
      if g.(l).[c] = target then res := Some (l, c)
    done 
  done;
  match !res with 
  | Some p -> p
  | None -> failwith (Printf.sprintf "Caractère '%c' introuvable" target)

let print_with_path g path =
  let h = height g in
  (* On utilise Array.init et Bytes.of_string pour la modification en place *)
  let display = Array.init h (fun i -> Bytes.of_string g.(i)) in
  
  List.iter (fun (r, c) ->
    if is_inside g r c && Bytes.get display.(r) c = ' ' then 
      Bytes.set display.(r) c '.'
  ) path;

  Array.iter (fun b -> 
    print_endline (Bytes.to_string b)
  ) display

let get_stats g =
  let rows = Array.length g in
  let cols = if rows > 0 then String.length g.(0) else 0 in
  let walls = ref 0 in
  let paths = ref 0 in
  Array.iter (fun row ->
    String.iter (fun c ->
      if c = '+' || c = '-' || c = '|' then incr walls
      else if c = ' ' || c = 'S' || c = 'E' || c = '.' then incr paths
    ) row
  ) g;
  (rows, cols, !walls, !paths)