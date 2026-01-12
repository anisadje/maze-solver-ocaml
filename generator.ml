
(* 1. Créer la structure de base avec murs fermés et +/- *)
(* On utilise ligs et cols pour calculer la taille réelle de la grille ASCII *)
let init_struct ligs cols =
  let h = 2 * ligs + 1 in
  let w = 2 * cols + 1 in
  Array.init h (fun i ->
    Array.init w (fun j ->
      if i mod 2 = 0 then
        if j mod 2 = 0 then '+' else '-'
      else
        if j mod 2 = 0 then '|' else ' '
    )
  )

(* 2. Choisir une coordonnée au hasard parmi les cases "pièces" (indices impairs) *)
let choisir_random ligs cols =
  let l = (Random.int ligs) * 2 + 1 in
  let c = (Random.int cols) * 2 + 1 in
  (l, c)

(* 3. Convertit le char array array en Grid.t (qui est un string array) *)
(* Utilisation de Bytes -> efficacité *)
let to_grid_t g_chars =
  Array.map (fun row ->
    let s = Bytes.create (Array.length row) in
    Array.iteri (fun i c -> Bytes.set s i c) row;
    Bytes.to_string s
  ) g_chars

(* 4. La fonction principale de génération *)
let genere ligs cols densite =
  (* Initialisation de la graine aléatoire *)
  Random.self_init ();

  (* Création de la structure vide (tous les murs fermés) *)
  let g = init_struct ligs cols in

  (* 5. Creuser des murs selon la densité *)
  for i = 1 to ligs do
    for j = 1 to cols do
      let r = Random.float 1.0 in
      if r < densite then begin
        if Random.bool () then (
          (* On creuse horizontalement si on n'est pas sur le bord bas *)
          if i < ligs then g.(2 * i).(2 * j - 1) <- ' ' 
        ) else (
          (* On creuse verticalement si on n'est pas sur le bord droit *)
          if j < cols then g.(2 * i - 1).(2 * j) <- ' ' 
        )
      end
    done
  done;

  (* 6. Placement aléatoire du point de départ 'S' *)
  let (s_l, s_c) = choisir_random ligs cols in
  g.(s_l).(s_c) <- 'S';

  (* 7. Placement de l'arrivée 'E' sur une case différente de 'S' *)
  let rec placer_e () =
    let (e_l, e_c) = choisir_random ligs cols in
    if (e_l, e_c) = (s_l, s_c) then placer_e () (* Recommence si c'est la même case *)
    else g.(e_l).(e_c) <- 'E'
  in
  placer_e ();

  (* 8. Conversion finale vers le type abstrait de Grid *)
  to_grid_t g