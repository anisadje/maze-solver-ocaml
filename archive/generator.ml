
(* creer la structure de base avec murs fermes et +/- *)
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

(* choisir coordonnee au hasard *)
let choisir_random ligs cols =
  let l = (Random.int ligs) * 2 + 1 in
  let c = (Random.int cols) * 2 + 1 in
  (l, c)

(* convertit le char array array en Grid.t (c'est un string array)*)
let to_grid_t g_chars =
  Array.map (fun row ->
    let s = Bytes.create (Array.length row) in
    Array.iteri (fun i c -> Bytes.set s i c) row;
    Bytes.to_string s
  ) g_chars

(*la fn principale*)

let genere ligs cols densite =
  Random.self_init ();

  (*creation structure vide*)
  let g = init_struct ligs cols in

  (*creuser des murs selon la densite*)
  for i = 1 to ligs do
    for j = 1 to cols do
      let r = Random.float 1.0 in
      if r < densite then begin
        if Random.bool () then (
          if i < ligs then g.(2 * i).(2 * j - 1) <- ' ' (*horizontal *)
        ) else (
          if j < cols then g.(2 * i - 1).(2 * j) <- ' ' (*vertical *)
        )
      end
    done
  done;

  (* placement aleatoire de S *)
  let (s_l, s_c) = choisir_random ligs cols in
  g.(s_l).(s_c) <- 'S';

  (* placement de E different de S*)
  let rec placer_e () =
    let (e_l, e_c) = choisir_random ligs cols in
    if (e_l, e_c) = (s_l, s_c) then placer_e () (*  recommence si c'est la mm case *)
    else g.(e_l).(e_c) <- 'E'
  in
  placer_e ();

  (* conversion*)
  to_grid_t g