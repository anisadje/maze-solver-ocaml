(* Ce fichier doit être dédié à la création de labyrinthe aléatoires pour en pas 
encombrer maze.ml*)

open Grid

(*creeer une grille remplie de murs *)
let creer_murs ligs cols =
  Array.make_matrix ligs cols '#'

(*creuse aleatoirement des chemins *)
let creuser g densite =
  let ligs = Array.length g in
  let cols = Array.length g.(0) in

  for i = 0 to ligs - 1 do
    for j = 0 to cols - 1 do
      if Random.float 1.0 < densite then
        g.(i).(j) <- ' '
    done
  done

(* place S et E sur des cases libres*)
let place_s_e g =
  let ligs = Array.length g in
  let cols = Array.length g.(0) in

  let libre_random () =
    let rec aux () =
      let l = Random.int ligs in
      let c = Random.int cols in
      if g.(l).(c) = ' ' then (l, c)
      else aux ()
    in aux ()
  in

  let (sl, sc) = libre_random () in
  let (el, ec) = libre_random () in

  g.(sl).(sc) <- 'S';
  g.(el).(ec) <- 'E'

(* fonction principale appelee sur maze.ml *)
let genere ligs cols densite  =
  Random.self_init ();
  let g = creer_murs ligs cols in
  creuser g densite;
  place_s_e g;
  g

