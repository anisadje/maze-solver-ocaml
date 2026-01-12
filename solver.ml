(** Cherche le chemin le plus court entre 'S' et 'E' dans la grille g.
    L'algorithme utilise un parcours en largeur (BFS) pour garantir l'optimalité. *)
let solve g =
  (* 1. Initialisation des points de départ (S) et d'arrivée (E) *)
  let (sr, sc) = Grid.find_char g 'S' in
  let (er, ec) = Grid.find_char g 'E' in

  (* 2. Préparation des structures de données pour le parcours *)
  let rows = Grid.height g in
  let cols = Grid.width g in
  (* visited permet d'éviter de repasser par la même case (évite les boucles infinies) *)
  let visited = Array.make_matrix rows cols false in

  (* 3. Utilisation d'une file (Queue) pour stocker les chemins à explorer.
     Le BFS (parcours en largeur) nécessite une structure FIFO pour explorer 
     les cases par "couches" successives. *)
  let q = Queue.create () in

  (* 4. Fonction récursive de parcours *)
  let rec fn () =
    if Queue.is_empty q then 
      None (* Cas d'échec : tous les chemins possibles ont été explorés sans trouver 'E' *)
    else
      (* On récupère le chemin le plus ancien dans la file (FIFO) *)
      let current_path = Queue.take q in
      let (r, c) = List.hd current_path in (* On regarde la dernière case atteinte dans ce chemin *)
      
      (* 5. Test de victoire *)
      if (r, c) = (er, ec) then
        Some (List.rev current_path) (* Succès : on retourne le chemin remis dans l'ordre (S vers E) *)
      else
        begin
          (* 6. Exploration des voisins accessibles *)
          let neighbors = Grid.get_neighbors g r c in
          
          (* On ne garde que les voisins qui n'ont pas encore été visités *)
          let new_neighbors = List.filter (fun (nr, nc) -> not visited.(nr).(nc)) neighbors in
          
          (* 7. Mise à jour de la file et marquage des visites *)
          List.iter (fun (nr, nc) -> 
            visited.(nr).(nc) <- true;
            
            (* Calcul de la coordonnée du mur entre la case actuelle et le voisin.
               Indispensable pour l'affichage avec des points '.' sur les murs ouverts. *)
            let mur_r = (r + nr) / 2 in
            let mur_c = (c + nc) / 2 in
            
            (* On crée le nouveau chemin : on ajoute le mur PUIS le voisin au chemin actuel *)
            let new_path = (nr, nc) :: (mur_r, mur_c) :: current_path in
            Queue.add new_path q
          ) new_neighbors;
          
          (* Appel récursif terminal pour traiter le prochain chemin de la file *)
          fn () 
        end
  in

  (* 8. Lancement de l'algorithme *)
  visited.(sr).(sc) <- true;
  (* On commence avec un chemin contenant uniquement les coordonnées de départ *)
  Queue.add [(sr, sc)] q;
  fn ()