let solve g =
  let (sr, sc) = Grid.find_char g 'S' in
  let (er, ec) = Grid.find_char g 'E' in

  let rows = Grid.height g in
  let cols = Grid.width g in
  let visited = Array.make_matrix rows cols false in

  let rec fn queue =
    match queue with
    | [] -> None (* plus de chemin possible *)
    | current_path :: rest ->
        let (r, c) = List.hd current_path in (*on regarde la derniere case atteinte *)
        
        if (r, c) = (er, ec) then
          Some (List.rev current_path) (* trouvé ! on retourne le chemin dans le bon sens *)
        else
          let neighbors = Grid.get_neighbors g r c in
          let new_neighbors = List.filter (fun (nr, nc) -> not visited.(nr).(nc)) neighbors in
          
          List.iter (fun (nr, nc) -> visited.(nr).(nc) <- true) new_neighbors;

          (*pour chaque voisin on cree un nouveau chemin en l ajoutant devant *)
          let new_paths = List.map (fun (nr, nc) -> 
            let mur_r = (r + nr) / 2 in
            let mur_c = (c + nc) / 2 in
            (nr, nc) :: (mur_r, mur_c) :: current_path
          ) new_neighbors in
          
          fn (rest @ new_paths)
  in

  visited.(sr).(sc) <- true;
  (* on commence avec un chemin qui contient que le depart *)
  fn [ [(sr, sc)] ]