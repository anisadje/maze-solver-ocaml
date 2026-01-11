(* Ce fichier doit contenir l'algorithme pour résoudre le labyrinthe *)

open Grid

let solve g =
  let (sr,sc) = Grid.find_char g 'S' in
  let (er,ec) = Grid.find_char g 'E' in

  let rows = Array.length g in
  let cols = Array.length g.(0) in

  let visited = Array.make_matrix rows cols false in

  let rec fn queue =
    match queue with
    [] -> false
    |(r, c) :: rest ->
        if (r, c) = (er, ec) then
          true
        else begin
          let neighbors = Grid.get_neighbors g r c in
          let new_cells =
            List.filter
              (fun (nr, nc) -> not visited.(nr).(nc))
              neighbors
          in
          List.iter
            (fun (nr, nc) -> visited.(nr).(nc) <- true)
            new_cells;
          fn (rest @ new_cells)
        end
  in

  visited.(sr).(sc) <- true;
  fn [ (sr, sc) ]
