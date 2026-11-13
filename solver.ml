(** Finds a shortest path from 'S' to 'E' in [g] with a BFS.
    Returns [None] if E cannot be reached. *)
let solve g =
  let (sr, sc) = Grid.find_char g 'S' in
  let (er, ec) = Grid.find_char g 'E' in

  let rows = Grid.height g in
  let cols = Grid.width g in
  let visited = Array.make_matrix rows cols false in

  (* each element of the queue is a path, stored in reverse (last cell first) *)
  let q = Queue.create () in

  let rec loop () =
    if Queue.is_empty q then
      None
    else
      let current_path = Queue.take q in
      let (r, c) = List.hd current_path in

      if (r, c) = (er, ec) then
        Some (List.rev current_path)
      else
        begin
          let neighbors = Grid.get_neighbors g r c in
          let new_neighbors = List.filter (fun (nr, nc) -> not visited.(nr).(nc)) neighbors in

          List.iter (fun (nr, nc) ->
            visited.(nr).(nc) <- true;

            (* we also add the opening between the two rooms,
               so that the path is drawn without gaps *)
            let wall_r = (r + nr) / 2 in
            let wall_c = (c + nc) / 2 in

            let new_path = (nr, nc) :: (wall_r, wall_c) :: current_path in
            Queue.add new_path q
          ) new_neighbors;

          loop ()
        end
  in

  visited.(sr).(sc) <- true;
  Queue.add [(sr, sc)] q;
  loop ()
