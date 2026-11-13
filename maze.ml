open Printf

(* Prints the content of the file line by line, without checking it *)
let dump_file (ic : in_channel) : unit =
  try
    while true do
      let line = input_line ic in
      printf "%s\n" line
    done
  with
  | End_of_file -> ()
  | Sys_error msg ->
      eprintf "Read error: %s\n" msg

let main () =
  let args = Sys.argv in
  if Array.length args < 2 then
    printf "Usage: %s [print|dump|solve|stats|random] <args...>\n" args.(0)
  else
    let action = args.(1) in

    match action with
    | "random" ->
        (* no input file here *)
        if Array.length args < 5 then
          printf "Usage: %s random <rows> <cols> <density>\n" args.(0)
        else
          begin match int_of_string_opt args.(2),
                      int_of_string_opt args.(3),
                      float_of_string_opt args.(4) with
          (* we need at least 2 rooms, otherwise S and E can't be placed *)
          | Some l, Some c, Some d when l > 0 && c > 0 && l * c >= 2 ->
              let g = Generator.generate l c d in
              Grid.print g
          | _ ->
              eprintf "Error: <rows> and <cols> must be positive integers (at least 2 rooms), <density> a number\n";
              exit 1
          end

    | "print" | "dump" | "solve" ->
        if Array.length args < 3 then
          printf "Usage: %s %s <file.laby>\n" args.(0) action
        else
          let filename = args.(2) in
          let ic = open_in filename in
          begin match action with
          | "dump" -> dump_file ic
          | "print" -> let g = Grid.from_file ic in Grid.print g
          | "solve" ->
              let g = Grid.from_file ic in
              begin match Solver.solve g with
              | None ->
                  print_endline "No path found."
              | Some path ->
                  print_endline "Path found:";
                  Grid.print_with_path g path
              end
          | _ -> ()
          end;
          close_in ic

    | "stats" ->
        if Array.length args < 3 then
          printf "Usage: %s stats <file.laby>\n" args.(0)
        else
          let ic = open_in args.(2) in
          let g = Grid.from_file ic in
          close_in ic;
          let (r, c, w, p) = Grid.get_stats g in
          let total = r * c in
          printf "--- Maze statistics ---\n";
          printf "Size     : %d rows x %d columns\n" r c;
          printf "Walls    : %d\n" w;
          printf "Passages : %d\n" p;
          if total > 0 then
            printf "Density  : %.1f%% walls\n" (float_of_int w /. float_of_int total *. 100.)

    | _ -> printf "Unknown action\n"

let () =
  try main () with
  | Sys_error msg ->
      eprintf "Error: %s\n" msg;
      exit 1
  | Failure msg ->
      eprintf "%s\n" msg;
      exit 1
