open Printf

(* [dump_file] lit toutes les lignes d'un fichier ouvert en lecture (in_channel)
   et les affiche dans la console jusqu'à la fin du fichier. *)
let dump_file (c_in : in_channel) : unit =
  try
    while true do
      let line = input_line c_in in  (* Lit la prochaine ligne *)
      printf "%s\n" line             (* Affiche la ligne, suivie d'un retour chariot *)
    done
  with
  | End_of_file -> () (* La boucle s'arrête lorsque la fin du fichier est atteinte  *)
  | Sys_error msg ->
      eprintf "Erreur de lecture: %s\n" msg

let main () = 
  let args = Sys.argv in 
  if Array.length args < 2 then 
    printf " Usage : %s [print|dump|solve|random] <args...>\n" args.(0)
  else 
    let action = args.(1) in 
    
    match action with 
    | "random" ->
        (* ici on ne touche pas à open_in *)
        if Array.length args < 4 then
          printf "Usage: %s random <ligs> <cols> <densite>\n" args.(0)
        else
          let l = int_of_string args.(2) in
          let c = int_of_string args.(3) in
          let d = float_of_string args.(4) in
          let g = Generator.genere l c d in
          Grid.print g

    | "print" | "dump" | "solve" ->
        (* ici on a besoin d'un fichier *)
        if Array.length args < 3 then
          printf "Usage: %s %s <fichier.laby>\n" args.(0) action
        else
          let filename = args.(2) in
          let ic = open_in filename in
          begin match action with
          | "dump" -> dump_file ic
          | "print" -> let g = Grid.from_file ic in Grid.print g
          | "solve" ->
            let g = Grid.from_file ic in
            close_in ic;
            begin match Solver.solve g with
            | None -> 
                print_endline "Aucun chemin trouvé."
            | Some path -> 
                print_endline "Chemin trouvé :";
               Grid.print_with_path g path
            end
          | _ -> ()
          end;
          close_in ic

    | _ -> printf "Action inconnue\n"

let () = main ()
