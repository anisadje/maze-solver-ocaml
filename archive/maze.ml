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
  if Array.length args < 3 then 
    printf " Usage : %s [print|dump|solve|generate] <fichier.laby>\n" args.(0)
else 
  let action = args.(1) in 
  let filename = args.(2) in 
  let ic = open_in filename in 

  match action with 
  | "dump" -> 
    dump_file ic; 
    close_in ic
  | "print" -> 
    let g = Grid.from_file ic in 
    close_in ic; 
    Grid.print g 
  | "solve" ->
    let g = Grid.from_file ic in
    close_in ic;
    if Solver.solve g then
      print_endline "Chemin trouvé"
    else
      print_endline "Aucun chemin" 
  | "generate" ->
    close_in ic;
    let g = Generator.generate 20 30 0.5 in
    Grid.print g
  | _ -> 
    printf "Action inconnue\n";
    close_in ic

let () = main ()

