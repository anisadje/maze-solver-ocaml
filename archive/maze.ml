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

