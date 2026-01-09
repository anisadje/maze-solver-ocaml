(* Ici sont les fonctions autorisées à être utilisé à l'extérieur du fichier grid.ml*)

(* Le type abstrait représentant un labyrinthe *)
type t

(* [from_file channel] lit un labyrinthe.
Lève une exception si le labyrinthe est invalide*)
val from_file : in_channel -> t 

(* [print g] affiche le labyrinthe *)
val print : t -> unit 

(* [height g] renvoie la hauteur du labyrinthe *)
val height : t -> int 

(* [width g] renvoie la largeur du labyrinthe *)
val width : t -> int 

(* [get g row col] renvoie le caractère à la position donnée *)
val get : t -> int -> char 

val get_neighbors : t -> int -> int -> (int * int) list
val find_char : t -> char -> (int * int)