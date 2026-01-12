(** Type abstrait représentant le labyrinthe. 
    L'implémentation interne est cachée pour garantir l'intégrité des données. *)
type t = string array

(** [from_file channel] lit un labyrinthe depuis un descripteur d'entrée.
    Vérifie la validité des dimensions, des caractères, des bords et la présence de S et E.
    @raise Failure si le labyrinthe est invalide ou vide. *)
val from_file : in_channel -> t

(** [print g] affiche le labyrinthe dans la console. *)
val print : t -> unit

(** [height g] renvoie la hauteur (nombre de lignes) du labyrinthe. *)
val height : t -> int

(** [width g] renvoie la largeur (nombre de colonnes) du labyrinthe. *)
val width : t -> int

(** [get g row col] renvoie le caractère à la position (row, col). *)
val get : t -> int -> int -> char

(** [get_neighbors g row col] renvoie la liste des coordonnées des pièces 
    accessibles depuis la pièce située en (row, col) en vérifiant l'absence de murs. *)
val get_neighbors : t -> int -> int -> (int * int) list

(** [find_char g c] cherche la position (ligne, colonne) de la première occurrence 
    du caractère [c] (typiquement 'S' ou 'E').
    @raise Failure si le caractère n'est pas trouvé. *)
val find_char : t -> char -> (int * int)

(** [print_with_path g path] affiche le labyrinthe en marquant les étapes du 
    chemin [path] par des points ('.'). *)
val print_with_path : t -> (int * int) list -> unit

(** [get_stats g] analyse la grille [g] et retourne un quadruplet 
    (lignes, colonnes, nb_murs, nb_passages). *)
val get_stats : t -> int * int * int * int