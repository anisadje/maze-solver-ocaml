(** A maze is stored as an array of lines, one string per line of the file. *)
type t = string array

(** [from_file ic] reads a maze from [ic] and checks that it is valid
    (rectangular, allowed characters only, closed borders, one S and one E).
    @raise Failure if the maze is empty or invalid. *)
val from_file : in_channel -> t

(** [print g] prints the maze. *)
val print : t -> unit

(** [height g] is the number of lines of the maze. *)
val height : t -> int

(** [width g] is the number of columns of the maze. *)
val width : t -> int

(** [get g row col] is the character at position (row, col). *)
val get : t -> int -> int -> char

(** [get_neighbors g row col] is the list of rooms reachable from the room
    at (row, col), i.e. the ones not separated from it by a wall. *)
val get_neighbors : t -> int -> int -> (int * int) list

(** [find_char g c] is the position (row, col) of the character [c]
    (usually 'S' or 'E'). If [c] appears several times, the last one is returned.
    @raise Failure if [c] is not in the maze. *)
val find_char : t -> char -> int * int

(** [print_with_path g path] prints the maze with the cells of [path]
    marked with dots. *)
val print_with_path : t -> (int * int) list -> unit

(** [get_stats g] returns (rows, columns, number of walls, number of passages). *)
val get_stats : t -> int * int * int * int
