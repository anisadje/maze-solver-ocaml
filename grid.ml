type t = string array

let is_valid_char c =
  match c with
  | '+' | '-' | '|' | 'S' | 'E' | ' ' -> true
  | _ -> false

(* Checks the whole maze and returns the positions of S and E *)
let validate g =
  let h = Array.length g in
  if h = 0 then failwith "Error: the maze is empty";
  let w = String.length g.(0) in
  let s_found = ref None in
  let e_found = ref None in

  for i = 0 to h - 1 do
    if String.length g.(i) <> w then
      failwith (Printf.sprintf "Error: line %d has a wrong length" i);

    for j = 0 to w - 1 do
      let c = g.(i).[j] in
      if not (is_valid_char c) then
        failwith (Printf.sprintf "Error: invalid character '%c' at (%d,%d)" c i j);

      (* the outer border must only contain walls *)
      if (i = 0 || i = h - 1 || j = 0 || j = w - 1) && c = ' ' then
        failwith "Error: the maze is not closed on its borders";

      if c = 'S' then s_found := Some (i, j);
      if c = 'E' then e_found := Some (i, j)
    done
  done;

  match !s_found, !e_found with
  | None, _ -> failwith "Error: missing start 'S'"
  | _, None -> failwith "Error: missing exit 'E'"
  | Some s, Some e -> (s, e)

let from_file ic =
  (* some files end with an empty line, we just ignore it *)
  let rec skip_empty = function
    | "" :: rest -> skip_empty rest
    | l -> l
  in
  let rec read_all_lines acc =
    try
      let line = input_line ic in
      read_all_lines (line :: acc)
    with End_of_file -> List.rev (skip_empty acc)
  in
  let lines = read_all_lines [] in
  let g = Array.of_list lines in
  let _ = validate g in
  g

let print g =
  Array.iter print_endline g

let height g = Array.length g

let width g =
  if Array.length g = 0 then 0 else String.length g.(0)

let get g l c = g.(l).[c]

let is_inside g l c =
  l >= 0 && l < height g && c >= 0 && c < width g

let get_neighbors g l c =
  let neighbors = ref [] in
  (* (wall_dl, wall_dc, next_dl, next_dc): the wall is 1 step away, the next room 2 steps *)
  let directions = [
    (-1, 0, -2, 0); (* up *)
    (1, 0, 2, 0);   (* down *)
    (0, 1, 0, 2);   (* right *)
    (0, -1, 0, -2)  (* left *)
  ] in

  List.iter (fun (dl_w, dc_w, dl_v, dc_v) ->
    let wl, wc = l + dl_w, c + dc_w in
    let vl, vc = l + dl_v, c + dc_v in
    if is_inside g wl wc && is_inside g vl vc then
      let wall = g.(wl).[wc] in
      if wall <> '-' && wall <> '|' then
        neighbors := (vl, vc) :: !neighbors
  ) directions;
  !neighbors

let find_char g target =
  let h = height g in
  let w = width g in
  let res = ref None in
  for l = 0 to h - 1 do
    for c = 0 to w - 1 do
      if g.(l).[c] = target then res := Some (l, c)
    done
  done;
  match !res with
  | Some p -> p
  | None -> failwith (Printf.sprintf "Character '%c' not found" target)

let print_with_path g path =
  let h = height g in
  (* strings are immutable, so we work on a copy made of bytes *)
  let display = Array.init h (fun i -> Bytes.of_string g.(i)) in

  List.iter (fun (r, c) ->
    if is_inside g r c && Bytes.get display.(r) c = ' ' then
      Bytes.set display.(r) c '.'
  ) path;

  Array.iter (fun b ->
    print_endline (Bytes.to_string b)
  ) display

let get_stats g =
  let rows = Array.length g in
  let cols = if rows > 0 then String.length g.(0) else 0 in
  let walls = ref 0 in
  let paths = ref 0 in
  Array.iter (fun row ->
    String.iter (fun c ->
      if c = '+' || c = '-' || c = '|' then incr walls
      else if c = ' ' || c = 'S' || c = 'E' || c = '.' then incr paths
    ) row
  ) g;
  (rows, cols, !walls, !paths)
