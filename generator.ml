(* Builds a rows x cols maze where every wall is closed.
   The ASCII grid is (2 * rows + 1) x (2 * cols + 1). *)
let init_struct rows cols =
  let h = 2 * rows + 1 in
  let w = 2 * cols + 1 in
  Array.init h (fun i ->
    Array.init w (fun j ->
      if i mod 2 = 0 then
        if j mod 2 = 0 then '+' else '-'
      else
        if j mod 2 = 0 then '|' else ' '
    )
  )

(* Picks a random room (rooms are at odd indices) *)
let random_room rows cols =
  let l = (Random.int rows) * 2 + 1 in
  let c = (Random.int cols) * 2 + 1 in
  (l, c)

(* char array array -> Grid.t *)
let to_grid_t g_chars =
  Array.map (fun row ->
    let s = Bytes.create (Array.length row) in
    Array.iteri (fun i c -> Bytes.set s i c) row;
    Bytes.to_string s
  ) g_chars

let generate rows cols density =
  Random.self_init ();

  let g = init_struct rows cols in

  (* for each room, with probability [density] we open one of its walls *)
  for i = 1 to rows do
    for j = 1 to cols do
      let r = Random.float 1.0 in
      if r < density then begin
        if Random.bool () then (
          (* wall below the room, except on the last row *)
          if i < rows then g.(2 * i).(2 * j - 1) <- ' '
        ) else (
          (* wall on the right of the room, except on the last column *)
          if j < cols then g.(2 * i - 1).(2 * j) <- ' '
        )
      end
    done
  done;

  let (s_l, s_c) = random_room rows cols in
  g.(s_l).(s_c) <- 'S';

  (* E must not be on the same room as S *)
  let rec place_exit () =
    let (e_l, e_c) = random_room rows cols in
    if (e_l, e_c) = (s_l, s_c) then place_exit ()
    else g.(e_l).(e_c) <- 'E'
  in
  place_exit ();

  to_grid_t g
