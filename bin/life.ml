(*
 * Conway's Game of Life in OCaml
 * Rules:
 *   1. Any live cell with fewer than two Alive neighbors dies (underpopulation)
 *   2. Any live cell with two or three Alive neighbors Alives on
 *   3. Any live cell with more than three Alive neighbors dies (overpopulation)
 *   4. Any dead cell with exactly three Alive neighbors becomes Alive (reproduction)
 *)

let width = 120
let height = 50

type cell = Alive | Dead

let live_cell_string () =
  "\u{1b}[3" ^ Int.to_string (Random.int 6 + 1) ^ "m\u{2588}\u{1b}[0m"

let cell_to_string = function Alive -> live_cell_string () | Dead -> " "

let print_horizontal_border left right =
  print_string left;
  for _ = 1 to width do
    print_string "\u{2500}"
  done;
  print_string right;
  print_newline ()

let print_row (row : cell list) =
  print_string "\u{2502}";
  List.iter (fun c -> print_string (cell_to_string c)) row;
  print_string "\u{2502}";
  print_newline ()

let print_grid (grid : cell list list) =
  print_horizontal_border "\u{250C}" "\u{2510}";
  List.iter print_row grid;
  print_horizontal_border "\u{2514}" "\u{2518}"

let count_live_neighbors (grid : cell list list) row col =
  let get_cell grid row col =
    try List.nth (List.nth grid row) col
    with Failure _ | Invalid_argument _ -> Dead
  in
  let offsets = [ -1; 0; 1 ] in
  List.fold_left
    (fun acc dr ->
      List.fold_left
        (fun acc dc ->
          if dr = 0 && dc = 0 then acc
          else if get_cell grid (row + dr) (col + dc) = Alive then acc + 1
          else acc)
        acc offsets)
    0 offsets

let next_grid (grid : cell list list) =
  List.mapi
    (fun row_i row ->
      List.mapi
        (fun col_i cell ->
          let n = count_live_neighbors grid row_i col_i in
          match cell with
          | Alive -> if n = 2 || n = 3 then Alive else Dead
          | Dead -> if n = 3 then Alive else Dead)
        row)
    grid

let random_grid () =
  List.init height (fun _ ->
      List.init width (fun _ -> if Random.int 2 = 1 then Alive else Dead))

let ascii_digits =
  [|
    [| " _ "; "| |"; "|_|" |];
    [| "   "; "  |"; "  |" |];
    [| " _ "; " _|"; "|_ " |];
    [| " _ "; " _|"; " _|" |];
    [| "   "; "|_|"; "  |" |];
    [| " _ "; "|_ "; " _|" |];
    [| " _ "; "|_ "; "|_|" |];
    [| " _ "; "  |"; "  |" |];
    [| " _ "; "|_|"; "|_|" |];
    [| " _ "; "|_|"; " _|" |];
  |]

let ascii_number_line digits row =
  String.concat ""
    (List.map
       (fun c -> ascii_digits.(Char.code c - Char.code '0').(row))
       digits)

let rec loop (grid : cell list list) =
  print_string "\u{001b}[2J\u{001b}[H";
  let alive_count =
    List.length (List.filter (fun c -> c = Alive) (List.concat grid))
  in
  let digits = String.to_seq (string_of_int alive_count) |> List.of_seq in
  Printf.printf "    _    _ _             ____     _ _       %s\n" (ascii_number_line digits 0) [@ocamlformat "disable"];
  Printf.printf "   / \\  | (_)_   _____  / ___|___| | |___   %s\n" (ascii_number_line digits 1) [@ocamlformat "disable"];
  Printf.printf "  / _ \\ | | \\ \\ / / _ \\| |   / _ \\ | / __|  %s\n" (ascii_number_line digits 2) [@ocamlformat "disable"];
  Printf.printf " / ___ \\| | |\\ V /  __/| |__|  __/ | \\__ \\ \n" [@ocamlformat "disable"];
  Printf.printf "/_/   \\_\\_|_| \\_/ \\___| \\____\\___|_|_|___/ \n" [@ocamlformat "disable"];
  print_grid grid;
  Unix.sleepf 0.5;
  loop (next_grid grid)

let () =
  Random.self_init ();
  loop (random_grid ())
