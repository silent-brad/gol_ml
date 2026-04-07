(*
 * Conway's Game of Life in OCaml
 * Rules:
 *   1. Any live cell with fewer than two live neighbors dies, as if by underpopulation
 *   2. Any live cell with two or three live neighbors lives on to the next generation
 *   3. Any live cell with more than three live neighbors dies, as if by overpopulation
 *   4. Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction
 *)

(* NOTE: Just rewrite this! *)

open Random
open Unix

(* Length of game display *)
let game_width = 120
let game_height = 50

let live_character () =
  (* Shuffles through colors *)
  "\u{1b}[3" ^ Int.to_string (Random.int 6 + 1) ^ "m\u{2588}\u{1b}[0m"

let dead_character = " "

type character = Live | Dead

let get_character_string = function
  | Live -> live_character ()
  | Dead -> dead_character

let print_border_top () =
  print_string "\u{250C}";
  for _ = 1 to game_width do
    print_string "\u{2500}"
  done;
  print_string "\u{2510}";
  print_newline ()

let print_border_bottom () =
  print_string "\u{2514}";
  for _ = 1 to game_width do
    print_string "\u{2500}"
  done;
  print_string "\u{2518}";
  print_newline ()

(* TODO: Use index instead of list so that doesn't return anything? *)
let rec print_game_line (characters : character list) : character list =
  match characters with
  | [] -> []
  | hd :: tl ->
      print_string (get_character_string hd);
      print_game_line tl

let print_game_lines (lines : character list list) =
  print_border_top ();
  let rec print_lines lines =
    match lines with
    | [] -> ()
    | hd :: tl ->
        print_string "\u{2502}";
        ignore (print_game_line hd);
        print_string "\u{2502}";
        print_newline ();
        print_lines tl
  in
  print_lines lines;
  print_border_bottom ()

let get_live_neighbors (grid : character list list) line_index cell_index : int
    =
  let neighbors =
    List.init 3 (fun i ->
        if line_index = 0 || cell_index + i = 0 then Dead
        else
          try List.nth (List.nth grid (line_index - 1)) (cell_index + i - 1)
          with Failure _ -> Dead)
    @ List.init 3 (fun i ->
        if i = 1 || cell_index + i = 0 then Dead
        else
          try List.nth (List.nth grid line_index) (cell_index + i - 1)
          with Failure _ -> Dead)
    @ List.init 3 (fun i ->
        if cell_index + i = 0 then Dead
        else
          try List.nth (List.nth grid (line_index + 1)) (cell_index + i - 1)
          with Failure _ -> Dead)
  in
  List.length (List.filter (fun neighbor -> neighbor = Live) neighbors)

let rec get_next_grid (old_grid : character list list)
    (grid : character list list) line_index =
  let rec get_next_line line line_index cell_index =
    let get_next_cell (cell : character) line_index cell_index =
      let live_neighbors = get_live_neighbors old_grid line_index cell_index in
      if live_neighbors < 2 || live_neighbors > 3 then Dead
      else if live_neighbors = 3 && cell = Dead then Live
      else cell
    in
    match line with
    | [] -> []
    | cell :: cells ->
        get_next_cell cell line_index cell_index
        :: get_next_line cells line_index (cell_index + 1)
  in
  match grid with
  | [] -> []
  | line :: lines ->
      get_next_line line line_index 0
      :: get_next_grid old_grid lines (line_index + 1)

let () = Random.self_init ()

let gen_random_game_character (_ : int) : character =
  if Random.int 2 = 1 then Live else Dead

let gen_random_game_line (_ : int) : character list =
  List.init game_width gen_random_game_character

let gen_random_game_lines : character list list =
  List.init game_height gen_random_game_line

let initial_grid = gen_random_game_lines

let rec loop (grid : character list list) =
  (* Clear the terminal *)
  print_string "\u{001b}[2J\u{001b}[H";
  let alive_count =
    List.length (List.filter (fun cell -> cell = Live) (List.concat grid))
  in
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
  in
  let digits = String.to_seq (string_of_int alive_count) |> List.of_seq in
  let ascii_number_line row =
    String.concat ""
      (List.map
         (fun c -> ascii_digits.(Char.code c - Char.code '0').(row))
         digits)
  in
  Printf.printf "    _    _ _             ____     _ _       %s\n" (ascii_number_line 0) [@ocamlformat "disable"];
  Printf.printf "   / \\  | (_)_   _____  / ___|___| | |___   %s\n" (ascii_number_line 1) [@ocamlformat "disable"];
  Printf.printf "  / _ \\ | | \\ \\ / / _ \\| |   / _ \\ | / __|  %s\n" (ascii_number_line 2) [@ocamlformat "disable"];
  Printf.printf " / ___ \\| | |\\ V /  __/| |__|  __/ | \\__ \\ \n" [@ocamlformat "disable"];
  Printf.printf "/_/   \\_\\_|_| \\_/ \\___| \\____\\___|_|_|___/ \n" [@ocamlformat "disable"];
  print_game_lines grid;
  Unix.sleepf 0.5;
  loop (get_next_grid grid grid 0)

let () = loop initial_grid
