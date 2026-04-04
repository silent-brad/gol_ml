(*
 * Conway's Game of Life in OCaml
 * Rules:
 *   1. Any live cell with fewer than two live neighbors dies, as if by underpopulation
 *   2. Any live cell with two or three live neighbors lives on to the next generation
 *   3. Any live cell with more than three live neighbors dies, as if by overpopulation
 *   4. Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction
 *)

open Random
open Unix

(* Constant length of game display *)
let game_width = 50
let game_height = 20

(* NOTE: These ANSI color codes are designed to work with Nushell/Ghostty and may not work with other shells/terminals *)
(* IDEA: Add colors by organ? *)
let block_character () =
  (* Shuffles through colors *)
  "\u{1b}[3" ^ Int.to_string (Random.int 6 + 1) ^ "m\u{2588}\u{1b}[0m"

let empty_character = " "

type character = Live | Dead

let get_character_string = function
  | Live -> block_character ()
  | Dead -> empty_character

(* TODO: Add frame border *)

(* Use index instead of list so that doesn't return anything? *)
let rec print_game_line (characters : character list) : character list =
  match characters with
  | [] -> []
  | hd :: tl ->
      print_string (get_character_string hd);
      print_game_line tl

let rec print_game_lines (lines : character list list) : character list list =
  match lines with
  | [] -> []
  | hd :: tl ->
      let _ = print_game_line hd in
      print_newline ();
      print_game_lines tl

(*
8 potential live cells to check
x x x
x 0 x
x x x
  *)

let get_live_neighbors (_lines : character list list) (_col : int) (_row : int)
    : int =
  Random.int 4

let rec get_next_grid old_grid grid line_index =
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
  ignore (print_game_lines grid);
  Unix.sleepf 0.5;
  loop (get_next_grid grid grid 0)

let () =
  Random.self_init ();
  loop initial_grid
