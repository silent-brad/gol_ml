(*
 * Conway's Game of Life in OCaml
 *)

open Random

(* Constant length of game display *)
let game_width = 100
let game_height = 25

(* TODO: add color *)
let block_character = "\u{2588}"
let empty_character = " "

type character = Block | Empty

let get_character_string = function
  | Block -> block_character
  | Empty -> empty_character

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

let () =
  Random.self_init ();
  let gen_random_game_character (_ : int) : character =
    if Random.int 2 = 1 then Block else Empty
  in
  let gen_random_game_line (_ : int) : character list =
    List.init game_width gen_random_game_character
  in
  let gen_random_game_lines : character list list =
    List.init game_height gen_random_game_line
  in
  ignore (print_game_lines gen_random_game_lines);
  ()
