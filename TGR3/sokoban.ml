
(************************************************************

SOKOBAN (Warehouse Keeper)

Game rules:

The game is played on a board of squares, where each square is a floor
or a wall. Some floor squares contain boxes, and some floor squares
are marked as storage locations.

The player is confined to the board, and may move horizontally or
vertically onto empty squares (never through walls or boxes). The
player can also move into a box, which pushes it into the square
beyond. Boxes may not be pushed into other boxes or walls, and they
cannot be pulled. The number of boxes is equal to the number of
storage locations. The puzzle is solved when all boxes are at storage
locations.

https://en.wikipedia.org/wiki/Sokoban

************************************************************)


(************************************************************

Data types

************************************************************)

open Graphics;;

type cell =
  Wall | Floor | Storage | Box | Player | Located | On_storage
;;

type move =
  Up | Down | Left | Right
;;

type board = {
  filename : string;
  mutable size_cell : int;
  mutable player_c_r : int * int;
  panel : cell array array;
  mutable steps_back : ((int * int) * cell) list list;
};;


(************************************************************

Predefined values

************************************************************)

let min_num_columns = 3;;
let max_num_columns = 20;;
let min_num_rows = 3;;
let max_num_rows = 20;;

let min_size_cell = 48;;
let max_size_cell = 80;;

let salmon = 0xff8c69;;
let pastel_green = 0x77dd77;;
let dodger_blue = 0x1e90ff;;
let saddle_brown = 0x8b4513;;


(************************************************************

Drawing functions

************************************************************)

let canvas_x_y brd (column, row) =
  let sz = brd.size_cell in
  (column * sz, row * sz)
;;

let draw_fill_cell brd (c, r) color =
  let (x, y) = canvas_x_y brd (c, r)
  and sz = brd.size_cell in
  let x1 = x+1 and x2 = x+sz-1 and y1 = y+1 and y2 = y+sz-1 in
  set_color color;
  fill_poly [| (x1, y1); (x2, y1); (x2, y2); (x1, y2) |]
;;

let draw_bricks brd (c, r) color =
  let (x, y) = canvas_x_y brd (c, r)
  and sz = brd.size_cell in
  let sz4 = sz/4 in
  let sz42 = sz4*2 and sz43 = sz4*3 in
  let x1 = x+sz4 and x2 = x+sz42 and x3 = x+sz43 and x4 = x+sz
  and y1 = y+sz4 and y2 = y+sz42 and y3 = y+sz43 and y4 = y+sz in
  set_color color;
  set_line_width 2;
  draw_poly [| (x,   y); (x1,  y); (x1, y1); (x,  y1) |];
  draw_poly [| (x1,  y); (x3,  y); (x3, y1); (x1, y1) |];
  draw_poly [| (x3,  y); (x4,  y); (x4, y1); (x3, y1) |];
  draw_poly [| (x,  y1); (x2, y1); (x2, y2); (x,  y2) |];
  draw_poly [| (x2, y1); (x4, y1); (x4, y2); (x2, y2) |];
  draw_poly [| (x,  y2); (x1, y2); (x1, y3); (x,  y3) |];
  draw_poly [| (x1, y2); (x3, y2); (x3, y3); (x1, y3) |];
  draw_poly [| (x3, y2); (x4, y2); (x4, y3); (x3, y3) |];
  draw_poly [| (x,  y3); (x2, y3); (x2, y4); (x,  y4) |];
  draw_poly [| (x2, y3); (x4, y3); (x4, y4); (x2, y4) |]
;;

let draw_storage brd (c, r) color =
  let (x, y) = canvas_x_y brd (c, r)
  and sz = brd.size_cell in
  let sz8 = sz/8 in
  let sz83 = sz8*3 and sz85 = sz8*5 in
  let x3 = x+sz83 and x5 = x+sz85 and y3 = y+sz83 and y5 = y+sz85 in
  set_color color;
  fill_poly [| (x3, y3); (x5, y3); (x5, y5); (x3, y5) |]
;;

let draw_box brd (c, r) color =
  let (x, y) = canvas_x_y brd (c, r)
  and sz = brd.size_cell in
  let sz8 = sz/8 in
  let sz82 = sz8*2 and sz83 = sz8*3 and sz84 = sz8*4
  and sz85 = sz8*5 and sz86 = sz8*6 and sz87 = sz8*7 in
  let x1 = x+sz8  and x2 = x+sz82 and x3 = x+sz83 and x4 = x+sz84
  and x5 = x+sz85 and x6 = x+sz86 and x7 = x+sz87
  and y1 = y+sz8  and y2 = y+sz82 and y3 = y+sz83 and y4 = y+sz84
  and y5 = y+sz85 and y6 = y+sz86 and y7 = y+sz87 in
  set_color color;
  fill_poly [| (x2, y1); (x6, y1); (x7, y2); (x7, y6);
               (x6, y7); (x2, y7); (x1, y6); (x1, y2) |];
  set_color black;
  set_line_width 2;
  draw_poly [| (x2, y1); (x6, y1); (x7, y2); (x7, y6);
               (x6, y7); (x2, y7); (x1, y6); (x1, y2) |];
  draw_poly [| (x2, y1); (x6, y1); (x6, y7); (x2, y7) |];
  draw_poly [| (x3, y1); (x4, y1); (x4, y7); (x3, y7) |];
  draw_poly [| (x4, y1); (x5, y1); (x5, y7); (x4, y7) |];
  set_color color;
  fill_poly [| (x2, y1); (x7, y6); (x6, y7); (x1, y2) |];
  set_color black;
  draw_poly [| (x2, y1); (x7, y6); (x6, y7); (x1, y2) |];
  set_color color;
  fill_poly [| (x1, y6); (x3, y4); (x4, y5); (x2, y7) |];
  set_color black;
  draw_poly [| (x1, y6); (x3, y4); (x4, y5); (x2, y7) |];
  set_color color;
  fill_poly [| (x4, y3); (x6, y1); (x7, y2); (x5, y4) |];
  set_color black;
  draw_poly [| (x4, y3); (x6, y1); (x7, y2); (x5, y4) |]
;;

let draw_player brd (c, r) color =
  let (x, y) = canvas_x_y brd (c, r)
  and sz = brd.size_cell in
  let sz8 = sz/8 in
  let sz83 = sz8*3 and sz85 = sz8*5 and sz87 = sz8*7 in
  let x1 = x+sz8 and x3 = x+sz83 and x5 = x+sz85 and x7 = x+sz87
  and y1 = y+sz8 and y3 = y+sz83 and y5 = y+sz85 and y7 = y+sz87 in
  set_color color;
  fill_poly [| (x1, y3); (x3, y3); (x3, y1); (x5, y1);
               (x5, y3); (x7, y3); (x7, y5); (x5, y5);
               (x5, y7); (x3, y7); (x3, y5); (x1, y5) |]
;;

let draw_cell brd (c, r) =
  match brd.panel.(c).(r) with
    Wall ->
      draw_fill_cell brd (c,r) salmon;
      draw_bricks brd (c,r) white
  | Floor ->
      draw_fill_cell brd (c,r) pastel_green
  | Storage ->
      draw_fill_cell brd (c, r) pastel_green;
      draw_storage brd (c, r) dodger_blue
  | Box ->
      draw_fill_cell brd (c, r) pastel_green;
      draw_box brd (c, r) saddle_brown
  | Player ->
      draw_fill_cell brd (c,r) pastel_green;
      draw_player brd (c, r) black
  | Located ->
      draw_fill_cell brd (c, r) pastel_green;
      draw_box brd (c, r) dodger_blue
  | On_storage ->
      draw_fill_cell brd (c,r) pastel_green;
      draw_player brd (c, r) black;
      draw_storage brd (c, r) dodger_blue
;;

let draw_board brd =
  let columns = Array.length brd.panel in
  let rows = Array.length brd.panel.(0) in
  let sz = brd.size_cell in
  set_window_title ("Sokoban Game: " ^ (Filename.basename brd.filename));
  resize_window (columns * sz) (rows * sz);
  for c = 0 to columns - 1 do
    for r = 0 to rows - 1 do
      draw_cell brd (c, r)
    done
  done
;;

let resize_board brd rel =
  let nsz = brd.size_cell + rel in
  if nsz >= min_size_cell && nsz <= max_size_cell
  then (brd.size_cell <- nsz; draw_board brd)
;;


(************************************************************

Moving the player

************************************************************)

exception Ilegal_move
;;

let step_forward brd mv =
  let (c, r) = brd.player_c_r in
  let (cn1, rn1, cn2, rn2) =
    match mv with
      Up -> (c, r+1, c, r+2)
    | Down -> (c, r-1, c, r-2)
    | Left -> (c-1, r, c-2, r)
    | Right -> (c+1, r, c+2, r) in
  let current = brd.panel.(c).(r)
  and next1 = brd.panel.(cn1).(rn1)
  and next2 = brd.panel.(cn2).(rn2) in
    (match (next1, next2) with
       Floor,   _       -> brd.panel.(cn1).(rn1) <- Player
     | Storage, _       -> brd.panel.(cn1).(rn1) <- On_storage
     | Box,     Floor   -> brd.panel.(cn1).(rn1) <- Player;     brd.panel.(cn2).(rn2) <- Box
     | Box,     Storage -> brd.panel.(cn1).(rn1) <- Player;     brd.panel.(cn2).(rn2) <- Located
     | Located, Floor   -> brd.panel.(cn1).(rn1) <- On_storage; brd.panel.(cn2).(rn2) <- Box
     | Located, Storage -> brd.panel.(cn1).(rn1) <- On_storage; brd.panel.(cn2).(rn2) <- Located
     | _ -> raise Ilegal_move);
    (match current with
       Player     -> brd.panel.(c).(r) <- Floor
     | On_storage -> brd.panel.(c).(r) <- Storage
     | _ -> ());
    brd.player_c_r <- (cn1, rn1);
    draw_cell brd (c, r);
    draw_cell brd (cn1, rn1);
    if next2 = brd.panel.(cn2).(rn2)
    then
      brd.steps_back <- [((c, r), current); ((cn1, rn1), next1)] :: brd.steps_back
    else
      (brd.steps_back <- [((c, r), current); ((cn1, rn1), next1); ((cn2, rn2), next2)] :: brd.steps_back;
       draw_cell brd (cn2, rn2))
;;

let move_player brd mv =
  try
    step_forward brd mv
  with
    _ -> ()
;;

let step_back brd =
  if brd.steps_back <> []
  then
    (let step_back_cells = List.hd brd.steps_back in
     List.iter (function ((c, r), cl) -> brd.panel.(c).(r) <- cl; draw_cell brd (c, r)) step_back_cells;
     brd.player_c_r <- fst (List.hd step_back_cells);
     brd.steps_back <- List.tl brd.steps_back)
;;


(************************************************************

File management

************************************************************)

exception Wrong_board of (string * int)
;;

let load_board f =
  let chn = open_in f in
  let ln1 = input_line chn in
  let columns = try int_of_string ln1 with _ -> raise (Wrong_board (ln1, 1)) in
  let ln2 = input_line chn in
  let rows = try int_of_string ln2 with _ -> raise (Wrong_board (ln2, 2)) in
  if columns < min_num_columns || columns > max_num_columns
  then raise (Wrong_board (ln1, 1));
  if rows < min_num_rows || rows > max_num_rows
  then raise (Wrong_board (ln2, 2));
  let brd = { filename = f;
              size_cell = (min_size_cell+max_size_cell)/2;
              player_c_r = (0,0);
              panel = Array.make_matrix columns rows Wall;
              steps_back = [] } in
  for r = rows - 1 downto 0 do
    let row = input_line chn in
    for c = 0 to columns - 1 do
      match row.[c] with
        'W' -> brd.panel.(c).(r) <- Wall
      | ' ' -> brd.panel.(c).(r) <- Floor
      | '.' -> brd.panel.(c).(r) <- Storage
      | 'B' -> brd.panel.(c).(r) <- Box
      | '+' -> (brd.panel.(c).(r) <- Player;
                brd.player_c_r <- (c,r))
      | 'L' -> brd.panel.(c).(r) <- Located
      | _ -> raise (Wrong_board (row, rows-r+2))
      done
    done;
  brd
;;

(*
Work to do in load_board function:
  Check the complete integrity of the board file:
    - The four edges of the board must be walls
    - Put one (and only one) player
    - The number of boxes must be equal to the number of storage locations
*)

let reset_board brd =
  let nbrd = load_board brd.filename in
  draw_board nbrd;
  nbrd
;;

let read_line chn =
  try
    Some (input_line chn)
  with
    End_of_file -> None
;;

let move_of_string = function
    "Up" -> Up
  | "Down" -> Down
  | "Left" -> Left
  | "Right" -> Right
  | _ -> raise (Failure "move_of_string")
;;

exception Wrong_plan of string * int
;;

let load_plan f =
  let chn = open_in f in
  let rec aux acc n =
    match (read_line chn) with
      None ->
        (List.rev acc, n)
    | Some ln ->
        let mv =
          try
            move_of_string (String.trim ln)
          with
            _ -> raise (Wrong_plan (ln, n+1))
        in
          aux (mv :: acc) (n+1)
  in
    aux [] 0
;;

(************************************************************

Keyloops

************************************************************)

let rec keyloop_game brd =
  Graphics.synchronize ();
  let s = Graphics.wait_next_event [Graphics.Key_pressed] in
    match s.Graphics.key with
      'w' | 'i' -> move_player brd Up; keyloop_game brd
    | 'a' | 'j' -> move_player brd Left; keyloop_game brd
    | 's' | 'k' -> move_player brd Down; keyloop_game brd
    | 'd' | 'l' -> move_player brd Right; keyloop_game brd
    | 'b' -> step_back brd; keyloop_game brd
    | 'r' -> keyloop_game (reset_board brd)
    | '+' -> resize_board brd 8; keyloop_game brd
    | '-' -> resize_board brd (-8); keyloop_game brd
    | 'q' -> Graphics.close_graph(); exit 0
    | _   -> keyloop_game brd
;;

let rec keyloop_solver brd mvs_done mvs_todo i n =
  Graphics.set_window_title
    ("Sokoban Solver: " ^ (Filename.basename brd.filename) ^
     " (" ^ (string_of_int i) ^ "/" ^ (string_of_int n) ^ " steps)");
  Graphics.synchronize ();
  let s = Graphics.wait_next_event [Graphics.Key_pressed] in
    match s.Graphics.key with
    | 'f' -> if mvs_todo <> []
             then
               (let mv = List.hd mvs_todo in
                try
                  step_forward brd mv;
                  keyloop_solver brd (mv :: mvs_done) (List.tl mvs_todo) (i+1) n
                with
                  _ -> keyloop_solver brd mvs_done mvs_todo i n)
             else keyloop_solver brd mvs_done mvs_todo i n
    | 'b' -> if mvs_done <> []
             then
               (let mv = List.hd mvs_done in
                step_back brd;
                keyloop_solver brd (List.tl mvs_done) (mv :: mvs_todo) (i-1) n)
             else keyloop_solver brd mvs_done mvs_todo i n
    | 'r' -> keyloop_solver (reset_board brd) [] (List.rev_append mvs_done mvs_todo) 0 n
    | '+' -> resize_board brd 8; keyloop_solver brd mvs_done mvs_todo i n
    | '-' -> resize_board brd (-8); keyloop_solver brd mvs_done mvs_todo i n
    | 'q' -> Graphics.close_graph(); exit 0
    | _   -> keyloop_solver brd mvs_done mvs_todo i n
;;


(************************************************************

Usage and error messages

************************************************************)

let print_usage_msg () =
  print_newline ();
  print_endline ("Usage: " ^ Filename.basename Sys.argv.(0) ^ " <board-file> [moves-file]");
  print_newline ();

  print_endline "If you provide only the board file, you enter Game mode";
  print_endline "  You have the following keys available:";
  print_endline "    w or i : move the player up";
  print_endline "    a or j : move the player to the left";
  print_endline "    s or k : move the player down";
  print_endline "    d or l : move the player to the right";
  print_endline "    r : reload the board from file";
  print_newline ();

  print_endline "If you also provide a movement file, you enter Solver mode";
  print_endline "  You have the following keys available:";
  print_endline "    f : move the player one step forward according to the movement plan";
  print_endline "    r : reload the board and the plan from files";
  print_newline ();

  print_endline "In both modes you also have available:";
  print_endline "  b : move the player one step back";
  print_endline "  + : increase board size";
  print_endline "  - : decrease board size";
  print_endline "  q : quit";
  print_newline ()
;;

let print_syntax_board_file_msg () =
  print_endline "Syntax for the board file:";
  print_endline "  One line for number of columns (max: 20)";
  print_endline "  One line for number of rows (max: 20)";
  print_endline "  And then, one line per row";
  print_endline "  The four edges of the board should be walls";
  print_endline "  You should put one (and only one) player";
  print_endline "  The number of boxes should be equal to the number of storage locations";
  print_endline "  Use the following characters:";
  print_endline "    W : wall";
  print_endline "    . : storage location";
  print_endline "    B : box";
  print_endline "    L : box located";
  print_endline "    + : player";
  print_endline "    space : floor";
  print_newline ()
;;

let print_syntax_moves_file_msg () =
  print_endline "Syntax for the movement file:";
  print_endline "  One line per move";
  print_endline "  Use the word Up, Down, Left or Right in each line";
  print_newline ()
;;

let print_error_msg e =
  match e with
    Sys_error s ->
      print_endline s
  | Invalid_argument _ ->
        print_endline "Check the integrity of the board file";
        print_endline "Premature end of row";
        print_syntax_board_file_msg ();
  | End_of_file ->
        print_endline "Check the integrity of the board file";
        print_endline "Premature end of file";
        print_syntax_board_file_msg ();
  | Wrong_board (s, n) ->
      print_endline "Check the integrity of the board file";
      print_endline ("Error in line " ^ (string_of_int n) ^ ": " ^ s);
      print_syntax_board_file_msg ();
  | Wrong_plan (s, n) ->
      print_endline "Check the integrity of the movement file";
      print_endline ("Error in line " ^ (string_of_int n) ^ ": " ^ s);
      print_syntax_moves_file_msg ();
  | _ ->
      print_endline "Internal error"
;;


(************************************************************

Main

************************************************************)

match Array.length Sys.argv with
    2 ->
      (try
         let board = load_board Sys.argv.(1) in
         Graphics.open_graph " 100x100";
         draw_board board;
         keyloop_game board
       with
         e ->
           print_error_msg e;
           exit 1)
  | 3 ->
      (try
         let board = load_board Sys.argv.(1)
         and (plan, len) = load_plan Sys.argv.(2) in
         Graphics.open_graph " 100x100";
         draw_board board;
         keyloop_solver board [] plan 0 len
       with
         e ->
           print_error_msg e;
           exit 1)
  | _ ->
      print_usage_msg ();
      print_syntax_board_file_msg ();
      print_syntax_moves_file_msg ();
      exit 1
;;

(************************************************************

End of program.

************************************************************)

