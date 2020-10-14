(*
   TGR 3 Paradigmas de Programación

   Carlos Torres Paz

   carlos.torres@udc.es
*)


type cell =
    Wall | Floor | Storage

type move =
    Up | Down | Left | Right

type board = {
  size : int * int;
  panel : cell array array;
}

(*Functions that are in the List module in the latest version but not in 4.05*)

let init len f =
  let rec aux_tailrec acc i n f =
    if i>= n then acc
    else aux_tailrec (f i :: acc) (i+1) n f
  and aux i n f =
    if i>=n then []
    else
      let r = f i in
      r :: aux (i+1) n f
  in
  if len < 0 then invalid_arg "init"
  else if len > 10_000 then List.rev (aux_tailrec [] 0 len f)
  else (aux 0 len f)


let filter_map f =
  let rec aux accu = function
      [] -> List.rev accu
    | x :: l ->
      match f x with
        None -> aux accu l
      | Some v -> aux (v :: accu) l
  in
  aux []

module State =
struct
  type t = {
    player_pos : int * int;
    box_list : (int * int) list;
  }
  let compare s1 s2 =
    match Pervasives.compare s1.player_pos s2.player_pos with
      0 -> Pervasives.compare s1.box_list s2.box_list
    | c -> c
end

open State

module StateSet = Set.Make(State)

type state = {
  player_pos : int * int;
  box_list : (int * int) list;
  plan : move list
}

exception Invalid_board of string

exception Invalid_movement of string

let setelem_of_state (x : state) : StateSet.elt = {player_pos = x.player_pos; box_list = x.box_list}

let is_none = function
    Some _ -> false
  | None -> true

let some_of_opt = function
    Some x -> x
  | None -> raise (Invalid_argument "some_of_opt")

let some_of_opt_pair = function
    (Some x, p) -> (x,p)
  | (None, _) -> raise (Invalid_argument "some_of_opt_pair")

let parse in_file =
  let cols = (int_of_string (input_line in_file)) in
  let rows = (int_of_string (input_line in_file)) in
  let player_position = ref (0,0) in
  let boxes = ref ([]) in
  let mat = Array.make_matrix (rows) (cols) Wall in
  for i = 0 to (rows-1) do
    for j = 0 to (cols-1) do
      match (input_char in_file) with
        'W' -> ()
      | '.' -> mat.(i).(j) <- Storage
      | 'B' -> (mat.(i).(j) <- Floor;
                boxes := (i,j)::(!boxes))
      | 'L' -> (mat.(i).(j) <- Storage;
                boxes := (i,j)::(!boxes))
      | ' ' -> mat.(i).(j) <- Floor
      | '+' -> (mat.(i).(j) <- Floor;
                player_position := (i,j))
      | _ -> raise ((Invalid_board "parsing failed"))
    done;
    ignore (input_char in_file);
  done;
  ({
    size = (rows,cols);
    panel = mat
  },
    {
      player_pos = !player_position;
      box_list = List.sort (Pervasives.compare) !boxes;
      plan = []
    })

let is_corner board (x,y) =
       if ((board.panel.(x-1).(y) = Wall) && (board.panel.(x).(y-1) = Wall)) then true
  else if ((board.panel.(x-1).(y) = Wall) && (board.panel.(x).(y+1) = Wall)) then true
  else if ((board.panel.(x+1).(y) = Wall) && (board.panel.(x).(y-1) = Wall)) then true
  else if ((board.panel.(x+1).(y) = Wall) && (board.panel.(x).(y+1) = Wall)) then true
  else false

let is_next_to_wall board (x,y) =
  let temp = ref true in
  if ((x = 1) || (x = (fst board.size -2))) then (
      for i = 1 to (snd board.size -2) do
        if (board.panel.(x).(i) = Storage) then (temp := false)
      done;
      !temp
    )
  else if ((y = 1) || (y = (snd board.size -2))) then (
    for i = 1 to (fst board.size -2) do
      if (board.panel.(i).(y) = Storage) then (temp := false)
    done;
    !temp
  )
  else false

exception False_exc

let useless_move board current mov =
  let i_from, i_to, j_from, j_to =
    match mov with
      Up -> (1,(fst current.player_pos),(snd current.player_pos -1),(snd current.player_pos +1))
    | Down -> ((fst current.player_pos),(fst board.size -2),(snd current.player_pos -1),(snd current.player_pos +1))
    | Left -> ((fst current.player_pos -1), (fst current.player_pos +1), 1, (snd current.player_pos))
    | Right -> ((fst current.player_pos -1), (fst current.player_pos +1), (snd current.player_pos), (snd board.size -2))
  in
  try
  for i = i_from to i_to do
    for j = j_from to j_to do
      if (board.panel.(i).(j) = Wall) then raise False_exc;
      if (List.mem (i,j) current.box_list) then raise False_exc;
    done;
  done;
  true
  with False_exc -> false


let mover board state_set current mov =
  let new_state current mov =
    let newpos, next_pos =
      match mov with
        Up -> ((fst current.player_pos -1, snd current.player_pos),(fst current.player_pos -2, snd current.player_pos))
      | Down -> ((fst current.player_pos +1, snd current.player_pos),(fst current.player_pos +2, snd current.player_pos))
      | Left -> ((fst current.player_pos, snd current.player_pos -1),(fst current.player_pos, snd current.player_pos -2))
      | Right -> ((fst current.player_pos, snd current.player_pos +1),(fst current.player_pos, snd current.player_pos +2))
    in
    match board.panel.(fst newpos).(snd newpos) with (*Check where I'm going*)
      Floor | Storage -> (
        if List.mem newpos current.box_list then (*If going to push a box*)
          (
            match (board.panel.(fst next_pos).(snd next_pos)) with (*Check where would that box go*)
              Floor | Storage -> (
                if List.mem next_pos current.box_list then (*If going to push two boxes*)
                  raise (Invalid_movement "tried to push two boxes")
                else if ((is_corner board next_pos) && (board.panel.(fst next_pos).(snd next_pos) <> Storage)) then
                  raise (Invalid_movement "tried to push a box into a corner that was not a storage location")
                else if (is_next_to_wall board next_pos) then
                    raise (Invalid_movement "tried to push a box into a wall (blocking it)")
                else
                  {
                    player_pos = newpos;
                    box_list = List.sort (Pervasives.compare) (next_pos::(List.filter ((<>) newpos) current.box_list));
                    plan = mov::(current.plan)
                  } (*Returns the new state, where the player moved up one position, pushing one box
                      (removes the box in newpos and adds it in next_pos)*)
              )
            | Wall -> raise (Invalid_movement "tried to push a box into a wall")
          )
          (*If not going to push a box*)
        else (
          if useless_move board current mov then raise (Invalid_movement "")
              else
          {
            player_pos = newpos;
            box_list = current.box_list;
            plan = mov::(current.plan)
          }) (*Returns the new state, where the player moved up one position*)
      )
    | Wall -> raise (Invalid_movement "tried to move into wall")
  in
  try
    let ns = new_state current mov in
    Some ns
  with
    Invalid_movement str -> None

let is_solution board state =
  List.for_all (fun (a,b) -> board.panel.(a).(b) = Storage) state.box_list

let string_of_move = function
    Up -> "Up"
  | Down -> "Down"
  | Left -> "Left"
  | Right -> "Right"

let rec print_moves_list = function
    [] -> ()
  | h::t -> print_endline (string_of_move h); print_moves_list t

let in_depth board set current_state =
  if (StateSet.mem (setelem_of_state current_state) set) then ([], set)
  else (
    (filter_map
       (fun x -> x)
         [(mover board set current_state Up);(mover board set current_state Down);(mover board set current_state Left);(mover board set current_state Right)]
    ), (StateSet.add (setelem_of_state current_state) set))

let rec flood =
  function board ->
  function set ->
  function [] -> ( (*current es vacía*)
      function [] -> raise Not_found (*Ambas listas son vacías, no se encuentra la solución*)
             | next -> flood board set next [](*Se acabó el nivel actual, se pasa al siguiente*)
    )
         | c::cc -> ( (*Aún quedan estados por procesar en la lista actual*)
             function next -> (
                 if (is_solution board c) then c (*Si la que vamos a procesar es solución, se devuelve esa y la función para*)
                 else (*Si la que se va a procesar no es solución, se quita de la lista actual y se añaden los productos de su procesado a la next*)
                   let newstates, set = in_depth board set c in
                   flood board set cc (newstates @ next) (*newstates es una lista de (como máximo) cuatro nuevos estados*)
               )
           )

let solve board_file =
  let board, inicial = parse (open_in board_file)
  in
  (List.rev (flood board (StateSet.empty) [inicial] []).plan)

;;

try
  if (Array.length Sys.argv) < 2 then print_endline "usage: ./ia board_file"
  else
    print_moves_list (solve Sys.argv.(1))
with
  (Sys_error str) -> print_endline (Sys.argv.(1) ^ " is not a valid board file ")
