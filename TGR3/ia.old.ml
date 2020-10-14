type t_cell =
    Wall | Floor | Storage | Box | Player | Box_Located | Player_on_storage
;;

type move =
    Up | Down | Left | Right
;;

type cell = {
  tipo : t_cell;
  mutable visitada : bool;
}
;;

type board = {
  size : int * int;
  player_pos : int * int;
  panel : cell array array;
}
;;

exception Invalid_board of string
;;

exception Different_boards
;;

exception Invalid_movement of string
;;

type 'a tree = Gt of 'a * 'a tree list;;

let get_raiz arb = match arb with
    Gt (raiz, _) -> raiz;;

let parse in_file =
  let cols = (int_of_string (input_line in_file)) in
  let rows = (int_of_string (input_line in_file)) in
  let player_position = ref (0,0) in
  let mat = Array.make_matrix (rows) (cols) ({tipo = Wall; visitada = false}) in
  for i = 0 to (rows-1) do
    for j = 0 to (cols-1) do
      match (input_char in_file) with
      'W' -> ()
      | '.' -> mat.(i).(j) <- {tipo = Storage; visitada = false}
      | 'B' -> mat.(i).(j) <- {tipo = Box; visitada = false}
      | 'L' -> mat.(i).(j) <- {tipo = Box_Located; visitada = false}
      | ' ' -> mat.(i).(j) <- {tipo = Floor; visitada = false}
      | '+' -> (mat.(i).(j) <- {tipo = Player; visitada = false};
                player_position := (i,j))
      | _ -> raise ((Invalid_board "parsing failed"))
    done;
    ignore (input_char in_file);
  done;
  {
    size = (rows,cols);
    player_pos = !player_position;
    panel = mat
  }
;;

let tabl_existed tab1 tablist =
    let compare_boards tab1 tab2 =
      if tab1.size <> tab2.size then raise (Invalid_board "tabl_existed: different sized boards");
      try
        for i=0 to fst tab1.size -1 do
          for j=0 to snd tab2.size -1 do
            if ((tab1.panel.(i).(j).tipo) <> (tab2.panel.(i).(j).tipo)) then raise Different_boards;
          done;
        done;
        true
      with Different_boards -> false
    in
    List.exists (compare_boards tab1) tablist
;;

let mover current mov =
  match mov with
    Up -> (
      let newtab =
        {size = current.size; player_pos = ((fst current.player_pos - 1),(snd current.player_pos)); panel = Array.map Array.copy current.panel} in
      (
        match newtab.panel.(fst current.player_pos).(snd current.player_pos).tipo with (*Comprobar donde estaba*)
          Player -> newtab.panel.(fst current.player_pos).(snd current.player_pos) <- {tipo = Floor; visitada = false}
        | Player_on_storage -> newtab.panel.(fst current.player_pos).(snd current.player_pos) <- {tipo = Storage; visitada = false}
        | _ -> raise (Invalid_movement "mover Down: La celda apuntada por player_pos no es el player")
      );
      (
        match newtab.panel.(fst newtab.player_pos).(snd current.player_pos).tipo with (*Comprobar a donde voy*)
          Floor -> newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos) <- {tipo = Player; visitada = false}
        | Storage -> newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos) <- {tipo = Player_on_storage; visitada = false}
        | Box -> (
            (*Caso en el que se empuja una caja*)
            newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos) <- {tipo = Player; visitada = false};
            (*Comprobar a dónde va a ir la caja*)
            (
              match newtab.panel.(fst newtab.player_pos - 1).(snd newtab.player_pos).tipo with
                Floor -> newtab.panel.(fst newtab.player_pos - 1).(snd newtab.player_pos) <- {tipo = Box; visitada = false}
              | Storage -> newtab.panel.(fst newtab.player_pos - 1).(snd newtab.player_pos) <- {tipo = Box_Located; visitada = false}
              | _ -> raise (Invalid_movement "mover Down: la caja se va a ir contra un muro o otra caja")
            )
          )
        | Box_Located -> (
            (*Se empuja una caja que ya estaba en el sitio*)
            newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos) <- {tipo = Player_on_storage; visitada = false};
            (*Comprobar a dónde va a ir la caja*)
            (
              match newtab.panel.(fst newtab.player_pos - 1).(snd newtab.player_pos).tipo with
                Floor -> newtab.panel.(fst newtab.player_pos - 1).(snd newtab.player_pos) <- {tipo = Box; visitada = false}
              | Storage -> newtab.panel.(fst newtab.player_pos - 1).(snd newtab.player_pos) <- {tipo = Box_Located; visitada = false}
              | _ -> raise (Invalid_movement "mover Down: la caja (ya situada) se va contra un muro u otra caja")
            )
          )
        | _ -> raise (Invalid_movement "mover Down: el jugador se va a mover hacia un muro (u otro jugador)")
      );
      newtab
    )
  | Down -> (
      let newtab =
        {size = current.size; player_pos = ((fst current.player_pos + 1),(snd current.player_pos)); panel = Array.map Array.copy current.panel} in
      (
        match newtab.panel.(fst current.player_pos).(snd current.player_pos).tipo with (*Comprobar donde estaba*)
          Player -> newtab.panel.(fst current.player_pos).(snd current.player_pos) <- {tipo = Floor; visitada = false}
        | Player_on_storage -> newtab.panel.(fst current.player_pos).(snd current.player_pos) <- {tipo = Storage; visitada = false}
        | _ -> raise (Invalid_movement "mover Down: La celda apuntada por player_pos no es el player")
      );
      (
        match newtab.panel.(fst newtab.player_pos).(snd current.player_pos).tipo with (*Comprobar a donde voy*)
          Floor -> newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos) <- {tipo = Player; visitada = false}
        | Storage -> newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos) <- {tipo = Player_on_storage; visitada = false}
        | Box -> (
            (*Caso en el que se empuja una caja*)
            newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos) <- {tipo = Player; visitada = false};
            (*Comprobar a dónde va a ir la caja*)
            (
              match newtab.panel.(fst newtab.player_pos + 1).(snd newtab.player_pos).tipo with
                Floor -> newtab.panel.(fst newtab.player_pos + 1).(snd newtab.player_pos) <- {tipo = Box; visitada = false}
              | Storage -> newtab.panel.(fst newtab.player_pos + 1).(snd newtab.player_pos) <- {tipo = Box_Located; visitada = false}
              | _ -> raise (Invalid_movement "mover Down: la caja se va a ir contra un muro o otra caja")
            )
          )
        | Box_Located -> (
            (*Se empuja una caja que ya estaba en el sitio*)
            newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos) <- {tipo = Player_on_storage; visitada = false};
            (*Comprobar a dónde va a ir la caja*)
            (
              match newtab.panel.(fst newtab.player_pos + 1).(snd newtab.player_pos).tipo with
                Floor -> newtab.panel.(fst newtab.player_pos + 1).(snd newtab.player_pos) <- {tipo = Box; visitada = false}
              | Storage -> newtab.panel.(fst newtab.player_pos + 1).(snd newtab.player_pos) <- {tipo = Box_Located; visitada = false}
              | _ -> raise (Invalid_movement "mover Down: la caja (ya situada) se va contra un muro u otra caja")
            )
          )
        | _ -> raise (Invalid_movement "mover Down: el jugador se va a mover hacia un muro (u otro jugador)")
      );
      newtab
    )
  | Left -> (
      let newtab =
        {size = current.size; player_pos = ((fst current.player_pos),(snd current.player_pos - 1)); panel = Array.map Array.copy current.panel} in
      (
        match newtab.panel.(fst current.player_pos).(snd current.player_pos).tipo with (*Comprobar donde estaba*)
          Player -> newtab.panel.(fst current.player_pos).(snd current.player_pos) <- {tipo = Floor; visitada = false}
        | Player_on_storage -> newtab.panel.(fst current.player_pos).(snd current.player_pos) <- {tipo = Storage; visitada = false}
        | _ -> raise (Invalid_movement "mover Left: el puntero player_pos no apunta al jugador")
      );
      (
        match newtab.panel.(fst newtab.player_pos).(snd current.player_pos).tipo with (*Comprobar a donde voy*)
          Floor -> newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos) <- {tipo = Player; visitada = false}
        | Storage -> newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos) <- {tipo = Player_on_storage; visitada = false}
        | Box -> (
            (*Caso en el que se empuja una caja*)
            newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos) <- {tipo = Player; visitada = false};
            (*Comprobar a dónde va a ir la caja*)
            (
              match newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos - 1).tipo with
                Floor -> newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos - 1) <- {tipo = Box; visitada = false}
              | Storage -> newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos - 1) <- {tipo = Box_Located; visitada = false}
              | _ -> raise (Invalid_movement "mover Left: la caja va a una zona ilegal")
            )
          )
        | Box_Located -> (
            (*Se empuja una caja que ya estaba en el sitio*)
            newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos) <- {tipo = Player_on_storage; visitada = false};
            (*Comprobar a dónde va a ir la caja*)
            (
              match newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos - 1).tipo with
                Floor -> newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos - 1) <- {tipo = Box; visitada = false}
              | Storage -> newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos - 1) <- {tipo = Box_Located; visitada = false}
              | _ -> raise (Invalid_movement "mover Left: caja ya situada va a zona ilegal")
            )
          )
        | _ -> raise (Invalid_movement "mover Left: el jugador va a moverse a zona ilegal")
      );
      newtab
    )
  | Right -> (
      let newtab =
        {size = current.size; player_pos = ((fst current.player_pos),(snd current.player_pos + 1)); panel = Array.map Array.copy current.panel} in
      (
        match newtab.panel.(fst current.player_pos).(snd current.player_pos).tipo with (*Comprobar donde estaba*)
          Player -> newtab.panel.(fst current.player_pos).(snd current.player_pos) <- {tipo = Floor; visitada = false}
        | Player_on_storage -> newtab.panel.(fst current.player_pos).(snd current.player_pos) <- {tipo = Storage; visitada = false}
        | _ -> raise (Invalid_movement "mover Right: el puntero player_pos no apunta al jugador")
      );
      (
        match newtab.panel.(fst newtab.player_pos).(snd current.player_pos).tipo with (*Comprobar a donde voy*)
          Floor -> newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos) <- {tipo = Player; visitada = false}
        | Storage -> newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos) <- {tipo = Player_on_storage; visitada = false}
        | Box -> (
            (*Caso en el que se empuja una caja*)
            newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos) <- {tipo = Player; visitada = false};
            (*Comprobar a dónde va a ir la caja*)
            (
              match newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos + 1).tipo with
                Floor -> newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos + 1) <- {tipo = Box; visitada = false}
              | Storage -> newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos + 1) <- {tipo = Box_Located; visitada = false}
              | _ -> raise (Invalid_movement "mover Right: la caja va a una zona ilegal")
            )
          )
        | Box_Located -> (
            (*Se empuja una caja que ya estaba en el sitio*)
            newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos) <- {tipo = Player_on_storage; visitada = false};
            (*Comprobar a dónde va a ir la caja*)
            (
              match newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos + 1).tipo with
                Floor -> newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos + 1) <- {tipo = Box; visitada = false}
              | Storage -> newtab.panel.(fst newtab.player_pos).(snd newtab.player_pos + 1) <- {tipo = Box_Located; visitada = false}
              | _ -> raise (Invalid_movement "mover Right: la caja va a una zona ilegal")
            )
          )
        | _ -> raise (Invalid_movement "mover Right: el jugador va a ir a una zona ilegal")
      );
      newtab
    )
;;

let movimiento_valido = function
    [] -> raise (Invalid_board "movimiento_valido con lista de tableros vacia")
  | tabl::stk_tabl ->
    function
      Up -> (
        match (tabl.panel.((fst tabl.player_pos)-1).(snd tabl.player_pos).tipo) with
          Wall -> false
        | Box | Box_Located -> (
            if ((fst tabl.player_pos) <2) then false
            else (
              match (tabl.panel.((fst tabl.player_pos)-2).(snd tabl.player_pos).tipo) with
                Wall | Box | Box_Located -> false
              | Floor | Storage -> not (tabl_existed (mover tabl Up) stk_tabl)
              | Player | Player_on_storage -> raise (Invalid_board "movimiento_valido Up: se va a empujar una caja y va a ir encima del jugador")
            )
          )
        | Floor | Storage -> not (tabl_existed (mover tabl Up) stk_tabl)
        | Player | Player_on_storage -> raise (Invalid_board "movimiento_valido Up: la celda a donde se va a mover es de tipo Player / Player_on_storage")
      )
    | Down -> (
        match (tabl.panel.((fst tabl.player_pos)+1).(snd tabl.player_pos).tipo) with
          Wall -> false
        | Box | Box_Located -> (
            if ((fst tabl.player_pos) > (fst tabl.size - 3)) then false
            else (
              match (tabl.panel.((fst tabl.player_pos)+2).(snd tabl.player_pos).tipo) with
                Wall | Box | Box_Located -> false
              | Floor | Storage -> not (tabl_existed (mover tabl Down) stk_tabl)
              | Player | Player_on_storage -> raise (Invalid_board "movimiento_valido Down: se va a empujar una caja y va a ir encima del jugador")
            )
          )
        | Floor | Storage -> not (tabl_existed (mover tabl Down) stk_tabl)
        | Player | Player_on_storage -> raise (Invalid_board "movimiento_valido Down: la celda a donde se va a mover es de tipo Player / Player_on_storage")
      )
    | Left -> (
        match (tabl.panel.(fst tabl.player_pos).((snd tabl.player_pos)-1).tipo) with
          Wall -> false
        | Box | Box_Located -> (
            if ((snd tabl.player_pos) < 2) then false
            else (
              match (tabl.panel.(fst tabl.player_pos).((snd tabl.player_pos)-2).tipo) with
                Wall | Box | Box_Located -> false
              | Floor | Storage -> not (tabl_existed (mover tabl Left) stk_tabl)
              | Player | Player_on_storage -> raise (Invalid_board "movimiento_valido Left: se va a empujar una caja y va a ir encima del jugador")
            )
          )
        | Floor | Storage -> not (tabl_existed (mover tabl Left) stk_tabl)
        | Player | Player_on_storage -> raise (Invalid_board "movimiento_valido Left: la celda a donde se va a mover es de tipo Player / Player_on_storage")
      )
    | Right -> (
        match (tabl.panel.(fst tabl.player_pos).((snd tabl.player_pos)+1).tipo) with
          Wall -> false
        | Box | Box_Located -> (
            if ((snd tabl.player_pos) > (snd tabl.size -3)) then false
            else (
              match (tabl.panel.(fst tabl.player_pos).((snd tabl.player_pos)+2).tipo) with
                Wall | Box | Box_Located -> false
              | Floor | Storage -> not (tabl_existed (mover tabl Right) stk_tabl)
              | Player | Player_on_storage -> raise (Invalid_board "movimiento_valido Right: se va a empujar una caja y va a ir encima del jugador")
            )
          )
        | Floor | Storage -> not (tabl_existed (mover tabl Right) stk_tabl)
        | Player | Player_on_storage -> raise (Invalid_board "movimiento_valido Right: la celda a donde se va a mover es de tipo Player / Player_on_storage")
      )
;;

let tablero_resuelto tabl =
  try
    for i = 0 to (fst tabl.size - 1) do
      for j = 0 to (snd tabl.size - 1) do
        if tabl.panel.(i).(j).tipo = Box then raise Not_found
      done;
    done;
    true
  with Not_found -> false
;;

let rec recorrer stk_tabl accmov lvl =
  match stk_tabl with
    [] -> raise (Invalid_board "recorrer lista vacia")
  | current::hist -> (
      if tablero_resuelto current then (List.rev accmov, lvl)
      else (
        if movimiento_valido stk_tabl Up then
          recorrer ((mover current Up)::stk_tabl) (("Up : level" ^string_of_int lvl)::accmov) (lvl+1)
        else (List.rev accmov, lvl);
        if movimiento_valido stk_tabl Down then
          recorrer ((mover current Down)::stk_tabl) (("Down : level"^string_of_int lvl)::accmov) (lvl+1)
        else (List.rev accmov, lvl);
        if movimiento_valido stk_tabl Left then
          recorrer ((mover current Left)::stk_tabl) (("Left : level "^string_of_int lvl)::accmov) (lvl+1)
        else (List.rev accmov, lvl);
        if movimiento_valido stk_tabl Right then
          recorrer ((mover current Right)::stk_tabl) (("Right : level"^string_of_int lvl)::accmov) (lvl+1)
        else (List.rev accmov, lvl)
      ))
;;

let rec print_list = function
    [] -> ()
  | h::t -> print_endline h; print_list t;;

print_list (fst (recorrer [parse (open_in Sys.argv.(1))] [] 0));;
