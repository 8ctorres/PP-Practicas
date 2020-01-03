type 'a g_tree = Gt of 'a * 'a g_tree list;;

let rec breadth_first = function
    Gt (x, []) -> x::[]
  | Gt (x, (Gt(y,t2))::t1) -> x::breadth_first (Gt (y,t1@t2));;

let arbolito = Gt (1,
   [Gt (2, [Gt (5, []); Gt (6, [])]); Gt (3, [Gt (7, [])]);
    Gt (4, [Gt (8, []); Gt (9, [])])]);;

(*Arbolito es el árbol cuyo recorrido en anchura da la lista ordenada del 1 al 9*)

let breadth_first_t arbol =
  let rec aux acc = function
      Gt (x, []) -> List.rev (x::acc)
    | Gt (x, Gt(raiz, ramas)::lista) ->
      aux (x::acc) (Gt(raiz, List.rev_append (List.rev lista) ramas))
  in
  aux [] arbol;;


(*Defino una función init porque mi versión de ocaml es anterior a la
4.06.0. Podría usarse directamente List.init en versiones posteriores*)
let init = function n -> function f ->
  let rec aux (f,i,l) = match (f,i,l) with
      (_,0,l) -> (f i)::l
    |(f,i,lst) -> aux(f,(i-1),(f i)::lst)
  in
  aux(f,n-1,[]);;

let id x = x;;
let leaf v = Gt(v,[]);;
let init_tree n = Gt(n, List.rev_map leaf (init n id));;

let t = init_tree 500_000;;
