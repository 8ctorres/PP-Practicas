open List;;

let split = function
  [] -> ([],[])
  |h::t -> (map fst (h::t)),(map snd (h::t));;

let combine l1 l2 = match l1,l2 with
  [],[] -> []
  |h1::t1, h2::t2 -> let aux = function a -> function b -> (a,b) in
    (try (map2 aux (h1::t1) (h2::t2))
     with (Invalid_argument _) ->
      raise(Invalid_argument "concat"))
  | _ -> raise(Invalid_argument "concat");;

let length = function
  [] -> 0
  |h::t -> let aux = function a -> function _ -> a+1 in
    fold_left aux 0 (h::t);;

let append l1 l2 = match l1,l2 with
  [],[] -> []
  |h::t,[] -> h::t
  |[],h::t -> h::t
  |h1::t1,h2::t2 -> let aux = function b -> function a -> b::a in
  fold_right aux (h1::t1) (h2::t2);;

let rev = function
  [] -> []
  |h::t -> let aux = function a -> function b -> b::a in
    fold_left aux [] (h::t);;


let concat_left = function (*concat usando fold_left*)
  [] -> []
  |h::t -> let aux = function b -> function an -> b@an in
    fold_left aux [] (h::t);;


let concat = function (*concat usando fold_right*)
  [] -> []
  | h::t -> let aux = function an -> function b -> an@b in
    fold_right aux (h::t) [];;

(*Ambas funciones concat_right y concat_left hacen N llamadas a su función aux, siendo N el número de elementos de la lista de entrada
  Pero la complejidad de la función auxiliar de concat_left es O(n), siendo n el número de elementos totales de la salida, mientras
  que la complejidad de la función auxiliar de concat_right es O(n), siendo n el número de elementos de la lista más grande dentro de
  la lista de entrada.

  Por tanto la complejidad de concat_right es O(n*m), siendo N el número de elementos de la entrada, y M la longitud de la mayor de los
  elementos de la entrada

  La complejidad de concat_left es O(n^2), siendo N el número de elementos de la entrada.

  En definitiva, concat_right es, en el peor caso, igual de eficiente que concat_left; y mejor en cualquier otro caso

  Ninguna de las dos es recursiva terminal, porque ambas usan el operador infijo @ en sus auxiliares, y el @ NO es recursivo terminal

  Una forma sencilla de convertir concat_left a recursiva terminal sería sustituir en su auxiliar el b@an por un rev_append (rev b) a
*)

let concat_left_terminal = function (*concat usando fold_left y recursiva terminal*)
  [] -> []
  |h::t -> let aux = function b -> function an -> rev_append (rev b) an in
    fold_left aux [] (h::t);;

let partition = function f -> function
  [] -> ([],[])
  |h::t -> let aux f x = not (f x) in
    (filter f (h::t), filter (aux f) (h::t));;

let remove_all = function a -> function
  [] -> []
  |h::t -> let aux a x = not (a=x) in
  filter (aux a) (h::t);;

let ldif l1 l2 = match l1,l2 with (*ldif usando fold_left*)
([],_) -> []
|(h::t, []) -> h::t
|(h1::t1,h2::t2) -> let aux l1 a = remove_all a l1 in
    fold_left aux l1 l2;;

let ldif_right l1 l2 = match l1,l2 with (*ldif usando fold_right*)
  ([],_) -> []
  |(h::t, []) -> h::t
  |(h1::t1,h2::t2) -> fold_right remove_all l2 l1;;

(*Ambas son de complejidad O(n), siendo N la longitud de la lista l2
  ldif_left es tail recursive y ldif_right no lo es*)

let lprod l1 l2 = match l1,l2 with
  ([],_) -> []
 |(_,[]) -> []
 |(h1::t1, h2::t2) -> let par = function h -> function x -> (h,x) in
                      let aux = function l2 -> function h -> map (par h) l2 in
                      concat (map (aux l2) l1);;

let rec lprod l1 l2 =
  let rec sprod s  = List.map (fun x -> s,x) in
  List.concat (List.map (fun x -> sprod x l2) l1);;

let comp f g x = f (g x);; (*Función comp de la P3*)

let multicomp l = fold_left comp (fun x -> x) l;;

