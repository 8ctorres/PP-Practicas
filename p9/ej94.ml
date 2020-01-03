open Gtree;;
open Bintree;;

let gleaf a = Gt(a, []);;

let arb2 = Gt(2, [gleaf 5; gleaf 6]);;

let arb4 = Gt(4, [gleaf 7]);;

let arbolito = Gt(1, [arb2; gleaf 3; arb4]);;

let rec cod_as_bin arbol =
  let grev arb = match arb with
      Gt(raiz, lista) -> Gt(raiz, List.rev lista)
  in
  let rec create_sibl b a = match (grev a) with
      Gt(raiz, []) -> Node(raiz, Empty, b)
    | Gt(raiz, rama1::[]) -> Node(raiz, cod_as_bin rama1, b)
    | Gt(raiz, rama1::ramas) ->
      Node(raiz,
           (List.fold_left create_sibl (cod_as_bin rama1) ramas), b)
  in
  match (grev arbol) with
    Gt(raiz, []) -> Node(raiz, Empty, Empty)
  | Gt(raiz, rama1::[]) -> Node(raiz, cod_as_bin rama1 , Empty)
  | Gt(raiz, rama1::ramas) ->
    Node(raiz,
         List.fold_left create_sibl (cod_as_bin rama1) ramas,
         Empty);;

let decod_from_bin arbol =
  let rec aux acc arb = match arb with
      Empty -> List.rev acc
    | Node(raiz, izq, der) -> aux ((Gt(raiz, aux [] izq))::acc) der
  in
  match arbol with
    Empty -> raise(Invalid_argument "decod_from_bin")
  | Node(raiz, izq, der) -> Gt(raiz, (aux [] izq));;

