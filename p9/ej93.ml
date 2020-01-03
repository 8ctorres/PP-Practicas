(*En este caso he implementado las funciones strict, perfect y complete de forma que al aplicarse al árbol vacío, devuelven false. El enunciado de la práctica no especifica qué deben devolver las funciones en ese caso*)

open Bintree;;

let rec strict arb = match arb with
    Empty -> false
  | Node (raiz, Empty, Empty) -> true
  | Node (raiz, Empty, Node (r,r1,r2)) -> false
  | Node (raiz, Node (r,r1,r2), Empty) -> false
  | Node (raiz, rama1, rama2) -> (strict rama1) && (strict rama2);;

let perfect arb =
  let rec aux acc arb = match arb with
      Empty -> (acc, false)
    | Node (raiz, Empty, Empty) -> (acc+1, true)
    | Node (raiz, Empty, Node (r,r1,r2)) -> (acc+1, false)
    | Node (raiz, Node (r,r1,r2), Empty) -> (acc+1, false)
    | Node (raiz, rama1, rama2) ->
      let n1,b1 = (aux acc rama1)
      and n2,b2 = (aux acc rama2) in
      (acc,((n1 = n2) && (b1 && b2))) in
  snd (aux 0 arb);;

let leaf a = Node (a,Empty,Empty);;

let arbolito = Node (1,
                     Node (2, Node (4, leaf 8, leaf 9),
                           Node (5, leaf 10, Empty)),
                     Node (3, leaf 6, leaf 7));;

(*arbolito es un árbol binario no estricto, no perfecto pero completo
Lo usé para probar las funciones*)

let complete arbol =
  let recorrer arb =
    let rec aux i = function
        Empty -> [(None, i)]
      | Node(raiz, Empty, Empty) -> [(Some raiz,i)]
      | Node(raiz, rama1, rama2) ->
        ((Some raiz,i)::(aux (i+1) rama1)) @ (aux (i+1) rama2)
    in
    List.map (fun (a,b) -> a)
      (List.stable_sort (function (a,b) -> function (c,d) -> b-d)
           (aux 0 arb)) in
  let filtrar lsta =
    let rec aux flag lst = match lst with
        [] -> false
      | None::[] -> true
      | (Some _)::[] -> not flag
      | None::t -> aux (true) t
      | (Some _)::t -> if (flag=false) then aux (flag) t else false
    in
    aux false lsta
  in
  match arbol with
    Empty -> false
  | _ -> filtrar (recorrer arbol);;
