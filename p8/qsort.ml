(*Pŕactica 8 Ejercicio 1*)

open List;;

let rec qsort1 ord = function
    [] -> []
  | h::t -> let after, before = partition (ord h) t
    in qsort1 ord before @ h :: qsort1 ord after;;

(*
  Si el primer elemento de la lista es de los más cercanos al menor o al mayor
  porque en ese caso, partition devolverá dos listas muy desiguales en
  longitud. Uno de los casos en los que pasaría esto sería si la lista
  estuviese ya ordenada.
*)

let rec qsort2 ord = function
    [] -> []
  | h::t -> let after, before = partition (ord h) t
    in rev_append (rev (qsort2 ord before)) (h::qsort2 ord after);;

(*
  Qsort2 tiene la ventaja sobre qsort1 de ser terminal. Cualquier lista que
  se pueda ordenar con qsort1 podrá ordenarse con qsort2 y viceversa
*)

let l1 = [];;

(*
  Qsort2 tiene la desventaja sobre qsort1 de ser más lento porque tiene que
  hacer la operación rev sobre la primera lista y luego hacer la operación
  rev_append sobre las dos listas.

  qsort1 sobre una lista de 200_000 elementos tarda 0.38 segundos
  qsort2 sobre la misma lista tarda 0.43

  qsort2 es aproximadamente un 13% más lenta que qsort1
*)

let qsort3 ord l =
  let rec sort_asc = function 
     [] -> []
    | h::t -> let men,may = List.partition (fun x -> ord x h) t in
      List.rev_append (sort_desc men) (h::sort_asc may)
  and sort_desc = function
      [] -> []
    | h::t -> let men,may = List.partition (fun x -> not(ord x h)) t in
      List.rev_append (sort_asc men) (h::sort_desc may)
  in sort_asc l;;

(*
  qsort1 sobre una lista de 200_000 elementos tarda 0.38 segundos
  qsort2 sobre la misma lista tarda 0.43
  qsort3 sobre la misma lista tarda 0.39 segundos

  Es decir, qsort3 es aproximadamente un 1.05% más lento que qsort1, pero
  es recursivo terminal.
  Al mismo tiempo es un 10% más rápido que qsort2

  Por tanto, qsort3 es la mejor implementación de las 3 porque qsort1 a pesar
  de ser marginalmente más rápida, está limitada a listas de hasta aprox.
  200 mil elementos (dado que listas más grandes producen stack overflow)
*)
