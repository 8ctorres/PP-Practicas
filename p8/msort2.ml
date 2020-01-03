(*Práctica 8 Ejercicio 2*)

let rec split = function
    [] -> [],[]
  | h::[] -> [h],[]
  | h1::h2::t -> let t1,t2 = split t in (h1::t1,h2::t2);;

let rec merge ord l1 l2 = match l1,l2 with
    [],l | l,[] -> l
  | h1::t1, h2::t2 -> if ord h1 h2 then h1::merge ord t1 l2
                      else h2::merge ord l1 t2;;

let rec msort1 ord l = match l with
    [] | _::[] -> l
  | _ -> let l1, l2 = split l in
    merge ord (msort1 ord l1) (msort1 ord l2);;

(*
   La no terminalidad de split y merge provoca stack overflow con listas de
   sólo 100_000 elementos, cuyos tiempos de ordenación serían razonables si
   las funciones fuesen terminales.
*)

(*Defino una implementación de init porque no tengo la última versión
de ocaml. Podría usarse directamente List.init en la definición de l2*)
let init = function n -> function f ->
  let rec aux (f,i,l) = match (f,i,l) with
      (_,0,l) -> (f i)::l
    |(f,i,lst) -> aux(f,(i-1),(f i)::lst)
  in
  aux(f,n-1,[]);;

let l2 = init 100_000 (fun _ -> Random.int 100_000);;

let split_t lst =
  let rec aux acc1 acc2 = function
      [] -> acc1, acc2
    | h::[] -> h::acc1, acc2
    | h1::h2::t -> aux (h1::acc1) (h2::acc2) t
  in
  aux [] [] lst;;

let merge_t ord l1 l2 =
  let rec aux ls1 ls2 acc = match ls1, ls2 with
      [],l | l,[] -> List.rev_append acc l
    | h1::t1, h2::t2 -> if ord h1 h2 then aux t1 ls2 (h1::acc)
                        else aux ls1 t2 (h2::acc)
  in
  aux l1 l2 [];;

let rec msort2 ord l = match l with
    [] | _::[] -> l
  | _ -> let l1,l2 = split_t l in
    merge_t ord (msort2 ord l1) (msort2 ord l2);;

(*
   Para una lista generada aleatoriamente y de tamaño 80_000:
   tiempo con msort1 0.23 segundos
   tiempo con msort2 0.14 segundos
   tiempo con qsort3 0.31 segundos

   Para una lista generada aleatoriamente de tamaño 1 millón:
   msort1 da stack overflow
   tiempo con msort2 2.78 segundos
   tiempo con qsort3 3.21 segundos

   msort2 es aproximadamente un 60% más rápida que msort1, además de ser terminal
   msort2 es aproximadamente un 15% más rápida que qsort3, siendo ambas terminales
*)
