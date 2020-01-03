(*
    Paradigmas de Programación
    TGR 1

    Carlos Torres Paz
    carlos.torres@udc.es

*)

open Z;;

let n =      of_string (Sys.argv.(1));; (*posición de la sucesión a calcular*)
let umbral = of_int 75000;; (*umbral por debajo del cual se cambia de método*)
                            (*Probado empíricamente que el valor idóneo se encuentra aproximadamente en 75.000 *)

let mfib = [[one;one];[one;zero]];; (*Represento las matrices como listas de listas de enteros (int list list) *)
let mzero = [[zero;zero];[zero;zero]];;

let mat_prod mat1 mat2 =
    let col1 mat = List.map List.hd mat in (*Función que devuelve la primera columna de una matriz*)
    let rec map_cols f mat = if List.mem [] mat then []
                             else f (col1 mat) :: map_cols f (List.map List.tl mat) in (*map_cols aplica una función f a todas las columnas de una matriz*)
        List.map (fun row -> map_cols (fun col ->
        List.fold_left add zero (List.map2 mul row col)) mat1) mat2
;;

let mat_sq mat = mat_prod mat mat;; (*Eleva una matriz al cuadrado*)

let rec mat_pow mat pow = if pow = one then mat (*Potenciación de matrices*)
                          else if (pow mod ~$2)=zero then
                            let t = mat_pow mat  (pow/(~$2)) in
                            mat_sq t
                          else
                            let t = mat_pow mat (pow/(~$2)) in
                            mat_prod mat (mat_sq t)
;;

let fibp n = if n=zero then (zero,one) (*Método 2*)
            else if n=one then (one,zero)
            else
              let sq = sqrt n in
              let fibmat = List.hd (List.tl (mat_prod (mat_pow mfib sq) (mat_pow mfib (n-sq)))) in
              (List.nth fibmat 0, List.nth fibmat 1)
;;

let rec fib n = if n=zero then zero (*Método 1*)
                else if n=one then one (*Caso base*)
                else
                    if ((n mod ~$2)=zero) then (*Caso para n pares*)
                        let k = n /| (~$2) in
                        if k < umbral then (*Si k está por debajo del umbral, cambio al método 2*)
                            let fibk,fibpk = fibp(k)
                                in
                            (~$2*fibpk + fibk)*fibk (*Aplico la fórmula 1*)
                        else
                            let fibk = fib(k)  (*Almaceno el valor fib(k) para evitar calcularlo dos veces *)
                                in
                            (~$2*fib(pred k) + fibk)*fibk (*Aplico la fórmula 1*)
                    else    (*Caso para n impares*)
                        let k = (succ n) /| ~$2 in
                        if k < umbral then  (*Si k está por debajo del umbral, cambio al método 2*)
                            let fibk,fibpk = fibp(k) in
                            fibk**2 + fibpk**2  (*Aplico la fórmula 1*)
                        else 
                            fib(k)**2 + fib(pred k)**2  (*Aplico la fórmula 1*)
;;

print (fib n);;
print_endline "";;