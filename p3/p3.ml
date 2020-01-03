(*EJERCICIO 1*)

let f = function x -> function y -> function z ->
        (z > y) || ((x<>y) && (z / (x-y) >y));;

let f2 = function x,y,z -> if (z > y) then true else
        if (x<>y) then (z / (x-y) > y ) else
        false;;

false && (2 / 0 > 0);;
(*- : bool = false*)

(* true && (2 / 0 > 0);;
#Excepción: División entre cero *)

true || (2 / 0 > 0);;
(* - : bool = true *)

(* false || (2 / 0 > 0)
Excepción: División entre cero *)

let con = (&&);;
(* val con : bool -> bool -> bool = <fun>*)

let dis = (||);;
(* val dis : bool -> bool -> bool = <fun>*)

(&&) (1<0) (2 / 0 > 0);;
(*- : bool = false*)

(*con (1<0) (2 / 0 > 0);;
Excepción: División entre cero *)

(||) (1>0) (2 / 0 > 0);;
(* - : bool = true*)

(*dis (1>0) (2 / 0 > 0);;
Excepción: División entre cero*)

(*EJERCICIO 2*)

let curry = function f -> function a -> function b -> f(a,b);;

let curry f a b = f (a,b);;

let uncurry = function f -> function (a,b) -> f a b;;

let uncurry f(a,b) = f a b;;

uncurry (+);;
(* - : int * int -> int = <fun >*)

let sum = uncurry (+);;
(* val sum : int * int -> int = <fun> *)

(* sum 1;;
Error de tipos. "sum" espera un par de ints (int*int) *)

sum (2,1);;
(* -: int = 3*)

let g = curry (function p -> 2 * fst p + 3 * snd p);;
(* val g : int -> int -> int = <fun> *)

(*g (2,5);;*)
(*Error de tipos. G espera dos int, no un par de (int*int) *)

let h = g 2;;
(* val h : int -> int = <fun>*)

h 1, h 2, h 3;;
(* - : int * int * int = (7,10,13) *)

(*EJERCICIO 3*)

let comp f g x = f (g x);;

let f2 = let square x = x*x in comp square ((+) 1);;
(* val f2: int -> int = <fun>*)

f2 1, f2 2, f2 3;;
(* - : int * int * int = (4 , 9, 16) *)