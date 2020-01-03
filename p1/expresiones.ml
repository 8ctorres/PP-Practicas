();;
(* - : unit *)

2 + 5 * 3;;
(* - : int = 17 *)

1.0;;
(* - : float = 1.0 *)

(* 1.0 * 2;; *)
(* - : float = 2.0 *)
(* Da un error porque el operador * es solo para enteros *)
1.0 *. 2.0;;

(* 2 - 2.0;; *)
(* Error porque el operador - es solo para enteros *)
2.0 -. 2.0;;
(*Otra opción:*)
2 - 2;;

(* 3.0 + 2.0;; *)
(* Error porque el operador + es solo para enteros*)
3.0 +. 2.0;;

5 / 3;;
(* - int : 1 *)

5 mod 3;;
(* - int : 2 *)

3.0 *. 2.0 ** 3.0;;
(* - : float = 24.0 *)

3.0 = float_of_int 3;;
(* - : bool = true *)

(* sqrt 4;; *)
(* - : float = 2. *)
(* Error: la función sqrt es de tipo float->float y el 4 es int*)
sqrt 4.;;

int_of_float 2.1 + int_of_float (-2.9);;
(* - : int = 0 *)

truncate 2.1 + truncate (-2.9);;
(* - : int = 0 *)

floor 2.1 +. floor (-2.9);;
(* - : float = -1. *)

ceil 2.1 +. ceil (-2.9);;
(* - : float = 1. *)

'B';;
(* - : char = 'B' *)

int_of_char 'A';;
(* - : int = 65 *)

char_of_int 66;;
(* - : char = 'B' *)

Char.code 'B';;
(* - : int = 66 *)

Char.chr 67;;
(* - : char 'C' *)

'\067';;
(* - : char = 'C' *)

Char.chr (Char.code 'a' - Char.code 'A' + Char.code 'Ñ');;
(* - : char = '\197' *)
(* OJO! Si el encoding de texto es Unicode UTF-8, ocaml reconoce la Ñ como dos caracteres distintos y devuelve un error *)

Char.uppercase 'ñ';;
(* - : char = '\209'*)

Char.lowercase 'O';;
(* - : char = 'o' *)
(* Además sale un warning recomendando el uso de Char.lowercase_ascii
sobre Char.lowercase *)

"this is a string";;
(* - : string = "this is a string" *)

String.length "longitud";;
(* - : int = 8*)

(* "1999" + "1";;*)
(* - : string = "19991" *)
(* Error de tipo: + es un operador solo para enteros,
no sirve para concatenar strings*)

"1999" ^ "1";;
(* - : string = "19991" *)(* ^ es el operador de concatenación de
strings*)

int_of_string "1999" + 1;;
(* - : int = 2000 *)

"\064\065";;
(* - : string = "@A"*)

string_of_int 010;;
(* - : string = "10" *)

not true;;
(*- : bool = false*)

true && false;;
(*- : bool = false*)

true || false;;
(*- : bool = true*)

(1 < 2) = false;;
(*- : bool = false*)

"1" < "2";;
(*Error de tipos, no se puede comparar dos strings con < o >*)
(* - : bool = true;*)
(*Sí se pueden comparar strings con < o >*)

2 < 12;;
(* - : bool = true;*)

"2" < "12";;
(* - : bool = false*)
(*Es false porque el primer caracter del string "2" es MAYOR que el
primer caracter del string "12"*)

"uno" < "dos";;
(* - bool = false*)

2,5;;
(*Dará error porque los float se separan con . decimal, no con , decimal*)
(* - : int * int = (2,5) *)
(* Dos elementos separados por comas forman una lista *)

"hola", "adios";;
(* - : string * string = ("hola", "adios") *)

0,0.0;;
(* - : int * float = (0,0.0)*)

fst ('a', 0);;
(* - : char = 'a'*)

snd (false, true);;
(* - : bool = true*)

(1,2,3);;
(* - : int * int * int = (1,2,3)*)

(1,2),3;;
(* - : (int * int) * int = ((1,2),3)*)

fst ((1,2),3);;
(* - : int * int = (1,2)*)

if 3=4 then 0 else 4;;
(* - : int = 4*)

if 3=4 then "0" else "4";;
(* - : string = "4"*)

(* if 3=4 then 0 else "4";; *)
(* - : string = "4"*)
(*Error de tipos. Las dos posibles salidas deben ser del mismo tipo*)

(if 3<5 then 8 else 10) + 4;;
(* - : int = 12 *)

2.0 *. asin 1.0;;
(* - : float = 3.14159*)

sin (2.0 *. asin 1.0 /. 2.);;
(* - : float = 1. *)

function x -> 2 * x;;
(* - int -> int = <fun>*)

(function x -> 2 * x) (2+1);;
(* - : int = 6*)

function (x,y) -> 2 * x + y;;
(* - : int * int -> int = <fun>*)

(function (x,y) -> 2 * x + y) (1+2,3);;
(* - : int = 9*)

(*
Carlos Torres
carlos.torres@udc.es
PP Grupo 4.3
*)
