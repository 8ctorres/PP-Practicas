(*Función con tipo 'a -> 'a*)
(*Solo existe una función con este tipo*)

let f x = x;;

(*Función con tipo 'a -> 'a * 'a *)
(*Solo existe una función con este tipo*)

let g x = x,x;;

(*Función con tipo ('a * 'b ) -> 'a*)
(*Existen dos formas de escribir la función con este tipo, que realmente son la misma función
  porque fst x es lo mismo que escribir directamente la primera componente de un par*)

let h (a,b) = a;;
let h_2 x = fst x;;

(*Función con tipo ('a * 'b ) -> 'b *)
(*Existen dos formas de escribir la función con este tipo, que realmente son la misma función
  porque snd x es lo mismo que escribir directamente la segunda componente de un par*)

let i (a,b) = b;;
let i_2 x = snd x;;

(*Función con tipo 'a -> 'a list*)
(*Solo existe una función con este tipo*)

let j a = a::[];;

(**************************************)

(*Función con tipo 'a -> 'b*)
(*Se pueden escribir infinitas funciones de este tipo, pero todas tienen en común que no devuelven nada porque lanzan una excepción*)

let k a = raise(Failure "a");;

(*Función con tipo 'a -> 'b list*)
(*Se pueden escribir infinitas funciones de este tipo, pero todas tienen en común que no devuelven nada porque lanzan una excepción*)

let l a = raise(Failure "a")::[];;

(***************************************

let f_123_ab f = f[1;2;3] + f['a';'b'];;

  Esta función no es posible porque al aplicar f sobre [1;2;3] y sumar ese resultado se fuerza a que f sea del tipo (int list -> int),
  y al aplicar f sobre ['a';'b'] se fuerza a que f sea de tipo (char list -> int), que no es compatible con el (int list -> int), porque
  int no es compatible con char

*)
