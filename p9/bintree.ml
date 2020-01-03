type 'a bin_tree = Empty | Node of 'a * 'a bin_tree * 'a bin_tree;;

let breadth_first arbol =
  let rec aux i = function
      Empty -> []
    | Node(raiz, Empty, Empty) -> [(raiz,i)]
    | Node(raiz, rama1, rama2) -> ((raiz,i)::(aux (i+1) rama1)) @ (aux (i+1) rama2)
  in
  List.map (fun (a,b) -> a)
    (List.stable_sort (function (a,b) -> function (c,d) -> b-d)
         (aux 0 arbol));;
