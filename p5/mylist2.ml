let hd =
  function a::_ -> a
  | [] -> raise (Failure "hd");;

let tl = 
  function _::a -> a
  | [] -> raise (Failure "tl");;

let length = function lst ->
    let rec aux = function
      ([],i) -> i;
     |(h::t,i) -> aux (t,i+1)
  in aux(lst, 0);;

let compare_lengths = function l1 -> function l2 ->
  let rec aux = function
    ([],[]) -> 0
   |([],_) -> 1
   |(_,[]) -> -1
   |(h::t,h2::t2) -> aux (t,t2)
  in aux(l1,l2);;

let nth = function lst -> function n ->
  let rec aux = function
    [] -> (function n -> raise(Failure "nth"))
    |h::t -> (function 0 -> h | n -> aux t (n-1))
  in
  if n<0 then raise (Invalid_argument "nth")
  else aux lst n;;

let rec append = function
  [] -> (function l2 -> l2)
  | h1::t1 -> (function l2 -> h1::(append t1 l2));;

let init = function n -> function f ->
  let rec aux (f,i,l) = match (f,i,l) with
    (_,0,l) -> (f i)::l
   |(f,i,lst) -> aux(f,(i-1),(f i)::lst)
   in
   aux(f,n-1,[]);;

let rev = function lst ->
    let rec aux (a,b) = match (a,b) with
    ([],[]) -> []
    |(a,[]) -> a
    |(a,hb::tb) -> aux (hb::a , tb)
    in
    aux ([],lst);;
    
let rev_append = function lst1 -> function lst2 -> 
  let rec aux (a,b) = match (a,b) with
  ([],[]) -> []
  |(a,[]) -> a
  |(a,hb::tb) -> aux (hb::a , tb)
  in
  aux (lst2, lst1);;

let rec concat = function 
  [] -> []
  | h::t ->
  append h (concat t);;

let flatten = concat;;

let rec map = function f -> function
    [] -> []
   |h::t -> (
    if t=[] then
      (f h)::[]
    else
      append ((f h)::[]) (map f t)
      );;

let rev_map = function f -> function lst ->
    let rec aux (lv,f,lst) = match (lv,f,lst) with
    ([],f,[]) -> []
    |(a,f,[]) -> a
    |(a,f,hb::tb) -> aux ((f hb)::a ,f, tb)
    in
    aux ([],f,lst);;

let rec map2 = function f -> function
  h1::t1 -> ( function 
  | [] -> ( [] )
  |h2::t2 ->
  if (length (h1::t1) != length (h2::t2)) then
  raise (Invalid_argument "map2")
  else (
    if (t1 = []) then
    (f h1 h2)::[]
    else
    ((f h1 h2)::[]) @ (map2 f t1 t2) )) 
  | [] -> ( function _ -> [] );;

let fold_left = function f -> function a -> function lst ->
  let rec aux (f,i,lst) = match (f,i,lst) with
    (f,i,[]) -> i
    |(f,i,hb::[]) -> (f i hb)
    |(f,i,hb::tb) -> aux (f, (f i hb), tb)
  in
  aux(f,a, lst);;

let rec fold_right = function f -> function lst -> function i -> 
  if lst = [] then i
    else if (tl lst)=[] then
      (f (hd lst) i)
    else
      f (hd lst) (fold_right f (tl lst) i);;
  
let rec find = function f -> function
  [] -> raise (Not_found)
  | h::t ->
    if f h then h
    else find f t;;

let rec for_all = function f -> function
  [] -> true;
 | h::t ->
    if (f h)=false then false
    else for_all f t;;

let rec exists = function f -> function
  [] -> false
 |h::t ->
    if f h then true
    else exists f t;;

let rec mem = function a -> function
  [] -> false
 |h::t ->
    if a = h then true
    else mem a t;;

let rec filter = function f -> function
  [] -> []
 |h::t -> 
    if f h then rev_append (rev (h::[])) (filter f t)
    else (filter f t);;

let filter = function f -> function lst ->
    let rec aux (f,lv,lst) = match (f,lv,lst) with
    (f,lv,[]) -> []
    |(f,lv,h::[]) -> if (f h) then h::lv else lv
    |(f,lv,h::t) -> if (f h) then aux(f, h::lv, t) else aux(f, lv, t)
    in
    rev (aux(f,[],lst));;

let find_all = filter;;

let partition = function f -> function lst ->
    let rec aux2 (a,b) = (rev a, rev b) in
    let rec aux (f,lv1,lv2,lst) = match (f,lv1,lv2,lst) with
    (f,lv1,lv2,[]) -> ([],[])
    |(f,lv1,lv2,h::[]) -> if (f h) then (h::lv1,lv2) else (lv1,h::lv2)
    |(f,lv1,lv2,h::t) -> if (f h) then aux(f,h::lv1,lv2,t) else aux(f,lv1,h::lv2,t)
    in
    aux2(aux(f,[],[],lst));;

let rec split = function [] -> ([],[]) | lst ->
    let rec fstl = function [] -> [] | h::t -> if t = [] then (fst h)::[] else (fst h)::(fstl t)
    in let rec sndl = function [] -> [] | h::t -> if t = [] then (snd h)::[] else (snd h)::(sndl t)
    in (fstl lst, sndl lst);;

let rec combine = function l1 -> function l2 ->
  if length l1 != length l2 then raise (Invalid_argument "combine")
  else if l1 = [] || l2=[] then []
  else
    if l1 = [] then
      ((hd l1) , (hd l2))::[]
    else
      ((hd l1), (hd l2))::(combine (tl l1) (tl l2));;