let hd =
  function a::_ -> a
  | [] -> raise (Failure "hd");;

let tl = 
  function _::a -> a
  | [] -> raise (Failure "tl");;

let rec length = function
  [] -> 0
  | a -> (1 + (length (tl a)));;

let rec compare_lengths = function a -> function b -> 
  if length (a) > length (b) then 1
  else if length (a) < length(b) then -1
  else 0;;

let rec nth = function lst -> function n ->
  if n<(length (lst)) then
    if n=0 then (hd lst)
    else if n>0 then nth (tl lst) (n-1)
    else raise (Invalid_argument "nth")
  else raise (Failure "nth");;

let rec append = function
  [] -> (function l2 -> l2)
  | h1::t1 -> ( function l2 ->
    if (t1 == []) then
      h1::l2
    else h1::(append t1 l2) );;

let rec init = function n -> function f ->
  if n=1 then
    (f (n-1))::[]
  else
    append (init (n-1) f) ((f (n-1))::[]);;

let rec rev = 
  function [] -> []
| h::t ->
  if t=[] then
    (h::t)
  else
    append (rev t) (h::[]);;

let rev_append = function l1 -> function l2 ->
  append (rev l1) l2;;

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

let rev_map f l = rev (map f l);;

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

let rec fold_left = function f -> function i -> function lst ->
  if lst = [] then i
    else if (tl lst = []) then
      f i (hd lst)
    else
      f (fold_left f i (tl lst)) (hd lst);;

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
    if ((f h)=false) then false
    else (for_all f t);;

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
    if f h then append (h::[]) (filter f t)
    else (filter f t);;

let find_all = filter;;

let partition = function f ->
  function [] -> ([],[])
  | l -> (let rec find_all_not = function f -> function
      [] -> [] 
     | h::t ->
        if not (f h) then
          append (h::[]) (find_all_not f t)
        else find_all_not f t 
        in let l1 = find_all f l
        in let l2 = find_all_not f l
        in (l1,l2));;

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

