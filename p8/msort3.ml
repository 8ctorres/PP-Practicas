let split_firsts n l =
  let n1 = (n+1) /2 and n2 = n /2 in
  let rec cut i l =
    if i>0 then cut (i-1) (List.tl l)
    else l
  in (n1, l, n2, cut n1 l);;

let merge_t ord l1 l2 =
  let rec aux ls1 ls2 acc = match ls1, ls2 with
      [],l | l,[] -> List.rev_append acc l
    | h1::t1, h2::t2 -> if ord h1 h2 then aux t1 ls2 (h1::acc)
      else aux ls1 t2 (h2::acc)
  in
  aux l1 l2 [];;

let msort3 ord l =
  let rec msort3_aux n l = match n with
      0 -> []
    | 1 -> (List.hd l)::[]
    | _ -> let n1,l1,n2,l2 = split_firsts n l
      in
      merge_t ord (msort3_aux n1 l1) (msort3_aux n2 l2)
  in
  msort3_aux (List.length l) l;;
