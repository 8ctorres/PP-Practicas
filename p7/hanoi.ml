let rec hanoi (t1,t2,t3) ndiscos =
    if ndiscos<=0 then raise (Invalid_argument "hanoi")
    else if ndiscos=1 then [(t1,t3)]
    else List.append (hanoi (t1,t3,t2) (ndiscos-1)) ((t1,t3)::hanoi (t2,t1,t3) (ndiscos-1));;
