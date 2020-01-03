type symbol =
    Chr of char
  | Range of (char * char);;

type regexp =
    Empty
  | EmptyStr
  | Sym of symbol
  | Exc of symbol
  | Any
  | Ccat of regexp * regexp
  | Rept of regexp
  | Or of regexp * regexp
  | And of regexp * regexp;;

let symbol_of_char c = Chr c;;

let symbol_of_range a b = Range (a,b);;

let empty = Empty;;

let empty_string = EmptyStr;;

let single s = Sym s;;

let except s = Exc s;;

let any = Any;;

let concat r1 r2 = Ccat (r1,r2);;

let repeat r = Rept r;;

let alt r1 r2 = Or (r1,r2);;

let all r1 r2 = And (r1,r2);;

