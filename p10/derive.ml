let regexp_of_string s =
  Regexp_parser.main Regexp_lex.token (Lexing.from_string s);;

open Regexp;;

let rec nullable exp = match exp with
    Empty -> Empty
  | EmptyStr -> EmptyStr
  | Sym a -> Empty
  | Exc a -> Empty
  | Any -> Empty
  | Ccat (r,s) ->
    if ((nullable r = EmptyStr) && (nullable s = EmptyStr)) then EmptyStr
    else Empty
  | Rept r -> EmptyStr
  | Or (r,s) ->
    if ((nullable r = EmptyStr) || (nullable s = EmptyStr)) then EmptyStr
    else Empty
  | And (r,s) ->
    if ((nullable r = EmptyStr) && (nullable s = EmptyStr)) then EmptyStr
    else Empty;;

let rec derive a exp = match exp with
    Empty -> Empty
  | EmptyStr -> Empty
  | Sym (Chr x) ->
    if (x=a) then EmptyStr else Empty
  | Exc (Chr x) ->
    if (x=a) then Empty else EmptyStr
  | Sym (Range (c,d)) ->
    if ((c <= a) && (a <= d)) then EmptyStr else Empty
  | Exc (Range (c,d)) ->
    if ((c <= a) && (a <= d)) then Empty else EmptyStr
  | Any -> EmptyStr
  | Ccat (r,s) ->
    Or ((Ccat ((derive a r), s)), (Ccat((nullable r),(derive a s))))
  | Rept r -> Ccat((derive a r), (Rept r))
  | Or (r,s) -> Or ((derive a r), (derive a s))
  | And (r,s) -> And ((derive a r), (derive a s));;

let rec simplify exp = match exp with
  (*Casos base:*)
    Empty -> Empty
  | EmptyStr -> EmptyStr
  | Sym s -> Sym s
  | Exc s -> Exc s
  | Any -> Any
  (*Otros casos*)
  | Ccat (r,s) -> (
      let r' = simplify r
      and s' = simplify s
      in match (r', s') with
        (Empty, _) -> Empty
      | (_, Empty) -> Empty
      | (EmptyStr, s') -> s'
      | (r', EmptyStr) -> r'
      | _ -> Ccat(r', s'))
  | Rept (r) -> (
      let r' = simplify r
      in match r' with
        Empty -> Empty
      | EmptyStr -> EmptyStr
      | _ -> Rept (r'))
  | Or (r,s) -> (
      let r' = simplify r
      and s' = simplify s
      in match (r',s') with
        (Empty, s')-> s'
      | (r', Empty) -> r'
      | _ -> Or(r',s'))
  | And (r,s) -> (
      let r' = simplify r
      and s' = simplify s
      in match (r',s') with
        (Empty, _) -> Empty
      | (_, Empty) -> Empty
      | _ -> And (r',s'));;


let matches_regexp str exp =
  let list_of_string str =
    let rec aux acc i =
      let l = String.length str in
      if (i < l) then (aux ((str.[i])::acc) (i+1)) else acc
    in
    List.rev (aux [] 0)
  in
  let derive_aux r c = simplify (derive c r)
  in
  (nullable
     (List.fold_left derive_aux (simplify exp) (list_of_string str))
  ) = EmptyStr;;

let matches str1 str2 = matches_regexp str1 (regexp_of_string str2);;
