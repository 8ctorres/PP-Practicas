let uppercase = function c -> 
  if ((Char.code c >= 97) && (Char.code c <= 122)) then Char.chr (Char.code c - 32)
    else c;;

let lowercase = function c -> 
  if ((Char.code c >= 65) && (Char.code c <= 90)) then Char.chr (Char.code c + 32)
    else c;;