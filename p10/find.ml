let rec recorrer path exp =
  let ficheros = Sys.readdir path in
  for i = 0 to (Array.length ficheros -1) do
    if (Derive.matches_regexp ficheros.(i) exp) then
      print_endline (Filename.concat path (ficheros.(i)))
  done;

  (*Se recorre el array de ficheros dos veces para que primero se muestren todos
  los ficheros del directorio ACTUAL que cumplen la condición, antes de pasar a
  buscar en los subdirectorios. Es decir, se asegura que el arbol de directorios
  se recorre en anchura

  Podría hacerse en un sólo bucle for, y para cada elemento comprobar si es un
  directorio y llamar a la función "recorrer", o si es un fichero y el nombre
  es aceptado por la expresión regular, mostrarlo por pantalla.
    Esta versión no mostraría los ficheros en ningún orden determinado*)

  for i = 0 to (Array.length ficheros -1) do
    if (Sys.is_directory (Filename.concat path ficheros.(i))) then
      recorrer (Filename.concat path (ficheros.(i))) exp
  done

let main () =
  if (Array.length Sys.argv) < 3 then
    print_endline "find path reg_exp"
  else
    (
      try (
        let rexp = Derive.regexp_of_string Sys.argv.(2) in
          recorrer Sys.argv.(1) rexp
      )
      with
        Parsing.Parse_error -> print_endline "Expresión regular errónea"
      | Sys_error c -> print_endline ("Error: "^c )
    );;

main ()
