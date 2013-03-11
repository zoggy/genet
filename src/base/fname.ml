(** *)

type 'a filename = string

let absolute name =
  if Filename.is_relative name then
    failwith (Printf.sprintf "%S is not an absolute filename." name);
  name
;;

let relative name =
 if Filename.is_relative name then
   name
  else
    failwith (Printf.sprintf "%S is not a relative filename." name)
;;

let quote s = Filename.quote s;;
let rel_quote = quote;;
let concat dir f = Filename.concat dir f;;
let concat_s dir s = Filename.concat dir (relative s);;

let path_under ~parent file =
  let s = Misc.path_under ~parent file in
  relative s
;;

let string s = s ;;
let abs_string = string;;
let rel_string = string;;

let compare = Pervasives.compare;;