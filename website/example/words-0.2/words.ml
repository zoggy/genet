
(*c==v=[File.string_of_file]=1.0====*)
let string_of_file name =
  let chanin = open_in_bin name in
  let len = 1024 in
  let s = String.create len in
  let buf = Buffer.create len in
  let rec iter () =
    try
      let n = input chanin s 0 len in
      if n = 0 then
        ()
      else
        (
         Buffer.add_substring buf s 0 n;
         iter ()
        )
    with
      End_of_file -> ()
  in
  iter ();
  close_in chanin;
  Buffer.contents buf
(*/c==v=[File.string_of_file]=1.0====*)


(*c==v=[String.split_string]=1.1====*)
let split_string ?(keep_empty=false) s chars =
  let len = String.length s in
  let rec iter acc pos =
    if pos >= len then
      match acc with
        "" -> []
      | _ -> [acc]
    else
      if List.mem s.[pos] chars then
        match acc with
          "" ->
            if keep_empty then
              "" :: iter "" (pos + 1)
            else
              iter "" (pos + 1)
        | _ -> acc :: (iter "" (pos + 1))
      else
        iter (Printf.sprintf "%s%c" acc s.[pos]) (pos + 1)
  in
  iter "" 0
(*/c==v=[String.split_string]=1.1====*)

let usage = Printf.sprintf "Usage: %s infile outfile" Sys.argv.(0);;

if Array.length Sys.argv <> 3 then
  ( prerr_endline usage; exit 1 );;


module Sset = Set.Make
  (struct type t = string let compare = Pervasives.compare end)
;;

try
  let words = split_string (string_of_file Sys.argv.(1)) ['\n'] in
  let set = List.fold_right Sset.add words Sset.empty in
  let oc = open_out Sys.argv.(2) in
  Sset.iter (fun word -> output_string oc (word^"\n")) set;
  close_out oc;
with
  Failure msg
| Sys_error msg  -> prerr_endline msg; exit 1
;;
