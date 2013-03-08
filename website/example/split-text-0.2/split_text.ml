(** Same as split-text 0.1 but handles an option -p to split according
  to paragraphs (blank lines) instead of word spaces.
*)

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

let usage = Printf.sprintf "Usage: %s [-p] infile <outfile|outdir>" Sys.argv.(0);;

if Array.length Sys.argv <> 3 then
  ( prerr_endline usage; exit 1 );;

let separators = [' ' ; '\t' ; '\n' ; '.' ; ',' ; ';' ; '?' ; '!' ; ':'];;

let split_in_pars infile outdir =
  let text = string_of_file infile in
  let len = String.length text in
  let b = Buffer.create 256 in
  let make_file n s =
    let oc = open_out (Filename.concat outdir (string_of_int n)) in
    output_string oc s;
    close_out oc
  in
  let prev_nl = ref false in
  let file_cpt = ref 0 in
  for i = 0 to len - 1 do
    match text.[i] with
      '\n' ->
        if !prev_nl then
          begin
            let s = Buffer.contents b in
            Buffer.reset b;
            (
             match split_string s ['\n'] with
               [] -> ()
             | _ -> make_file !file_cpt  s; incr file_cpt
            );
            prev_nl := false
          end
        else
          begin
            prev_nl := true;
            Buffer.add_char b '\n'
          end
    | c -> Buffer.add_char b c
  done;
  let s = Buffer.contents b in
  match split_string s ['\n'] with
    [] -> ()
   | _ -> make_file !file_cpt  s
;;

let split_par = ref false;;
let options =
  [ "-p", Arg.Set split_par, " produce one file per paragraph in output dir" ]
;;

try
  let args = ref [] in
  Arg.parse options (fun s -> args := !args @ [s]) usage;
  match !args with
    [] | [_] | _::_::_::_ ->
      failwith usage
  | [infile ; outfile] ->
      match !split_par with
        false ->
          let words = split_string (string_of_file infile) separators in
          let oc = open_out Sys.argv.(2) in
          List.iter (fun word -> output_string oc (word^"\n")) words;
          close_out oc;
      | true ->
          split_in_pars infile outfile
with
  Failure msg
| Sys_error msg  -> prerr_endline msg; exit 1
;;

