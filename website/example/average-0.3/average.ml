(* Same as average-0.2, with addition -c and -n options.
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


let usage = Printf.sprintf "Usage: %s [-n|-t] <infile|indir> outfile" Sys.argv.(0);;

type mode = Average | Count | Average_of_list
let mode = ref Average;;

let options =
  [
    "-c", Arg.Unit (fun _ -> mode := Count),
    " count total input words length" ;

    "-n", Arg.Unit (fun _ -> mode := Average_of_list),
    " read one number by file in input dir, and compute average of these numbers" ;
  ]
;;

let files_in_dir dir =
  try
    let d = Unix.opendir dir in
    let rec iter acc =
      match
        try Some (Unix.readdir d)
        with _ -> None
      with
        None -> acc
      | Some entry ->
          let filename = Filename.concat dir entry in
          if filename <> Filename.current_dir_name
            && filename <> Filename.parent_dir_name
          then
            iter (filename :: acc)
          else
            iter acc
    in
    let files = iter [] in
    Unix.closedir d;
    files
  with
    Unix.Unix_error (e, s1, s2) ->
      let msg = Printf.sprintf "%s: %s %s" (Unix.error_message e) s1 s2 in
      failwith msg
;;

try
  let args = ref [] in
  Arg.parse options (fun s -> args := !args @ [s]) usage;
  match !args with
    [] | [_] | _ :: _ :: _ :: _ ->
      failwith usage
  | [infile ; outfile] ->
      let output =
        match !mode with
          Average ->
            let lines = split_string (string_of_file infile) ['\n'] in
            let (sum, max, min) = List.fold_left
              (fun (acc, acc_max, acc_min) line ->
                 let n = String.length line in
                 (acc + n, max acc_max n, min acc_min n)
              )
                (0, min_int, max_int) lines
            in
            let (sum, nb_lines) =
              if List.length lines > 2 then
                (sum - min - max, List.length lines - 2)
              else
                (sum, List.length lines)
            in
            let avg = (float sum) /. (float nb_lines) in
            string_of_float avg
        | Count ->
            let n = String.length (string_of_file infile) in
            string_of_int n
        | Average_of_list ->
            let files = files_in_dir infile in
            let f acc file =
              match split_string (string_of_file file) ['\n'] with
                [] -> acc
              | s :: _ ->
                  try (int_of_string s) + acc
                  with _ -> failwith ("Invalid int string: "^s)
            in
            let sum = List.fold_left f 0 files in
            match files with
              [] -> "0."
            | _ ->
                string_of_float ((float sum) /. (float (List.length files)))
      in
      let oc = open_out Sys.argv.(2) in
      output_string oc (output ^ "\n");
      close_out oc
with
| Failure msg
| Sys_error msg  -> prerr_endline msg; exit 1
;;
