(** *)

(*c==v=[Misc.safe_main]=1.0====*)
let safe_main main =
  try main ()
  with
    Failure s
  | Sys_error s ->
      prerr_endline s;
      exit 1
(*/c==v=[Misc.safe_main]=1.0====*)


(*c==v=[String.string_of_opt]=1.0====*)
let string_of_opt = function
  None -> ""
| Some s -> s
(*/c==v=[String.string_of_opt]=1.0====*)

(*c==v=[String.opt_of_string]=1.0====*)
let opt_of_string = function
  "" -> None
| s -> Some s
(*/c==v=[String.opt_of_string]=1.0====*)

let map_opt f = function None -> None | Some x -> Some (f x);;

let create_log_fun_with_set ?prefix ?(print=prerr_endline) env_var =
  let log_level =
    ref
      (try int_of_string (Sys.getenv env_var)
      with _ -> 0)
  in
  let pref =
    match prefix with
      None -> ""
    | Some s -> Printf.sprintf "[%s]" s
  in
  (fun ?loc ?(level=1) f ->
    if !log_level >= level then
       begin
         let loc =
           match loc with
             None -> ""
           | Some s -> Printf.sprintf "[%s]" s
         in
         let sep = match pref, loc with "", "" -> "" | _ -> " " in
         let s = Printf.sprintf "%s%s%s%s"
           pref loc sep (f())
         in
         print s
       end
  ),
  ((:=) log_level)
;;

let create_log_fun ?prefix ?print env_var =
  fst (create_log_fun_with_set ?prefix ?print env_var);;

let mkdir ?(verbose=false) dir =
  if verbose then print_endline (Printf.sprintf "creating %s" dir);
  let dir = Filename.quote dir in
  let com = Printf.sprintf "mkdir -p %s" dir in
  match Sys.command com with
    0 -> ()
  | _ -> failwith (Printf.sprintf "Could not create directoy %s" dir)
;;

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

(*c==v=[Misc.try_finalize]=1.0====*)
let try_finalize f x finally y =
  let res =
    try f x
    with exn -> finally y; raise exn
  in
  finally y;
  res
(*/c==v=[Misc.try_finalize]=1.0====*)

module Norm_path =
  struct
    (* mostly from Didier Remy's course. *)

    let equal_node n1 n2 =
      n1.Unix.st_ino = n2.Unix.st_ino && n1.Unix.st_dev = n2.Unix.st_dev;;

    type info = { path : string; lstat : Unix.stats };;
    let info path = { path = path; lstat = Unix.lstat path };;

    let dir_find f path =
      let dir_handle = Unix.opendir path in
      let rec find () =
        let name = Unix.readdir dir_handle in
        if f name then name else find ()
      in
      try
        try_finalize find () Unix.closedir dir_handle
      with End_of_file -> raise Not_found;;

    let normalized_path file =
      let rec find_root node =
        let parent_node = info (Filename.concat node.path Filename.parent_dir_name) in
        if equal_node node.lstat parent_node.lstat then "/"
        else
          let found name =
            name <> Filename.current_dir_name && name <> Filename.parent_dir_name &&
            equal_node node.lstat (Unix.lstat (Filename.concat parent_node.path name)) in
          let name = dir_find found parent_node.path in
          Filename.concat (find_root parent_node) name
      in
      match (Unix.stat file).Unix.st_kind with
        Unix.S_DIR -> find_root (info file)
      | _ ->
          let root = find_root (info (Filename.dirname file)) in
          Filename.concat root (Filename.basename file)
  end
;;
let normalized_path = Norm_path.normalized_path;;

(*c==v=[File.file_of_string]=1.1====*)
let file_of_string ~file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc
(*/c==v=[File.file_of_string]=1.1====*)


(*c==v=[String.strip_string]=1.0====*)
let strip_string s =
  let len = String.length s in
  let rec iter_first n =
    if n >= len then
      None
    else
      match s.[n] with
        ' ' | '\t' | '\n' | '\r' -> iter_first (n+1)
      | _ -> Some n
  in
  match iter_first 0 with
    None -> ""
  | Some first ->
      let rec iter_last n =
        if n <= first then
          None
        else
          match s.[n] with
            ' ' | '\t' | '\n' | '\r' -> iter_last (n-1)
          |	_ -> Some n
      in
      match iter_last (len-1) with
        None -> String.sub s first 1
      |	Some last -> String.sub s first ((last-first)+1)
(*/c==v=[String.strip_string]=1.0====*)


(*c==v=[String.is_prefix]=1.0====*)
let is_prefix s1 s2 =
  let len1 = String.length s1 in
  let len2 = String.length s2 in
  (len1 <= len2) &&
    (String.sub s2 0 len1) = s1
(*/c==v=[String.is_prefix]=1.0====*)


(*c==v=[String.chop_n_char]=1.0====*)
let chop_n_char n s =
  let len = String.length s in
  if len <= n +1 or n < 0 then
    s
  else
    Printf.sprintf "%s..." (String.sub s 0 (n+1))
(*/c==v=[String.chop_n_char]=1.0====*)


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

let is_capitalized s =
  if String.length s > 0 then
    match s.[0] with 'A'..'Z' -> true | _ -> false
  else
    false
;;

let exec_command com =
  let pref = Filename.basename Sys.argv.(0) in
  let file_out = Filename.temp_file pref "out" in
  let file_err = Filename.temp_file pref "err" in
  let com = Printf.sprintf "( %s ) > %s 2> %s"
    com (Filename.quote file_out) (Filename.quote file_err)
  in
  match Sys.command com with
    0 ->
      let s = string_of_file file_out in
      Sys.remove file_out ; Sys.remove file_err ;
      s
  | n ->
      let err = try string_of_file file_err with _ -> "" in
      (try Sys.remove file_out with _ -> ());
      (try Sys.remove file_err with _ -> ());
      let msg = Printf.sprintf "Command %S failed with code %d: %s" com n err in
      failwith msg
;;

exception Git_modified_file of string

type git_status = Git_modified of string | Git_id of string | Git_unknown

let get_git_id file =
  let dir = Filename.dirname file in
  let basename = Filename.basename file in
  let com = Printf.sprintf "cd %s ; git log -n 1 %s | head -n 1 | cut -d' ' -f 2"
    (Filename.quote dir) (Filename.quote basename)
  in
  let out = exec_command com in
  match split_string out ['\n'] with
    id :: _ -> id
  | _ -> failwith "No git id"
;;

let git_status_file file =
  let dir = Filename.dirname file in
  let basename = Filename.basename file in
  let com = Printf.sprintf "cd %s ; git status --porcelain %s | cut  -b 1-2 "
    (Filename.quote dir) (Filename.quote basename)
  in
  match strip_string (exec_command com) with
  | "??" -> Git_unknown
  | "" -> Git_id (get_git_id file)
  | s -> Git_modified s
;;

let git_status_dir dir =
  let parent = Filename.dirname dir in
  let basename = Filename.basename dir in
  let com = Printf.sprintf
    "cd %s ; git status --porcelain --untracked-files=no %s | cut -b 1-2"
    (Filename.quote parent) (Filename.quote basename)
  in
  let out = exec_command com in
  let lines = split_string out ['\n'] in
  match lines with
    [] | [""] -> Git_id (get_git_id dir)
  | _ -> Git_modified out
;;

let git_status file =
  match
    try Some (Unix.lstat file).Unix.st_kind
    with _ -> None
  with
    None -> Git_unknown
  | Some Unix.S_DIR -> git_status_dir file
  | _ -> git_status_file file
;;

let unique_id () =
  Random.self_init();
  Printf.sprintf "%04x-%04x-%04x-%04x"
    (Random.int 0xffff) (Random.int 0xffff)
    (Random.int 0xffff) (Random.int 0xffff)
;;


let dir_md5sum dir =
  let temp_file = Filename.temp_file "dir_md5sum" ".txt" in
  let com = Printf.sprintf
    "cd %s && find . -type f  -exec md5sum {} + | awk '{print $2, $1}' | sort | cut -b 3-1000 | md5sum | cut -d' ' -f 1 > %s"
    (Filename.quote dir) (Filename.quote temp_file)
  in
  match Sys.command com with
    0 ->
      let s = strip_string (string_of_file temp_file) in
      Sys.remove temp_file;
      s
  | n ->
      (try Sys.remove temp_file with _ -> ());
      failwith (Printf.sprintf "Command failed [%d]: %s" n com)
;;

let file_md5sum file =
  let temp_file = Filename.temp_file "file_md5sum" ".txt" in
  let com = Printf.sprintf "md5sum %s | cut -d' ' -f 1 > %s"
    (Filename.quote file) (Filename.quote temp_file)
  in
  match Sys.command com with
    0 ->
      let s = strip_string (string_of_file temp_file) in
      Sys.remove temp_file;
      s
  | n ->
      (try Sys.remove temp_file with _ -> ());
      failwith (Printf.sprintf "Command failed [%d]: %s" n com)
;;

let path_under ~parent file =
  if is_prefix parent file then
    begin
      let len = String.length parent + 1 in
      String.sub file len (String.length file - len)
    end
  else
    failwith (Printf.sprintf "%s is not under %s" file parent)
;;

let file_mimetype file =
  let temp_file = Filename.temp_file "mimetype_md5sum" ".txt" in
  let com = Printf.sprintf "file -i %s | cut -d' ' -f 2-10 > %s"
    (Filename.quote file) (Filename.quote temp_file)
  in
  match Sys.command com with
    0 ->
      let s = strip_string (string_of_file temp_file) in
      Sys.remove temp_file;
      s
  | n ->
      (try Sys.remove temp_file with _ -> ());
      failwith (Printf.sprintf "Command failed [%d]: %s" n com)
;;

let copy_file ~src ~dst =
  let com = Printf.sprintf "cp -r %s %s"
    (Filename.quote src) (Filename.quote dst)
  in
  match Sys.command com with
    0 -> ()
  | n ->
      failwith (Printf.sprintf "Command failed [%d]: %s" n com)
;;

let split_filename s = split_string s [ Filename.dir_sep.[0] ];;

(*c==v=[List.list_diff]=1.0====*)
let list_diff ?(pred=(=)) l1 l2 =
  List.fold_right
    (fun el acc ->
       if not (List.exists (pred el) l2) then
         el :: acc
       else
         acc
    )
    l1 []
(*/c==v=[List.list_diff]=1.0====*)
