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
