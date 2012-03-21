(** *)

type chain_modname = string list
type chain_basename = string
type chain_name = chain_modname * chain_basename

let chain_modname (modname,_) = modname
let chain_basename (_,name) = name
let mk_chain_name modname name = (modname, name)

let string_of_chain_modname = String.concat ".";;
let string_of_chain_basename s = s;;
let string_of_chain_name (modname, name) =
  Printf.sprintf "%s.%s" (String.concat "." modname) name
;;

let chain_modname_of_string s =
  List.map Misc.strip_string (Misc.split_string s ['.']);;
let chain_basename_of_string s = s;;

let chain_name_of_string s =
  match List.rev (Misc.split_string s ['.']) with
    [] | [_] -> failwith ("Invalid chain full name: "^s)
  | name :: rev_modname -> (List.rev rev_modname, name)
;;

let compare_chain_modname = Pervasives.compare;;
let compare_chain_basename = Pervasives.compare;;
let compare_chain_name = Pervasives.compare;;

let uri_chain_module prefix modname =
  let s = String.concat "/" modname in
  Grdfs.uri_chain_module prefix s
;;

let uri_chain prefix fullname =
  let modname = String.concat "/" (chain_modname fullname) in
  let name = chain_basename fullname in
  Grdfs.uri_chain ~prefix ~modname name
;;

let is_uri_chain_module prefix uri =
  match Grdfs.is_uri_chain_module prefix uri with
    None -> None
  | Some slashes_modname ->
      let modname = Misc.split_string slashes_modname ['/'] in
      Some (string_of_chain_modname modname)
;;

let is_uri_chain prefix uri =
  match Grdfs.is_uri_chain prefix uri with
    None -> None
  | Some (slashes_modname, name) ->
      let s = Misc.split_string slashes_modname ['/'] in
      Some (String.concat "." (s@[name]))
;;
