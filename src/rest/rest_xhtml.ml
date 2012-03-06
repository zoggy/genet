(** Replying to xhtml queries. *)

open Rest_types;;

let ctype ?(t="text/html; charset=\"utf-8\"") () = ("Content-Type", t);;

let page ?env ctx ~title contents =
  let s = Rest_xpage.page ctx.ctx_cfg ?env
    ~title [Xtmpl.xml_of_string contents]
  in
  Printf.sprintf "<!DOCTYPE html>\n%s\n" s
;;

let page_active v =
  let env = Xtmpl.env_add_att ("navbar-"^v) "active" Xtmpl.env_empty in
  page ~env
;;

let tool_page = page_active "tools";;
let filetype_page = page_active "filetypes";;

let get_tool ctx uri =
  let name = Grdf_tool.name ctx.ctx_rdf uri in
  let contents = name in
  ([ctype ()], tool_page ctx ~title: name contents)
;;

let get_filetype ctx uri =
  let name = Grdf_ftype.name ctx.ctx_rdf uri in
  let contents = name in
  ([ctype ()], filetype_page ctx ~title: name contents)
;;

let get ctx thing args =
  match thing with
  | Static_file (f, t) -> ([ctype ~t ()], Misc.string_of_file f)
  | Tool uri -> get_tool ctx uri
  | Filetype uri -> get_filetype ctx uri
  | _ -> ([ctype ()], page ctx ~title: "coucou" "Coucou")
;;
