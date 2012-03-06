(** Replying to xhtml queries. *)

open Rest_types;;

let ctype = ("Content-Type", "text/html; charset=\"utf-8\"");;

let get_tool ctx uri =
  let name = Grdf_tool.name ctx.ctx_rdf uri in
  ([ctype], "<!DOCTYPE html>\n<html><body><h1>"^name^"</h1></body></html>\n")

let get ctx thing args =
  match thing with
    Tool uri -> get_tool ctx uri
  | _ ->
      ([ctype], "<!DOCTYPE html>\n<html><body><h1>Coucou</h1></body></html>\n")
;;
