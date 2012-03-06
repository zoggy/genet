(** *)

open Rest_types;;

exception Not_implemented of string;;
let not_implemented msg = raise (Not_implemented msg);;

let xhtml_handlers =
  { h_get = Rest_xhtml.get ;
    h_del = (fun _ _ -> not_implemented "xhtml delete");
    h_post = (fun _ _ -> not_implemented "xhtml post");
    h_put = (fun _ _ -> not_implemented "xhtml put");
  }

let json_handlers =
  { h_get = (fun _ _ _ -> not_implemented "json get") ;
    h_del = (fun _ _ -> not_implemented "json delete");
    h_post = (fun _ _ -> not_implemented "json post");
    h_put = (fun _ _ -> not_implemented "json put");
  }


let thing_of_uri wld uri =
  prerr_endline (Printf.sprintf "thing_of_uri %s" uri);
  let sub = Rdf_node.new_from_uri_string wld.Grdf_types.wld_world uri in
  match Grdfs.class_of wld sub with
    None -> Other uri
  | Some c when c = Grdfs.genet_tool -> Tool uri
  | Some c when c = Grdfs.genet_branch -> Branch uri
  | Some c when c = Grdfs.genet_version -> Version uri
  | Some c when c = Grdfs.genet_intf -> Intf uri
  | Some c when c = Grdfs.genet_filetype -> Filetype uri
  | Some c ->
    prerr_endline c;
    Other uri
;;

let handler_by_method h ctx = function
  Get (uri, args) -> h.h_get ctx (thing_of_uri ctx.ctx_rdf uri) args
| Delete uri -> h.h_del ctx (thing_of_uri ctx.ctx_rdf uri)
| Post (uri, json) -> h.h_post ctx (thing_of_uri ctx.ctx_rdf uri) json
| Put (uri, json) -> h.h_put ctx (thing_of_uri ctx.ctx_rdf uri) json

let query content_type =
  match content_type with
    Xhtml -> handler_by_method xhtml_handlers
  | Json -> handler_by_method json_handlers
;;
