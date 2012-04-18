(** *)

open Rdf_node;;
open Grdf_types;;
open Rdf_sparql;;


let dbg = Misc.create_log_fun
  ~prefix: "Grdf_tool"
    "GENET_GRDF_TOOL_DEBUG_LEVEL"
;;

let tools wld =
  Grdfs.subject_uris wld
    ~pred: Grdfs.rdf_type
    ~obj: (Uri Grdfs.genet_tool)
;;

let name wld uri = Grdfs.name wld (Uri uri)
;;

let tool_exists wld uri =
  dbg ~level: 1 (fun () -> "Grdf_tool.tool_exists uri="^(Rdf_uri.string uri));
  if Grdfs.is_a_tool wld uri then
    Some (name wld uri)
  else
    None
;;

let add_tool wld name =
  let uri = Grdfs.uri_tool ~prefix: wld.wld_prefix ~tool: name in
  match tool_exists wld uri with
    Some name2 -> Grdf_types.error (Grdf_types.Tool_exists name2)
  | None ->
      let sub = Uri uri in
      Grdfs.add_type wld ~sub ~obj: (Uri Grdfs.genet_tool);
      Grdfs.add_name wld sub name;
      uri
;;

let branches wld uri = Grdf_branch.subs wld uri;;
