(** *)

open Rdf_node;;
open Grdf_types;;
open Rdf_sparql;;

let dbg = Misc.create_log_fun
  ~prefix: "Grdf_version"
    "GENET_GRDF_VERSION_DEBUG_LEVEL"
;;

let versions wld =
  dbg ~level: 1 (fun () -> "Grdf_version.versions");
  Grdfs.subject_uris wld
    ~pred: Grdfs.rdf_type ~obj: (Uri Grdfs.genet_version)
;;

let name wld uri = Grdfs.name wld (Uri uri);;

let parent wld uri =
  dbg ~level: 1 (fun () -> "Grdf_version.parent uri="^(Rdf_uri.string uri));
  let obj = Uri uri in
  Grdfs.subject_uri wld ~pred: Grdfs.genet_hasversion ~obj
;;

let version_exists wld uri =
  dbg ~level: 1 (fun () -> "Grdf_version.version_exists uri="^(Rdf_uri.string uri));
  if Grdfs.is_a_version wld uri then
    Some (name wld uri)
  else
    None
;;

let do_add wld uri name =
  dbg ~level: 1 (fun () -> "Grdf_version.do_add uri="^(Rdf_uri.string uri)^" name="^name);
  let sub = Uri uri in
  let obj = Uri Grdfs.genet_version in
  Grdfs.add_type wld ~sub ~obj;
  Grdfs.add_name wld sub name
;;

let add wld ~tool ?(parent=tool) name =
  dbg ~level: 1 (fun () -> "Grdf_version.add parent="^(Rdf_uri.string parent)^" name="^name);
  let node_parent = Uri parent in

  let tool_is_tool = Grdfs.is_a_tool wld tool in
  let parent_is_tool = Grdfs.is_a_tool wld parent in
  let parent_is_branch = Grdfs.is_a_branch wld parent in

  if not tool_is_tool then
    Grdf_types.error (Grdf_types.Not_a_tool tool);

  if not (parent_is_tool || parent_is_branch) then
    Grdf_types.error (Grdf_types.Not_tool_or_branch parent);

  let root_tool = Grdf_branch.tool wld parent in
  if root_tool <> tool then
    Grdf_types.error (Grdf_types.Tool_of_branch_differs (parent, root_tool, tool));

  let uri = Grdfs.uri_version tool name in

  match version_exists wld uri with
    Some name -> Grdf_types.error (Grdf_types.Version_exists name)
  | None ->
      do_add wld uri name;
      Grdfs.add_triple wld
      ~sub: node_parent
      ~pred: (Uri Grdfs.genet_hasversion)
      ~obj:  (Uri uri);
      uri
;;

let versions_of wld ?(recur=false) uri =
  dbg ~level: 1 (fun () -> "Grdf_branch.versions_of uri="^(Rdf_uri.string uri));
  if recur then
    begin
      let add set uri = Uriset.add uri set in
      let rec iter set uri =
        let versions = Grdfs.object_uris wld
          ~sub: (Uri uri) ~pred: Grdfs.genet_hasversion
        in
        let set = List.fold_left add set versions in
        let subs = Grdf_branch.subs wld uri in
        List.fold_left iter set subs
      in
      let set = iter Uriset.empty uri in
      Uriset.elements set
    end
  else
    (
     Grdfs.object_uris wld ~sub: (Uri uri) ~pred: Grdfs.genet_hasversion
    )
;;

let tool_of_version uri = Grdfs.uri_tool_of_version uri;;
