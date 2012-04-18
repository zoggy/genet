open Rdf_node;;
open Grdf_types;;
open Rdf_sparql;;

let dbg = Misc.create_log_fun
  ~prefix: "Grdf_intf"
    "GENET_GRDF_INTF_DEBUG_LEVEL"
;;

let uriset_of_list = List.fold_left (fun set x -> Uriset.add x set) Uriset.empty ;;

let intfs wld =
  dbg ~level: 1 (fun () -> "Grdf_intf.intfs");
  let l = Grdfs.subject_uris wld ~pred: Grdfs.rdf_type ~obj: (Uri Grdfs.genet_intf) in
  uriset_of_list l
;;

let name wld uri = Grdfs.name wld (Uri uri);;

let command_path wld uri =
  let pred = Grdfs.genet_haspath in
  Grdfs.object_literal wld ~sub: (Uri uri) ~pred
;;

let intf_exists wld uri =
  dbg ~level: 1 (fun () -> "Grdf_intf.intf_exists uri="^(Rdf_uri.string uri));
  if Grdfs.is_a_intf wld uri then
    Some (name wld uri)
  else
    None
;;

let do_add wld uri name =
  dbg ~level: 1 (fun () -> "Grdf_intf.do_add uri="^(Rdf_uri.string uri)^" name="^name);
  let sub = Uri uri in
  Grdfs.add_type wld ~sub ~obj: (Uri Grdfs.genet_intf);
  Grdfs.add_name wld sub name
;;

let add wld ~parent name =
  dbg ~level: 1 (fun () -> "Grdf_intf.add parent="^(Rdf_uri.string parent)^" name="^name);
  let node_parent = Uri parent in
  let parent_is_tool = Grdfs.is_a_tool wld parent in
  let parent_is_branch = Grdfs.is_a_branch wld parent in
  if not (parent_is_tool || parent_is_branch) then
    Grdf_types.error (Grdf_types.Not_tool_or_branch parent);

  let tool = Grdf_branch.tool wld parent in
  if not (Grdfs.is_a_tool wld tool) then
    Grdf_types.error (Grdf_types.Not_a_tool tool);
  let uri = Grdfs.uri_intf ~tool ~intf: name in
  begin
    match intf_exists wld uri with
      Some _ -> ()
    | None -> do_add wld uri name
  end;
  Grdfs.add_triple wld
  ~sub: node_parent
  ~pred: (Uri Grdfs.genet_hasintf)
  ~obj:  (Uri uri);
  uri
;;

let explicit_intfs_of wld uri =
  uriset_of_list (Grdfs.object_uris wld ~sub: (Uri uri) ~pred: Grdfs.genet_hasintf)
;;

let explicit_no_intfs_of wld uri =
  uriset_of_list (Grdfs.object_uris wld ~sub: (Uri uri) ~pred: Grdfs.genet_nointf)
;;

let intfs_of wld ?(recur=false) uri =
  dbg ~level: 1 (fun () -> "Grdf_intf.intfs uri="^(Rdf_uri.string uri));
  if recur then
    begin
      let rec iter set uri =
        let uris = explicit_intfs_of wld uri in
        let set = Uriset.union set uris in
        match Grdf_branch.parent wld uri with
          None -> set
        | Some (uri, _) -> iter set uri
      in
      iter Uriset.empty uri
    end
  else
    explicit_intfs_of wld uri
;;

let intfs_of_tool wld uri =
  let branches = Grdf_branch.subs wld ~recur: true uri in
  List.fold_left
    (fun acc b -> Uriset.union acc (intfs_of wld b))
    (explicit_intfs_of wld uri) branches
;;

let compute_intfs_of wld uri =
  prerr_endline "Grdf_intf.compute_intfs_of: start";
  let rec inher = function
    None -> Uriset.empty
  | Some (uri, _) ->
      let set = inher (Grdf_branch.parent wld uri) in
      let explicit = explicit_intfs_of wld uri in
      let explicit_no = explicit_no_intfs_of wld uri in
      Uriset.union (Uriset.diff set explicit_no) explicit
  in
  prerr_endline ("Grdf_intf.compute_intfs_of: node ok, uri="^(Rdf_uri.string uri));
  if Grdfs.is_a_tool wld uri then
    (* show all interfaces *)
    (
     prerr_endline "Grdf_intf.compute_intfs_of: then";
     let ret = (intfs_of_tool wld uri, Uriset.empty) in
     prerr_endline "Grdf_intf.compute_intfs_of: ok";
     ret
    )
  else
    begin
      prerr_endline "Grdf_intf.compute_intfs_of: else";
      let explicit = explicit_intfs_of wld uri in
      prerr_endline "Grdf_intf.compute_intfs_of: explicit ok";
      let parent =
        if Grdfs.is_a_version wld uri then
          (
           prerr_endline "Grdf_intf.compute_intfs_of: is_a_version: true";
           match Grdf_version.parent wld uri with None -> None | Some uri -> Some (uri, false)
          )
        else
          (
           prerr_endline "Grdf_intf.compute_intfs_of: is_a_version: false";
           Grdf_branch.parent wld uri
          )
      in
      prerr_endline "Grdf_intf.compute_intfs_of: parent ok";
      let inherited = inher parent in
      prerr_endline "Grdf_intf.compute_intfs_of: inherited ok";
      (explicit, inherited)
    end
;;

let implementors wld uri =
  Grdfs.subject_uris wld ~pred: Grdfs.genet_hasintf ~obj: (Uri uri)
;;

let not_implementors wld uri =
  Grdfs.subject_uris wld ~pred: Grdfs.genet_nointf ~obj: (Uri uri)
;;

let tool_of_intf uri = Grdfs.uri_tool_of_intf uri

let string_of_intf wld ?(with_uri=true) uri =
  let ports_in = Grdf_port.ports wld uri Grdf_port.In in
  let ports_out = Grdf_port.ports wld uri Grdf_port.Out in
  Printf.sprintf "%s%s -> %s"
  (if with_uri then Printf.sprintf "%s : " (Rdf_uri.string uri) else "")
  (Grdf_port.string_type_of_ports wld ~sep: " -> " ports_in)
  (Grdf_port.string_type_of_ports wld ~sep: " * " ports_out)
;;




