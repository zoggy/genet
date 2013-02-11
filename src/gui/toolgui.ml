(** Gui elements for tools, interfaces, branches, versions. *)

open Chn_types;;

type kind = [`Tool | `Branch | `Intf | `Version]

let string_of_kind = function
  `Tool -> "T"
| `Branch -> "B"
| `Intf -> "I"
| `Version -> "V"
;;

type elt = kind * Rdf_uri.uri

let tools ctx () =
  List.map (fun uri -> (`Tool, uri)) (Grdf_tool.tools ctx.ctx_rdf)
;;

let get_name ctx uri =
  match Grdfs.name ctx.ctx_rdf (Rdf_node.Uri uri) with
    "" -> Rdf_uri.string uri
  | s -> s
;;

let uriset_to_elt_list k set =
  Uriset.fold
    (fun uri acc -> (k, uri) :: acc) set []
;;

let children ctx = function
  `Tool, uri
| `Branch, uri ->
    let wld = ctx.ctx_rdf in
    let intfs = uriset_to_elt_list `Intf (Grdf_intf.intfs_of wld uri) in
    let branches = List.map (fun uri -> (`Branch, uri)) (Grdf_branch.subs wld uri) in
    let versions = List.map (fun uri -> (`Version, uri)) (Grdf_version.versions_of wld uri) in
    intfs @ branches @ versions
| `Intf, _ -> []
| `Version, _ -> []
;;


class tool_tree ctx wuri =
  let f_roots = tools ctx in
  let f_children = children ctx in
  let f_contents = function
    (k, uri) -> [`String (get_name ctx uri)]
  in
  let box = GPack.vbox () in
  object(self)
    inherit [elt] Gmytree.tree_edit ~f_roots ~f_children ~f_contents [`String ""]
    method coerce = box#coerce

    method on_select (_,uri) = wuri#set_text (Rdf_uri.string uri)
    method on_unselect _ = wuri#set_text ""


    initializer
      self#view#misc#reparent box#coerce
  end;;



class box ctx =
  let paned = GPack.paned `HORIZONTAL () in
  let vbox = GPack.vbox () in
  let wuri = GMisc.label ~xalign: 0. ~text: "" ~packing: (vbox#pack ~expand: false ~fill: true) () in
  let treebox = new tool_tree ctx wuri in
  object(self)
    method coerce = paned#coerce

    initializer
      paned#add1 treebox#coerce;
      paned#add2 vbox#coerce;
      paned#set_position 120;

  end;;