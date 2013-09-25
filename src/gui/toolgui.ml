(** Gui elements for tools, interfaces, branches, versions. *)

open Chn_types;;

module Iriset = Rdf_iri.Iriset

type kind = [`Tool | `Branch | `Intf | `Version]

let string_of_kind = function
  `Tool -> "T"
| `Branch -> "B"
| `Intf -> "I"
| `Version -> "V"
;;

type elt = kind * Rdf_iri.iri

let tools ctx () =
  List.map (fun iri -> (`Tool, iri)) (Grdf_tool.tools ctx.ctx_rdf)
;;

let get_name ctx iri =
  match Grdfs.name ctx.ctx_rdf (Rdf_term.Iri iri) with
    "" -> Rdf_iri.string iri
  | s -> s
;;

let iriset_to_elt_list k set =
  Iriset.fold
    (fun iri acc -> (k, iri) :: acc) set []
;;

let children ctx = function
  `Tool, iri
| `Branch, iri ->
    let wld = ctx.ctx_rdf in
    let intfs = iriset_to_elt_list `Intf (Grdf_intf.intfs_of wld iri) in
    let branches = List.map (fun iri -> (`Branch, iri)) (Grdf_branch.subs wld iri) in
    let versions = List.map (fun iri -> (`Version, iri)) (Grdf_version.versions_of wld iri) in
    intfs @ branches @ versions
| `Intf, _ -> []
| `Version, _ -> []
;;


class tool_tree ctx wiri =
  let f_roots = tools ctx in
  let f_children = children ctx in
  let f_contents = function
    (k, iri) -> [`String (get_name ctx iri)]
  in
  let box = GPack.vbox () in
  object(self)
    inherit [elt] Gmytree.tree_edit ~f_roots ~f_children ~f_contents [`String ""]
    method coerce = box#coerce

    method on_select (_,iri) = wiri#set_text (Rdf_iri.string iri)
    method on_unselect _ = wiri#set_text ""


    initializer
      self#view#misc#reparent box#coerce
  end;;



class box ctx =
  let paned = GPack.paned `HORIZONTAL () in
  let vbox = GPack.vbox () in
  let wiri = GMisc.label ~xalign: 0. ~text: "" ~packing: (vbox#pack ~expand: false ~fill: true) () in
  let treebox = new tool_tree ctx wiri in
  object(self)
    method coerce = paned#coerce

    initializer
      paned#add1 treebox#coerce;
      paned#add2 vbox#coerce;
      paned#set_position 120;

  end;;