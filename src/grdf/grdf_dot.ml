(*********************************************************************************)
(*                Genet                                                          *)
(*                                                                               *)
(*    Copyright (C) 2012-2013 Institut National de Recherche en Informatique     *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License version 3             *)
(*    or later as published by the Free Software Foundation.                     *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public License          *)
(*    along with this program; if not, write to the Free Software Foundation,    *)
(*    Inc., 59 Temple Place, Suite 330, Boston, MA                               *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*                                                                               *)
(*********************************************************************************)

(** *)

let md5 s = Digest.to_hex (Digest.string s);;

let dot_id iri = Printf.sprintf "N%s" (md5 (Rdf_iri.string iri));;

let tool_bgcolor2 = "#d7d7d9";;
let tool_bgcolor = "#e7e7e9";;
let tool_fgcolor = "black";;

let version_bgcolor = "#a1d99b";;
let version_fgcolor = "black";;

let branch_bgcolor = "#ccebc5";;
let branch_fgcolor = "black";;

let intf_bgcolor = "#fdcdac";;
let intf_fgcolor = "black";;

let ftype_bgcolor = "#0868ac";;
let ftype_fgcolor = "#white";;


let gen_tool b wld iri =
  let name = Grdf_tool.name wld iri in
  Printf.bprintf b "%s [ label=\"%s\", style=\"filled\", fillcolor=\"%s\", fontcolor=\"%s\", shape=\"box\", href=\"%s\"];\n"
    (dot_id iri) (String.escaped name) tool_bgcolor tool_fgcolor (Rdf_iri.string iri)
;;

let gen_version b wld iri =
  let name = Grdf_version.name wld iri in
  let label = Misc.chop_n_char 8 name in
  Printf.bprintf b
  "%s [ label=\"%s\", style=\"filled\", fillcolor=\"%s\", fontcolor=\"%s\", shape=\"box3d\", tooltip=\"%s\" href=\"%s\"];\n"
  (dot_id iri) (String.escaped label) version_bgcolor version_fgcolor (String.escaped name) (Rdf_iri.string iri)
;;

let gen_branch b wld iri =
  let name = Grdf_version.name wld iri in
  Printf.bprintf b
  "%s [ label=\"%s\", style=\"filled\", fillcolor=\"%s\", fontcolor=\"%s\", shape=\"ellipse\", href=\"%s\"];\n"
  (dot_id iri) (String.escaped name) branch_bgcolor branch_fgcolor (Rdf_iri.string iri)
;;

let gen_intf b wld iri =
  let name = Grdf_intf.name wld iri in
  let intf =
    Printf.sprintf "%s : %s"
    name
    (Grdf_intf.string_of_intf wld ~with_iri: false iri)
  in
  Printf.bprintf b
  "%s [ label=\"%s\", style=\"filled\", fillcolor=\"%s\", fontcolor=\"%s\", shape=\"octagon\", tooltip=\"%s\", href=\"%s\"];\n"
  (dot_id iri) (String.escaped name) intf_bgcolor intf_fgcolor (String.escaped intf) (Rdf_iri.string iri)
;;

let gen_hasbranch b wld ?(label=true) ?pred iri =
  let l = Grdf_branch.subs wld iri in
  let l = match pred with None -> l | Some f -> List.filter f l in
  List.iter
  (fun iri2 -> Printf.bprintf b "%s -> %s [%s];\n"
     (dot_id iri) (dot_id iri2)
     (if label then "label=\"hasBranch\"" else "")
  )
     l
;;

let gen_hasversion b wld ?(label=true) ?pred iri =
  let l = Grdf_version.versions_of wld iri in
  let l = match pred with None -> l | Some f -> List.filter f l in
  List.iter
    (fun iri2 -> Printf.bprintf b "%s -> %s [%s];\n"
     (dot_id iri) (dot_id iri2)
     (if label then "label=\"hasVersion\"" else "")
  )
     l
;;

let gen_hasintf b wld ?(label=true) ?pred iri =
  let set = Grdf_intf.explicit_intfs_of wld iri in
  let set = match pred with None -> set | Some f -> Rdf_iri.Iriset.filter f set in

  let set_no = Grdf_intf.explicit_no_intfs_of wld iri in
  let set_no = match pred with None -> set_no | Some f -> Rdf_iri.Iriset.filter f set_no in

  let f edgelabel color iri2 =
    Printf.bprintf b "%s -> %s [color=%S%s];\n"
      (dot_id iri) (dot_id iri2)
      color
      (if label then Printf.sprintf ", label=%S" edgelabel else "")
  in
  Rdf_iri.Iriset.iter (f "hasintf" "black") set ;
  Rdf_iri.Iriset.iter (f "nointf" "red") set_no ;
;;

let dot ?(edge_labels=true) wld =
  let tools = Grdf_tool.tools wld in
  let versions = Grdf_version.versions wld in
  let branches = List.map (fun t -> t.Grdf_branch.bch_iri) (Grdf_branch.branches wld) in
  let intfs = Grdf_intf.intfs wld in
  let b = Buffer.create 256 in
  Buffer.add_string b "digraph g {colorscheme=\"brewer\";\nrankdir=TB;\n";
  List.iter (gen_tool b wld) tools;
  List.iter (gen_version b wld) versions;
  List.iter (gen_branch b wld) branches;
  Rdf_iri.Iriset.iter (gen_intf b wld) intfs;

  let label = edge_labels in
  List.iter (gen_hasbranch b ~label wld) (tools @ branches);
  List.iter (gen_hasversion b ~label wld) (tools @ branches);
  List.iter (gen_hasintf b ~label wld) (tools @ branches @ versions);

  Buffer.add_string b "}\n";
  Buffer.contents b
;;

let dot_of_tool wld tool =
  let b = Buffer.create 256 in
  Buffer.add_string b "digraph g {\nrankdir=TB;\nfontsize=10;\n";
  let versions = Grdf_version.versions_of wld ~recur: true tool in
  let branches = Grdf_branch.subs wld ~recur:true tool in
  let intfs = Grdf_intf.intfs_of_tool wld tool in
  gen_tool b wld tool;
  List.iter (gen_version b wld) versions;
  List.iter (gen_branch b wld) branches;
  Rdf_iri.Iriset.iter (gen_intf b wld) intfs;

  let all = tool :: branches in
  let pred = fun iri -> List.mem iri all in
  List.iter (gen_hasbranch b wld ~label: false ~pred) all;

  let all = all @ versions in
  let pred = fun iri -> List.mem iri all in
  List.iter (gen_hasversion b wld ~label: false ~pred) all;

  let all = all @ (Rdf_iri.Iriset.elements intfs) in
  let pred = fun iri -> List.mem iri all in
  List.iter (gen_hasintf b wld ~label: false ~pred) all;

  Buffer.add_string b "}\n";
  Buffer.contents b
;;

let dot_of_branch wld iri =
  let b = Buffer.create 256 in
  Buffer.add_string b "digraph g {\nrankdir=TB;\nfontsize=10;\n";
  let versions = Grdf_version.versions_of wld ~recur: true iri in
  let branches = Grdf_branch.subs wld ~recur:true iri in
  let intfs = Grdf_intf.intfs_of_tool wld iri in
  prerr_endline (Printf.sprintf "dot_of_branch: branch has %d interfaces" (Rdf_iri.Iriset.cardinal intfs));
  List.iter (gen_version b wld) versions;
  List.iter (gen_branch b wld) branches;
  Rdf_iri.Iriset.iter (gen_intf b wld) intfs;

  let pred = fun iri -> List.mem iri branches in
  List.iter (gen_hasbranch b wld ~label: false ~pred) branches;

  let all = branches @ versions in
  let pred = fun iri -> List.mem iri all in
  List.iter (gen_hasversion b wld ~label: false ~pred) all;

  let all = all @ (Rdf_iri.Iriset.elements intfs) in
  let pred = fun iri -> List.mem iri all in
  List.iter (gen_hasintf b wld ~label: false ~pred) all;

  Buffer.add_string b "}\n";
  Buffer.contents b
;;

let dot_to_svg ?(program="dot") ?(options="") ?size dot =
  let temp_file = Filename.temp_file "genet" "svg" in
  let com = Printf.sprintf "echo %s |%s %s -Tsvg | tail --lines=+%d > %s"
    (Filename.quote dot) (Filename.quote program) options
    (match size with None -> 7 | Some _ -> 9)
    (Filename.quote temp_file)
  in
  match Sys.command com with
    0 ->
      let svg = Misc.string_of_file temp_file in
      (* remove bad xlink:title="&lt;TABLE&gt;" in svg code *)
      let svg = Misc.replace_in_string ~pat: "xlink:title=\"&lt;TABLE&gt;\"" ~subs: "" ~s: svg in
      Sys.remove temp_file;
      let svg =
        match size with
          None -> svg
        | Some (w,h) ->
          Printf.sprintf "<svg width=\"%d\" height=\"%d\" viewBox=\"0.0 0.0 %d.00 %d.00\"\n
          xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n%s"
            w h w h svg
      in
      [Xtmpl.xml_of_string svg]
  | n ->
      let msg = Printf.sprintf "Execution failed (%d): %s" n com in
      failwith msg
;;



  