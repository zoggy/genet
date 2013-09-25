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

open Rdf_term;;
open Grdf_types;;

let dbg = Misc.create_log_fun
  ~prefix: "Chn_flat"
    "GENET_GRDF_PORT_DEBUG_LEVEL"
;;
type dir = In | Out ;;

let pred_of_dir = function
| In -> Grdfs.genet_consumes
| Out -> Grdfs.genet_produces
;;

let dir_of_string = function
  "in" -> In | "out" -> Out
| s -> failwith (Printf.sprintf "Invalid direction %S" s)
;;
let string_of_dir = function
| In -> "in"
| Out -> "out"

let port_name wld port = Grdfs.name wld (Iri port);;
let port_rank = Grdfs.port_rank ;;

let port_dir port =
  let s = Grdfs.port_dir_string port in
  dir_of_string s
;;

let rec map_port_type f = function
| Var s -> Var s
| T a -> T (f a)
| Set s -> Set (map_port_type f s)
| Tuple l -> Tuple (List.map (map_port_type f) l)
;;

exception Invalid_type of string
exception Invalid_type_id of string

let parse_port_ident str =
  let lexbuf = Lexing.from_string str in
  try Grdf_parser.port_ident Lexer.main lexbuf
  with
    Grdf_parser.Error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      let loc = { Loc.loc_start = pos ; Loc.loc_end = pos } in
      Loc.raise_problem loc (Printf.sprintf "Invalid port ident: %s" str)
;;

let parse_port_type str =
  let lexbuf = Lexing.from_string str in
  try Grdf_parser.port_type Lexer.main lexbuf
  with
    Grdf_parser.Error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      let loc = { Loc.loc_start = pos ; Loc.loc_end = pos } in
      Loc.raise_problem loc (Printf.sprintf "Invalid type: %s" str)
;;

let string_of_port_type =
  let rec iter f = function
    Var s -> Printf.sprintf "'%s" s
  | T s -> f s
  | Set t -> Printf.sprintf "%s set" (iter f t)
  | Tuple l -> Printf.sprintf "(%s)" (String.concat " * " (List.map (iter f) l))
  in
  iter
;;

let port_type wld port =
  match Grdfs.object_literal wld ~sub: (Iri port) ~pred: Grdfs.genet_hastype with
    Some s_type -> parse_port_type s_type
  | None -> failwith (Printf.sprintf "No type for port %s" (Rdf_iri.string port))
;;

let port_file_type_iri =
  let rec iter prefix = function
    Var _ -> None
  | T s -> Some (Grdfs.iri_filetype prefix s)
  | Set t -> iter prefix t
  | Tuple [] -> None
  | Tuple (h::_) -> iter prefix h
  in
  iter
;;
let sort_ports l =
  List.sort (fun p1 p2 ->
    Pervasives.compare (port_rank p1) (port_rank p2))
  l
;;
let ports wld intf dir =
  let pred = pred_of_dir dir in
  sort_ports (Grdfs.object_iris wld ~sub: (Iri intf) ~pred)
;;


let unset_port_type wld port =
  let sub = Iri port in
  let pred = Grdfs.genet_hastype in
  List.iter
  (fun obj -> Grdfs.rem_triple wld ~sub ~pred ~obj)
  (Grdfs.objects wld ~sub ~pred)
;;

let set_port_type wld port typ =
  let str = string_of_port_type (fun x -> x) typ in
  let pred = Grdfs.genet_hastype in
  unset_port_type wld port;
  Grdfs.add_triple wld
     ~sub: (Iri port)
     ~pred
     ~obj: (Rdf_term.term_of_literal_string str)
;;

let iri_intf_port_of_dir = function
  In -> Grdfs.iri_intf_in_port
| Out -> Grdfs.iri_intf_out_port
;;

let insert_port wld intf dir (n, typ, name) =
  let pred = pred_of_dir dir in
  let iri = (iri_intf_port_of_dir dir) intf n in
  let sub = Iri intf in
  let obj = Iri iri in
  Grdfs.add_triple wld ~sub ~pred ~obj;
  set_port_type wld iri typ;
  match name with None -> () | Some name -> Grdfs.add_name wld sub name
;;

let delete_ports wld iri dir =
  let dir_pred = pred_of_dir dir in
  let sub = Iri iri in
  let pred = Grdfs.genet_hastype in
  let ports = Grdfs.object_iris wld ~sub ~pred: dir_pred in
  List.iter
  (fun obj -> Grdfs.rem_triple wld ~sub ~pred: dir_pred ~obj: (Iri obj))
  ports;
  let f port =
    List.iter
    (fun obj -> Grdfs.rem_triple wld ~sub: (Iri port) ~pred ~obj)
    (Grdfs.objects wld ~sub: (Iri port) ~pred: pred)
  in
  List.iter f ports
;;
(*
let delete_ports wld dir iri =
  let pred = pred_of_dir dir in
  let query =
    let triples1 = [
        (`I iri, [ `I pred, [`V "seq"] ] );
        (`V "seq", [ `V "seq_index", [`V "iri"] ]) ;
      ]
    in
    let triples2 =
      [ (`V "iri", [ `I Grdfs.genet_listof, [ `V "iri2"]]) ]
    in
    { Rdf_sparql.delins_insert = None ;
      delins_delete = Some (Some (triples1 @ triples2), []) ;
      delins_where = (triples1, Some (`Optional (triples2, None))) ;
    }
  in
  let query = Rdf_sparql.Delete_insert query in
  ignore (Rdf_sparql.exec wld.wld_world wld.wld_model query)
;;
*)
let set_ports wld intf dir ports =
  delete_ports wld intf dir ;
  List.iter (insert_port wld intf dir) ports
;;

let delete_port wld iri =
  let dir = port_dir iri in
  let pred = pred_of_dir dir in
  match Grdfs.subject_iri wld ~pred ~obj: (Iri iri) with
    None -> ()
  | Some sub ->
      let ports = ports wld sub dir in
      let ports = List.filter (fun p -> not (Rdf_iri.equal p iri)) ports in
      let rec iter m acc = function
        [] -> List.rev acc
      | p :: q ->
          let ftype = port_type wld p in
          let name = port_name wld p in
          iter (m+1) ((m,ftype,Some name)::acc) q
      in
      let ports = iter 1 [] ports in
      set_ports wld sub dir ports
;;


let copy_ports wld ~src ~dst =
  dbg ~level:2 ~loc:"copy_ports"
    (fun () ->
      Printf.sprintf "src=%s dst=%s"
        (Rdf_iri.string src) (Rdf_iri.string dst));
  let map iri =
    (port_rank iri, port_type wld iri, Some (port_name wld iri))
  in
  let f_dir dir =
    let src_ports = ports wld src dir in
    set_ports wld dst dir (List.map map src_ports);
    let dst_ports = ports wld dst dir in
    List.fold_left2 (fun acc p1 p2 -> Rdf_iri.Irimap.add p1 p2 acc)
      Rdf_iri.Irimap.empty src_ports dst_ports
  in
  let map_in = f_dir In in
  let map_out = f_dir Out in
  (map_in, map_out)
;;

let add_port wld intf dir ?(pos=max_int) ?name filetype =
  let pos = max 1 pos in
  let prev_ports = ports wld intf dir in
  let prev_ports = List.map
    (fun iri ->
       (port_rank iri, port_type wld iri,
        Misc.opt_of_string (port_name wld iri))
    )
    prev_ports
  in
  let rec iter inserted m acc = function
    [] ->
      let acc =
        if not inserted then
          (m, filetype, name) :: acc
        else
          acc
      in
      List.rev acc
  | (_, hftype,hname) as h :: q ->
      if pos <= m && not inserted then
        iter true (m+1) ((m, filetype, name)::acc) (h :: q)
      else
        iter inserted (m+1) ((m,hftype,hname) :: acc) q
  in
  let new_ports = iter false 1 [] prev_ports in
  assert (List.length new_ports > List.length prev_ports);
  set_ports wld intf dir new_ports
;;

let string_of_iri_port_type wld t =
  let f = Grdf_ftype.extension wld in
  string_of_port_type f t
;;

let string_type_of_ports wld f ~sep = function
  [] -> "()"
| l ->
    String.concat sep
    (List.map (fun p -> f (port_type wld p)) l)
;;


  