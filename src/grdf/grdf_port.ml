(** *)

open Rdf_node;;
open Grdf_types;;

let dbg = Misc.create_log_fun
  ~prefix: "Chn_flat"
    "GENET_GRDF_PORT_DEBUG_LEVEL"
;;
type 'a port_type = Var of string | T of 'a | Set of 'a port_type ;;
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

let port_name wld port = Grdfs.name wld (Uri port);;
let port_rank = Grdfs.port_rank ;;

let port_dir port =
  let s = Grdfs.port_dir_string port in
  dir_of_string s
;;

let rec map_port_type f = function
| Var s -> Var s
| T a -> T (f a)
| Set s -> Set (map_port_type f s)
;;

exception Invalid_type of string
exception Invalid_type_id of string

let parse_port_type =
  let re = Str.regexp "'?[a-zA-Z][a-zA-Z_0-9]*$" in
  let get_leaf s =
    if Str.string_match re s 0 then
      if s.[0] = '\'' then
        Var (String.sub s 1 (String.length s - 1))
      else
        T s
    else
      raise (Invalid_type_id s)
  in
  fun str ->
    let l = Misc.split_string str [' ' ; '\n' ; '\t' ; '\r' ] in
    let rec iter = function
      [] -> raise (Invalid_type str)
    | [s] -> get_leaf s
    | "set" :: q -> Set (iter q)
    | _ :: _ -> raise (Invalid_type str)
    in
    iter (List.rev l)
;;

let string_of_port_type =
  let rec iter f = function
    Var s -> Printf.sprintf "'%s" s
  | T s -> f s
  | Set t -> Printf.sprintf "%s set" (iter f t)
  in
  iter
;;

let port_type wld port =
  match Grdfs.object_literal wld ~sub: (Uri port) ~pred: Grdfs.genet_hastype with
    Some s_type -> parse_port_type s_type
  | None -> failwith (Printf.sprintf "No type for port %s" (Rdf_uri.string port))
;;

let port_file_type_uri =
  let rec iter prefix = function
    Var _ -> None
  | T s -> Some (Grdfs.uri_filetype prefix s)
  | Set t -> iter prefix t
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
  sort_ports (Grdfs.object_uris wld ~sub: (Uri intf) ~pred)
;;


let unset_port_type wld port =
  let sub = Uri port in
  let pred = Uri Grdfs.genet_hastype in
  List.iter
  (fun obj -> Grdfs.rem_triple wld ~sub ~pred ~obj)
  (Grdfs.objects wld ~sub ~pred)
;;

let set_port_type wld port typ =
  let str = string_of_port_type (fun x -> x) typ in
  let pred = Grdfs.genet_hastype in
  unset_port_type wld port;
  Grdfs.add_triple wld
     ~sub: (Uri port)
     ~pred: (Uri pred)
     ~obj: (Rdf_node.node_of_literal_string str)
;;

let uri_intf_port_of_dir = function
  In -> Grdfs.uri_intf_in_port
| Out -> Grdfs.uri_intf_out_port
;;

let insert_port wld intf dir (n, typ, name) =
  let pred = pred_of_dir dir in
  let uri = (uri_intf_port_of_dir dir) intf n in
  let sub = Uri intf in
  let pred = Uri pred in
  let obj = Uri uri in
  Grdfs.add_triple wld ~sub ~pred ~obj;
  set_port_type wld uri typ;
  match name with None -> () | Some name -> Grdfs.add_name wld sub name
;;

let delete_ports wld uri dir =
  let dir_pred = pred_of_dir dir in
  let sub = Uri uri in
  let pred = Uri Grdfs.genet_hastype in
  let ports = Grdfs.object_uris wld ~sub ~pred: dir_pred in
  List.iter
  (fun obj -> Grdfs.rem_triple wld ~sub ~pred: (Uri dir_pred) ~obj: (Uri obj))
  ports;
  let f port =
    List.iter
    (fun obj -> Grdfs.rem_triple wld ~sub: (Uri port) ~pred ~obj)
    (Grdfs.objects wld ~sub: (Uri port) ~pred: pred)
  in
  List.iter f ports
;;
(*
let delete_ports wld dir uri =
  let pred = pred_of_dir dir in
  let query =
    let triples1 = [
        (`I uri, [ `I pred, [`V "seq"] ] );
        (`V "seq", [ `V "seq_index", [`V "uri"] ]) ;
      ]
    in
    let triples2 =
      [ (`V "uri", [ `I Grdfs.genet_listof, [ `V "uri2"]]) ]
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

let delete_port wld uri =
  let dir = port_dir uri in
  let pred = pred_of_dir dir in
  match Grdfs.subject_uri wld ~pred ~obj: (Uri uri) with
    None -> ()
  | Some sub ->
      let ports = ports wld sub dir in
      let ports = List.filter (fun p -> not (Rdf_uri.equal p uri)) ports in
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
        (Rdf_uri.string src) (Rdf_uri.string dst));
  let map uri =
    (port_rank uri, port_type wld uri, Some (port_name wld uri))
  in
  let f_dir dir =
    let src_ports = ports wld src dir in
    set_ports wld dst dir (List.map map src_ports);
    let dst_ports = ports wld dst dir in
    List.fold_left2 (fun acc p1 p2 -> Urimap.add p1 p2 acc)
      Urimap.empty src_ports dst_ports
  in
  let map_in = f_dir In in
  let map_out = f_dir Out in
  (map_in, map_out)
;;

let add_port wld intf dir ?(pos=max_int) ?name filetype =
  let pos = max 1 pos in
  let prev_ports = ports wld intf dir in
  let prev_ports = List.map
    (fun uri ->
       (port_rank uri, port_type wld uri,
        Misc.opt_of_string (port_name wld uri))
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

let string_of_uri_port_type wld t =
  let f = Grdf_ftype.extension wld in
  string_of_port_type f t
;;

let string_type_of_ports wld f ~sep = function
  [] -> "()"
| l ->
    String.concat sep
    (List.map (fun p -> f (port_type wld p)) l)
;;


  