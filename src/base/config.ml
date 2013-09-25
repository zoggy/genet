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

module CF = Config_file

type t =
  { project_name : string ;
    project_id : string ;
    db_engine : string ;
    db_name : string ;
    db_user : string ;
    db_passwd : string ;
    db_host : string ;
    rest_api : Rdf_iri.iri ;
    root_dir : [`Absolute] Fname.filename ;
  }

let read_config file =
  let group = new CF.group in
  let pname_cp = new CF.string_cp ~group ["project_name"] "Foo" "" in
  let pid_cp = new CF.string_cp ~group ["project_id"] "foo" "" in
  let dbengine_cp = new CF.string_cp ~group ["db"; "engine"] "mysql" "postgresql or mysql" in
  let dbname_cp = new CF.string_cp ~group ["db"; "name"] "genet" "" in
  let dbuser_cp = new CF.string_cp ~group ["db"; "user"] "genet" "" in
  let dbhost_cp = new CF.string_cp ~group ["db"; "host"] "localhost" "" in
  let dbpasswd_cp = new CF.string_cp ~group ["db"; "password"] "" "" in
  let rest_api_cp = new CF.string_cp ~group ["rest_api"] "http://localhost:8082/" "do not forget ending /" in

  begin
    try group#read file
    with Stream.Error _ ->
      failwith (Printf.sprintf "Syntax error in config file %S" file)
  end;
  let rest_api =
    let s = rest_api_cp#get in
    let s = Misc.strip_string s in
    let len = String.length s in
    if len <= 0 || s.[len-1] <> '/' then s^"/" else s
  in
  { project_name = pname_cp#get ;
    project_id = pid_cp#get ;
    db_engine = dbengine_cp#get ;
    db_name = dbname_cp#get ;
    db_user = dbuser_cp#get ;
    db_host = dbhost_cp#get ;
    db_passwd = dbpasswd_cp#get ;
    rest_api = Rdf_iri.iri rest_api ;
    root_dir = Fname.absolute (Filename.dirname (Misc.normalized_path file)) ;
  }
;;

let string_of_config c =
  let b = Buffer.create 256 in
  Printf.bprintf b "project_name=%s\n" c.project_name ;
  Printf.bprintf b "project_id=%s\n" c.project_id ;
  Printf.bprintf b "db_engine=%s\n" c.db_engine ;
  Printf.bprintf b "db_name=%s\n" c.db_name ;
  Printf.bprintf b "db_user=%s\n" c.db_user ;
  Printf.bprintf b "db_host=%s\n" c.db_host ;
  Printf.bprintf b "db_passwd=%s\n" c.db_passwd ;
  Printf.bprintf b "rest_api=%s\n" (Rdf_iri.string c.rest_api);
  Buffer.contents b
;;

let in_dir cfg = Fname.concat_s cfg.root_dir "in";;
let out_dir cfg = Fname.concat_s cfg.root_dir "out";;
let chains_dir cfg = Fname.concat_s (in_dir cfg) "chains";;
let data_dir cfg =  Fname.concat_s (in_dir cfg) "data";;
let web_dir cfg = Fname.concat_s (in_dir cfg) "web";;
