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
    rest_api : Rdf_uri.uri ;
    root_dir : string ;
  }

let read_config file =
  let group = new CF.group in
  let pname_cp = new CF.string_cp ~group ["project_name"] "Foo" "" in
  let pid_cp = new CF.string_cp ~group ["project_id"] "foo" "" in
  let dbengine_cp = new CF.string_cp ~group ["db"; "engine"] "postgresql" "postgresql or mysql" in
  let dbname_cp = new CF.string_cp ~group ["db"; "name"] "genet" "" in
  let dbuser_cp = new CF.string_cp ~group ["db"; "user"] "genet" "" in
  let dbhost_cp = new CF.string_cp ~group ["db"; "host"] "localhost" "" in
  let dbpasswd_cp = new CF.string_cp ~group ["db"; "password"] "" "" in
  let rest_api_cp = new CF.string_cp ~group ["rest_api"] "http://localhost:8082/" "do not forget ending /" in

  group#read file;
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
    rest_api = Rdf_uri.uri rest_api ;
    root_dir = Filename.dirname (Misc.normalized_path file) ;
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
  Printf.bprintf b "rest_api=%s\n" (Rdf_uri.string c.rest_api);
  Buffer.contents b
;;

let in_dir cfg = Filename.concat cfg.root_dir "in";;
let out_dir cfg = Filename.concat cfg.root_dir "out";;
let chains_dir cfg = Filename.concat (in_dir cfg) "chains";;
let data_dir cfg =  Filename.concat (in_dir cfg) "data";;
let web_dir cfg = Filename.concat (in_dir cfg) "web";;
