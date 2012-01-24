(** *)

module CF = Config_file

type t =
  { project_name : string ;
    project_id : string ;
    db_name : string ;
    db_user : string ;
    db_passwd : string ;
    db_host : string ;
    uri_prefix : string ;
  }

let read_config file =
  let group = new CF.group in
  let pname_cp = new CF.string_cp ~group ["project_name"] "Foo" "" in
  let pid_cp = new CF.string_cp ~group ["project_id"] "foo" "" in
  let dbname_cp = new CF.string_cp ~group ["db"; "name"] "genet" "" in
  let dbuser_cp = new CF.string_cp ~group ["db"; "user"] "genet" "" in
  let dbhost_cp = new CF.string_cp ~group ["db"; "host"] "localhost" "" in
  let dbpasswd_cp = new CF.string_cp ~group ["db"; "password"] "" "" in
  let uri_prefix_cp = new CF.string_cp ~group ["uri_prefix"] "http://foo.net" "" in
  group#read file;
  { project_name = pname_cp#get ;
    project_id = pid_cp#get ;
    db_name = dbname_cp#get ;
    db_user = dbuser_cp#get ;
    db_host = dbhost_cp#get ;
    db_passwd = dbpasswd_cp#get ;
    uri_prefix = uri_prefix_cp#get ;
  }
;;

let string_of_config c =
  let b = Buffer.create 256 in
  Printf.bprintf b "project_name=%s\n" c.project_name ;
  Printf.bprintf b "project_id=%s\n" c.project_id ;
  Printf.bprintf b "db_name=%s\n" c.db_name ;
  Printf.bprintf b "db_user=%s\n" c.db_user ;
  Printf.bprintf b "db_host=%s\n" c.db_host ;
  Printf.bprintf b "db_passwd=%s\n" c.db_passwd ;
  Printf.bprintf b "uri_prefix=%s\n" c.uri_prefix ;
  Buffer.contents b
;;
