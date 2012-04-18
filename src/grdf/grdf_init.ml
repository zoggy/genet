(** *)

open Config;;
open Grdf_types;;

let open_graph config =
  let options =
    [ "host", config.db_host ;
      "database", config.db_name ;
      "user", config.db_user ;
      "password", config.db_passwd ;
      "storage", config.db_engine ;
    ]
  in
  let prefix = config.rest_api in
  let graph = Rdf_graph.open_graph ~options prefix in
  { wld_graph = graph ;
    wld_prefix = prefix ;
  }
;;

let _ = Random.self_init();;
