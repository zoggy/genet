(** *)

open Config;;
open Grdf_types;;

let open_storage config =
  let world = Rdf_init.new_world () in
  Rdf_init.open_world world;
  let options = Printf.sprintf
    "host='%s',database='%s',user='%s','password='%s'"
    config.db_host config.db_name config.db_user config.db_passwd
  in
  let storage =
    try Rdf_storage.new_storage world
      ~options ~factory: config.db_engine ~name: "genet"
    with
      Rdf_storage.Storage_creation_failed _ ->
        prerr_endline "Storage may not exist, try to create it";
        let options = "new='yes'"^options in
        Rdf_storage.new_storage world
        ~options ~factory: config.db_engine ~name: "genet"
  in
  let model = Rdf_model.new_model world storage in
  let rasqal_world = Rdf_rasqal.new_world () in
  Rdf_rasqal.open_world rasqal_world;
  { wld_world = world ;
    wld_model = model ;
    wld_storage = storage ;
    wld_rasqal = rasqal_world ;
    wld_prefix = config.rest_api ;
  }
;;

let _ = Random.self_init();;
let init wld = ()
(*  ignore(Grdf_tool.add_tool wld (Printf.sprintf "Tool%d" (Random.int 1000)))*)
;;

let close wld = Rdf_init.free wld.wld_world;;