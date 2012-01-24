(** *)

open Config;;

let open_storage config =
  let world = Rdf_init.new_world () in
  Rdf_init.open_world world;
  let options = Printf.sprintf
    "host='%s',database='%s',user='%s','password='%s'"
    config.db_host config.db_name config.db_user config.db_passwd
  in
  let storage =
    try Rdf_storage.new_storage world
      ~options ~factory: "mysql" ~name: "genet"
    with
    Rdf_storage.Storage_creation_failed _ ->
        prerr_endline "Storage may not exist, try to create it";
        let options = "new='yes'"^options in
        Rdf_storage.new_storage world
        ~options ~factory: "mysql" ~name: "genet"
  in
  (storage, world)
;;
