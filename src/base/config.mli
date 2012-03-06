(** Access to genet main configuration file. *)

type t = {
  project_name : string;
  project_id : string;
  db_engine : string;
  db_name : string;
  db_user : string;
  db_passwd : string;
  db_host : string;
  uri_prefix : string;
  rest_api : string;
  root_dir : string;
}
val read_config : string -> t
val string_of_config : t -> string
