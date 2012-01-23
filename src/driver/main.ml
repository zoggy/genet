(** Main module of genet tool. *)

let options =
    Options.option_version "Genet" ::
    Options.option_config ::
    []
;;

let main () =
  let _opts = Options.parse options in
  exit 0
;;

let () = Misc.safe_main main;;
