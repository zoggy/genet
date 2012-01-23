(** *)

type option_spec = string * Arg.spec * string

let print_version pref () =
  Printf.printf "%s version %s\n" pref Version.version;
  exit 0
;;

let option_version pref =
  "--version", Arg.Unit (print_version pref),
  " Print version number and exit"
;;
