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

let config_file = ref "config.txt";;
let option_config =
  "--config", Arg.Set_string config_file,
  "<file> use <file> as config file instead of "^ !config_file
;;



type option_values = {
  config_file : string ;
  args : string list ;
  }

let parse options =
  let remaining = ref [] in
  Arg.parse (Arg.align options) (fun s -> remaining := s :: !remaining)
    (Printf.sprintf "Usage: %s [options] [arguments]" Sys.argv.(0));
  { config_file = !config_file ;
    args = List.rev !remaining ;
  }
;;

