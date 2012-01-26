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

let rdf_output_format = ref "turtle";;
let mk_rdf_output_format f =
  "--"^f, Arg.Unit (fun () -> rdf_output_format := f),
  " set rdf output format to "^f^" instead of "^ !rdf_output_format
;;

let option_rdfxml = mk_rdf_output_format "rdfxml";;
let option_ntriples = mk_rdf_output_format "ntriples";;

type option_values = {
  config_file : string ;
  args : string list ;
  rdf_output_format : string ;
  }

let remaining = ref [] ;;

let build_option_values () =
 { config_file = !config_file ;
    args = List.rev !remaining ;
    rdf_output_format = !rdf_output_format ;
  }
;;

let parse options =
  Arg.parse (Arg.align options) (fun s -> remaining := s :: !remaining)
    (Printf.sprintf "Usage: %s [options] [arguments]" Sys.argv.(0));
  build_option_values ()
;;

let parse_command com =
  remaining := Cmdline.parse com ;
  build_option_values ()
;;