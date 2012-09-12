(** Utilities. *)

(*i==v=[Misc.safe_main]=1.0====*)
(** [safe_main f] calls [f ()] but handles [Sys_error] and [Failure]
   exceptions by exiting with error code 1.
@author Maxence Guesdon
@version 1.0
@cgname Misc.safe_main*)
val safe_main : (unit -> unit) -> unit
(*/i==v=[Misc.safe_main]=1.0====*)


(*i==v=[String.string_of_opt]=1.0====*)
(** [string_of_opt s_opt] returns the empty string if
   [s_opt = None] or [s] if [s_opt = Some s].
@version 1.0
@cgname String.string_of_opt*)
val string_of_opt : string option -> string
(*/i==v=[String.string_of_opt]=1.0====*)

(*i==v=[String.opt_of_string]=1.0====*)
(** [opt_of_string s] returns [None] if the string if empty
   (length is 0) or [Some s].
@version 1.0
@cgname String.opt_of_string*)
val opt_of_string : string -> string option
(*/i==v=[String.opt_of_string]=1.0====*)

val map_opt : ('a -> 'b) -> 'a option -> 'b option

(** Creating a log function.
  [create_log_fun env_var] get the log level (an integer) from the given
  environment variable, and returns a function to print messages.
  This function takes a level (default is 1) and a function returning
  the message do print. The function is called only if the log level is
  higher than or equal to the given level.
  The [loc] parameter of the returned function can be used to indicate
  an additional string to print before the log message.
  If the environment variable is empty or does not contain an integer,
  then the log level is set to 0.
  @param prefix can be used to indicate a string prefixing every message
  @param print can be given to the function build the log function, to
  indicate an alternative way to display the message; default is to call
  [prerr_endline].
  *)
val create_log_fun :
  ?prefix: string ->
  ?print:(string -> unit) -> string ->
  (?loc: string -> ?level:int -> (unit -> string) -> unit)

(** Same as [create_log_fun] but also return a function to change
       the log level.*)
val create_log_fun_with_set :
  ?prefix: string ->
  ?print:(string -> unit) -> string ->
  (?loc: string -> ?level:int -> (unit -> string) -> unit) *
  (int -> unit)

(** [mkdir dir] runs [mkdir -p dir] in a shell to create the given directory.
@raise Failure if the command fails.
@param verbose indicates whether to print a message on stdout. Default is [false].*)
val mkdir : ?verbose: bool -> string -> unit

(*i==v=[File.string_of_file]=1.0====*)
(** [string_of_file filename] returns the content of [filename]
   in the form of one string.
@author Maxence Guesdon
@version 1.0
@raise Sys_error if the file could not be opened.
@cgname File.string_of_file*)
val string_of_file : string -> string
(*/i==v=[File.string_of_file]=1.0====*)

(*i==v=[Misc.try_finalize]=1.0====*)
(** [try_finalize f x g y] applies [f] to [x] and return
   the result or raises an exception, but in all cases
   [g] is applied to [y] before returning or raising the exception.
@author Didier R�my
@version 1.0
@cgname Misc.try_finalize*)
val try_finalize : ('l -> 'm) -> 'l -> ('n -> unit) -> 'n -> 'm
(*/i==v=[Misc.try_finalize]=1.0====*)

val normalized_path : string -> string

(*i==v=[File.file_of_string]=1.1====*)
(** [file_of_string ~file str] creates a file named
   [filename] whose content is [str].
@author Fabrice Lefessant
@version 1.1
@raise Sys_error if the file could not be opened.
@cgname File.file_of_string*)
val file_of_string : file:string -> string -> unit
(*/i==v=[File.file_of_string]=1.1====*)


(*i==v=[String.strip_string]=1.0====*)
(** [strip_string s] removes all leading and trailing spaces from the given string.
@author Maxence Guesdon
@version 1.0
@cgname String.strip_string*)
val strip_string : string -> string
(*/i==v=[String.strip_string]=1.0====*)


(*i==v=[String.is_prefix]=1.0====*)
(** [is_prefix pattern s] returns true if string [s] begins with [pattern].
@author Maxence Guesdon
@version 1.0
@cgname String.is_prefix*)
val is_prefix : string -> string -> bool
(*/i==v=[String.is_prefix]=1.0====*)


(*i==v=[String.chop_n_char]=1.0====*)
(** [chop_n_char n s] returns the given string where characters after position n
   are replaced by ["..."].
@version 1.0
@cgname String.chop_n_char*)
val chop_n_char : int -> string -> string
(*/i==v=[String.chop_n_char]=1.0====*)


(*i==v=[String.split_string]=1.1====*)
(** Separate the given string according to the given list of characters.
@author Maxence Guesdon
@version 1.1
@param keep_empty is [false] by default. If set to [true],
   the empty strings between separators are kept.
@cgname String.split_string*)
val split_string : ?keep_empty:bool -> string -> char list -> string list
(*/i==v=[String.split_string]=1.1====*)

val is_capitalized : string -> bool

(** [exec_command com] executes the given command and returns stdout or
  raise [Failure] with a message containing stderr if the command fails. *)
val exec_command : string -> string

type git_status = Git_modified of string | Git_id of string | Git_unknown

(** Get the git last commit id of the given file, or raise [Failure]. *)
val get_git_id : string -> string

(** [git_status file] returns the status of the given file or directory.
     Not that a directory can never have status [Git_unknown].*)
val git_status : string -> git_status

val unique_id : unit -> string

val dir_md5sum : string -> string
val file_md5sum : string -> string
val file_mimetype : string -> string

val copy_file : src: string -> dst: string -> unit

(** [path_under ~parent file] returns the path to [file] from [parent].
     @raise Failure if [parent] is not a prefix of [file].*)
val path_under : parent: string -> string -> string

val split_filename : string -> string list
