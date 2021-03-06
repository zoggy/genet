(*********************************************************************************)
(*                Genet                                                          *)
(*                                                                               *)
(*    Copyright (C) 2012-2013 Institut National de Recherche en Informatique     *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License version 3             *)
(*    or later as published by the Free Software Foundation.                     *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public License          *)
(*    along with this program; if not, write to the Free Software Foundation,    *)
(*    Inc., 59 Temple Place, Suite 330, Boston, MA                               *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*                                                                               *)
(*********************************************************************************)

(** Main module of the REST web API. *)

open Rest_types;;

let content_type_of_string s =
  match s with
    "application/json" -> Rest_types.Json
  | _ -> Rest_types.Xhtml
;;

let get_method cgi =
  let path = cgi#environment#cgi_path_translated in
  match cgi#request_method with
    `GET | `HEAD ->
      let args = List.map (fun a -> (a#name, a#value)) cgi#arguments in
      Rest_types.Get (path, args)
  | `DELETE -> Rest_types.Delete path
  | `POST -> Rest_types.Post (path, "" (*`Null*))
  | `PUT arg -> Rest_types.Put (path, "" (*`Null*))
;;

let rest_api config host port (cgi : Netcgi.cgi_activation) =
  let env = cgi#environment in
  (*
     prerr_endline ("#cgi_server_name=" ^ env#cgi_server_name);
     prerr_endline ("#cgi_server_protocol=" ^ env#cgi_server_protocol);
  *)
  prerr_endline ("#cgi_path_info=" ^ env#cgi_path_info);
  prerr_endline ("#cgi_path_translated=" ^ env#cgi_path_translated);
  (*
     prerr_endline ("#cgi_query_string=" ^ env#cgi_query_string);
  *)
  let accept = env#input_header_field ~default: "" "Accept" in
  let content_type = content_type_of_string accept in
  let met = get_method cgi in
  let rdf_wld = Grdf_init.open_graph config in
  let context = {
      Rest_types.ctx_rdf = rdf_wld ;
      ctx_cfg = config ;
      ctx_user = None ;
      }
  in
  try
    let (header_fields, body) = Rest_query.query
      content_type context met
    in
    (* Set the header. The header specifies that the page must not be
       * cached. This is important for dynamic pages called by the GET
       * method, otherwise the browser might display an old version of
       * the page.*)
    cgi#set_header
    ~cache:`No_cache
    ~fields: (List.map (fun (f,v) -> (f, [v])) header_fields)
    ();
    cgi#out_channel#output_string body
  with
    Rest_query.Not_implemented msg ->
      failwith (Printf.sprintf "method not implemented: %s" msg)
;;

let process handler (cgi : Netcgi.cgi_activation) =
  (* The [try] block catches errors during the page generation. *)
  try

    handler cgi;

    (* After the page has been fully generated, we can send it to the
     * browser.
     *)
    cgi#out_channel#commit_work();
  with
    error ->
      (* An error has happened. Generate now an error page instead of
         * the current page. By rolling back the output buffer, any
         * uncomitted material is deleted.
         *)
        cgi#output#rollback_work();

      (* We change the header here only to demonstrate that this is
         * possible.
         *)
      cgi#set_header
      ~status:`Forbidden                  (* Indicate the error *)
      ~cache:`No_cache
      ~content_type:"text/plain; charset=\"utf-8\""
      ();

      cgi#output#output_string "While processing the request an O'Caml exception has been raised:\n";
      let msg =
        match error with
          Failure s | Sys_error s -> s
        | _ -> Printexc.to_string error
      in
      cgi#output#output_string (msg ^ "\n");

      (* Now commit the error page: *)
      cgi#output#commit_work()
;;


let options =
  Options.option_version "Genet-rest" ::
  Options.option_config ::
  Options.option_verbose ::
  []
;;

let config_tree host port =
  `Section ("netplex",
   [
     `Section ("service",
      [
        `Parameter ("name", `String "nethttpd") ;
        `Section ("protocol",
         [
           `Parameter ("name", `String "http") ;
           `Section ("address",
            [ `Parameter ("type", `String "internet") ;
              `Parameter ("bind", `String (Printf.sprintf "0.0.0.0:%d" port));
            ]) ;
         ]) ;
        `Section ("processor",
         [
           `Parameter ("type", `String "nethttpd") ;
           `Parameter ("access_log", `String "debug");  (* or "off" or "enabled" *)
           `Parameter ("suppress_broken_pipe", `Bool true);
           `Section ("host",
            [
              (* Think of Apache's "virtual hosts" *)
              `Parameter ("pref_name", `String host) ;
              `Parameter ("pref_port", `Int port);
              `Parameter ("names", `String "*:0"); (* Which requests are matched here: all *)
              `Section ("uri",
               [
                 `Parameter ("path", `String "/");
                 `Section ("service",
                  [ `Parameter ("type", `String "dynamic") ;
                    `Parameter ("handler", `String "api");
                  ]);
               ]);
            ]);
         ]);
        `Section ("workload_manager",
         [
           `Parameter ("type", `String "dynamic");
           `Parameter ("max_jobs_per_thread", `Int 1);  (* Everything else is senseless *)
           `Parameter ("min_free_jobs_capacity", `Int 1);
           `Parameter ("max_free_jobs_capacity", `Int 1);
           `Parameter ("max_threads", `Int 20);
         ]);
      ]);
   ]
  )
;;


let main () =
  let opts = Options.parse options in
  let config = Config.read_config opts.Options.config_file in
  let url = Rdf_uri.neturl (Rdf_iri.to_uri config.Config.rest_api) in
  let host = Neturl.url_host url in
  let port = Neturl.url_port url in
  let path = Neturl.join_path (Neturl.url_path url) in

  if opts.Options.verb_level > 0 then
    prerr_endline (Printf.sprintf "host=%s, port=%d, path=%s" host port path);

  let parallelizer =
    (*Netplex_mt.mt()*)     (* multi-threading *)
    Netplex_mp.mp()   (* multi-processing *)
  in

  let fun_handler _ =
    process (rest_api config host port)
  in

  let api =
    { Nethttpd_services.dyn_handler = fun_handler ;
      dyn_activation = Nethttpd_services.std_activation `Std_activation_buffered;
      dyn_uri = Some "/";                 (* not needed *)
      dyn_translator = (fun s -> s); (* not needed *)
      dyn_accept_all_conditionals = true ;
    }
  in
  let nethttpd_factory =
    Nethttpd_plex.nethttpd_factory
    ~handlers:[ "api", api ]
    ()
  in
  let config_tree = config_tree host port in
  let netplex_config = Netplex_main.create ~config_tree () in
  let netplex_config = Netplex_main.modify ~foreground: true netplex_config in
  Netplex_main.startup
    parallelizer
    Netplex_log.logger_factories   (* allow all built-in logging styles *)
    Netplex_workload.workload_manager_factories (* ... all ways of workload management *)
    [ nethttpd_factory ]           (* make this nethttpd available *)
    netplex_config
;;

let () = Misc.safe_main main;;

