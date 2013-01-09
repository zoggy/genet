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

(** Executing a chain on input data. *)

open Chn_types;;

exception Error of string

let error msg = raise (Error msg);;

let last_flat_chain ctx chain_name =
  let flats = Chn_flat.flat_chains_of_chain ctx chain_name in
  let flats = List.map
    (fun uri ->
       (uri, Misc.map_opt Netdate.since_epoch (Grdfs.creation_date_uri ctx.ctx_rdf uri)))
    flats
  in
  match flats with
    [] -> None
  | h :: q ->
      let f (last_uri, last_date) (uri, date) =
        match last_date, date with
          None, _ -> (uri, date)
        | _, None -> (last_uri, last_date)
        | Some d1, Some d2 ->
            if d1 < d2 then
              (uri, date)
            else
              (last_uri, last_date)
      in
      let (uri, d) = List.fold_left f h q in
      match d with None -> None | Some _ -> Some uri
;;

let exec_chain_comb ctx reporter spec uri_fchain comb =
  try
    ignore(Chn_inst.instanciate ctx reporter uri_fchain spec comb)
  with
    Not_found as e -> raise e
  | exc ->
      let msg =
        match exc with
          Failure s | Sys_error s -> s
        | e -> Printexc.to_string e
      in
      error msg
;;

let exec_chain ctx reporter spec chain_name =
  match last_flat_chain ctx chain_name with
    None ->
      error
      (Printf.sprintf "No flat chain for chain %S"
       (Chn_types.string_of_chain_name chain_name))
  | Some fchain ->
      let combs = Chn_inst.version_combinations ctx fchain in
      match combs with
        [] ->
          error
          (Printf.sprintf "No version combination to execute %S"
           (Chn_types.string_of_chain_name chain_name))
      | _ ->
          let f comb =
            try exec_chain_comb ctx reporter spec fchain comb
            with Error msg ->
              reporter#error msg;
              reporter#incr_errors
          in
          List.iter f combs
;;

let exec_chain_str ctx reporter spec s_chain_name =
  let chain_name = Chn_types.chain_name_of_string s_chain_name in
  exec_chain ctx reporter spec chain_name
;;

let exec ctx reporter spec =
  let f s =
    try exec_chain_str ctx reporter spec s
    with Failure s
    | Error s ->
        reporter#error s;
        reporter#incr_errors
  in
  List.iter f spec.Ind_types.chains
;;
