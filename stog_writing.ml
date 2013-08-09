(*********************************************************************************)
(*                Stog-writing                                                   *)
(*                                                                               *)
(*    Copyright (C) 2012-2013 Maxence Guesdon. All rights reserved.              *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation, version 3 of the License.       *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public                  *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    As a special exception, you have permission to link this program           *)
(*    with the OCaml compiler and distribute executables, as long as you         *)
(*    follow the requirements of the GNU GPL in regard to all of the             *)
(*    software in the executable aside from the OCaml compiler.                  *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(** *)

let plugin_name = "writing";;
let verbose = Stog_plug.verbose ~info: plugin_name;;
let warning = Stog_plug.warning ~info: plugin_name;;
let error = Stog_plug.error ~info: plugin_name;;

let rc_file stog = Stog_plug.plugin_config_file stog plugin_name;;

module Smap = Stog_types.Str_map;;

let automatic_ids_default = ref true;;
let automatic_ids_by_type = ref Smap.empty ;;

let load_config stog =
  let module CF = Config_file in
  let group = new CF.group in
  let o_ids_def = new CF.bool_cp ~group ["automatic_ids"; "default"] true "" in
  let o_ids_typ = new CF.list_cp
    (CF.tuple2_wrappers CF.string_wrappers CF.bool_wrappers) ~group
    ["automatic_ids"; "by_type"] [] "pairs (type, bool) specifying whether to use automatic ids on element types"
  in
  let rc_file = rc_file stog in
  group#read rc_file;
  group#write rc_file;
  automatic_ids_default := o_ids_def#get ;
  automatic_ids_by_type :=
    List.fold_left (fun acc (t, b) -> Smap.add t b acc)
    !automatic_ids_by_type o_ids_typ#get
;;

let () = Stog_plug.register_stage0_fun (fun stog -> load_config stog; stog);;

(** Notes *)

let note_source_id n = Printf.sprintf "source_note_%d" n;;
let note_target_id n = Printf.sprintf "target_note_%d" n;;

let fun_prepare_notes env args subs =
  let count = ref 0 in
  let notes = ref [] in
  let rec iter = function
  | Xtmpl.D _ as x -> x
  | Xtmpl.E (tag, atts, subs) ->
      match tag with
      | ("", "note") ->
          incr count ;
          notes := (!count, subs) :: !notes ;
          let target = note_target_id !count in
          let source = note_source_id !count in
          Xtmpl.E (("","sup"), [("", "id"), source], [
            Xtmpl.E (("", "a"), [("", "href"), "#"^target],
             [  Xtmpl.D (string_of_int !count)])
           ])
      | _ ->
          Xtmpl.E (tag, atts, List.map iter subs)
  in
  let subs = List.map iter subs in
  let xml_of_note (n, xml) =
    let source = note_source_id n in
    let target = note_target_id n in
    Xtmpl.E (("", "div"), [ ("", "class"), "note" ; ("", "id"), target ],
     (Xtmpl.E (("", "sup"), [], [Xtmpl.E (("", "a"), [("", "href"), "#"^source], [Xtmpl.D (string_of_int n)])]) ::
      Xtmpl.D " " ::
      xml
     ))
  in
  let xml =
    Xtmpl.E (("", "div"), [("", "class"),"notes"], List.rev_map xml_of_note !notes)
  in
  let atts = [ ("", "notes"), Xtmpl.string_of_xml xml ] in
  [ Xtmpl.E (("", Xtmpl.tag_env), atts, subs) ]
;;

let rules_notes = [ ("", "prepare-notes"), fun_prepare_notes ];;

(** Bibliographies *)

let bib_entries = ref Smap.empty;;
let bibs_by_hid = ref Smap.empty;;

let add_bib_entry hid e =
  try
    ignore(Smap.find e.Bibtex.id !bib_entries);
    warning (Printf.sprintf "duplicate entry %S" e.Bibtex.id);
    raise Not_found
  with Not_found ->
      bib_entries := Smap.add e.Bibtex.id (hid, e) !bib_entries
;;

let bib_entries_of_file ?prefix file =
  verbose (Printf.sprintf "loading bibtex file %S" file);
  let inch =
    try open_in file
    with Sys_error s -> failwith s
  in
  try
    let lexbuf = Lexing.from_channel inch in
    let entries =
      try Bib_parser.entries Bib_lexer.main lexbuf
      with
      | Bib_parser.Error ->
          let pos = lexbuf.Lexing.lex_curr_p in
          let msg = Printf.sprintf "Parse error line %d, character %d"
            pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
          in
          failwith msg
      | e ->
          failwith (Printf.sprintf "line %d: %s" !Bib_lexer.line_number (Printexc.to_string e))
    in
    close_in inch;
    verbose "done";
    match prefix with
      None -> entries
    | Some prefix ->
        List.map (fun e -> { e with Bibtex.id = prefix ^ e.Bibtex.id }) entries
  with
    Sys_error s
  | Failure s ->
      close_in inch ;
      failwith s
;;


let get_bib_entry_field entry = function
  "id" -> entry.Bibtex.id
| "kind" -> entry.Bibtex.kind
| field ->
  try List.assoc field entry.Bibtex.fields
  with Not_found ->
        warning (Printf.sprintf "No field %S for bib entry %S" field entry.Bibtex.id);
        ""
;;

let sort_bib_entries sort_fields reverse entries =
  let comp e1 e2 =
    let l1 = List.map (get_bib_entry_field e1) sort_fields in
    let l2 = List.map (get_bib_entry_field e2) sort_fields in
    if reverse then
      Pervasives.compare l2 l1
    else
      Pervasives.compare l1 l2
  in
  List.sort comp entries
;;

let add_bibliography ?(name="default") ?(sort="id") ?(reverse=false) ?prefix elt (bib_map, rank) s_files =
  let sort_fields = Stog_misc.split_string sort [';' ; ',' ] in
  let sort_fields = List.map Stog_misc.strip_string sort_fields in
  let files =
    let l = Stog_misc.split_string s_files [','; ';'] in
    List.map Stog_misc.strip_string l
  in
  let files = List.map
    (fun file ->
       if Filename.is_relative file then
         Filename.concat (Filename.dirname elt.Stog_types.elt_src) file
       else
         file
    ) files
  in
  let entries = List.flatten (List.map (fun f -> bib_entries_of_file ?prefix f) files) in
  let entries = sort_bib_entries sort_fields reverse entries in
  let (entries, rank) = List.fold_left
    (fun (acc, rank) e ->
      let rank = rank + 1 in
      let e = { e with Bibtex.fields = ("rank", string_of_int rank) :: e.Bibtex.fields } in
      ((e :: acc), rank)
    ) ([], rank) entries
  in
  let entries = List.rev entries in
  List.iter (add_bib_entry elt.Stog_types.elt_human_id) entries;
  try
    ignore(Smap.find name bib_map);
    let msg = Printf.sprintf "A bibliography %S is already defined in %S"
      name (Stog_types.string_of_human_id elt.Stog_types.elt_human_id)
    in
    failwith msg
  with Not_found ->
      (Smap.add name entries bib_map, rank)
;;

let init_bib stog =
  let rec f_bib elt ?sort ?reverse ?prefix (bib_map, rank) = function
  | Xtmpl.E (("", "bibliography"), atts, subs) ->
      let name = Xtmpl.get_arg atts ("", "name") in
      let sort = match Xtmpl.get_arg atts ("", "sort") with None -> sort | x -> x in
      let prefix = match Xtmpl.get_arg atts ("", "prefix") with None -> prefix | x -> x in
      let reverse = match Xtmpl.get_arg atts ("", "reverse") with None -> reverse | x -> x in
      let reverse = Stog_misc.map_opt Stog_io.bool_of_string reverse in
      let files =
        match Xtmpl.get_arg atts ("", "files") with
          None -> failwith
            (Printf.sprintf "%s: No 'files' given for bibliography%s"
             (Stog_types.string_of_human_id elt.Stog_types.elt_human_id)
             (match name with None -> "" | Some s -> Printf.sprintf "%S" s)
            )
        | Some s -> s
      in
      add_bibliography ?name ?sort ?reverse ?prefix elt (bib_map, rank) files
  | Xtmpl.D _
  | Xtmpl.E _ -> (bib_map, rank)
   in
  let f_def elt (bib_map, rank) ((prefix, name), atts, xmls) =
    match prefix, name with
      "", "bib-files" ->
        add_bibliography elt (bib_map, rank) (Xtmpl.string_of_xmls xmls)
    | "", "bibliographies" ->
        let sort = Xtmpl.get_arg atts ("", "sort") in
        let prefix = Xtmpl.get_arg atts ("", "prefix") in
        let reverse = Xtmpl.get_arg atts ("", "reverse") in
        List.fold_left (f_bib elt ?sort ?reverse ?prefix) (bib_map, rank) xmls
    | _ -> (bib_map, rank)
  in
  let f_elt _elt_id elt =
    let (bib_map, _) = List.fold_left (f_def elt)
      (Smap.empty, 0) elt.Stog_types.elt_defs
    in
    bibs_by_hid := Smap.add
      (Stog_types.string_of_human_id elt.Stog_types.elt_human_id)
      bib_map
      !bibs_by_hid
  in
  Stog_tmap.iter f_elt stog.Stog_types.stog_elts;
  stog
;;

let () = Stog_plug.register_stage0_fun init_bib;;

let fun_bib_field e env atts _ =
  match Xtmpl.get_arg atts ("", "name") with
    None ->
      warning
        (Printf.sprintf "No \"name\" attribute for bib entry %S" (e.Bibtex.id));
      []
  | Some name ->
      [Xtmpl.D (get_bib_entry_field e name)]
;;

let add_bib_entry_env env e =
  let env = List.fold_left
     (fun env (fd, v) ->
       Xtmpl.env_add_att
       (Printf.sprintf "bib-entry-%s" fd) v env
    )
     env
     e.Bibtex.fields
  in
  let env = Xtmpl.env_add "bib-field" (fun_bib_field e) env in
  let env = Xtmpl.env_add_att "bib-entry-id" e.Bibtex.id env in
  let env = Xtmpl.env_add_att "bib-entry-kind" e.Bibtex.kind env in
  env
;;

let xml_of_format fmt =
  let re = Str.regexp "\\$(\\([a-zA-Z0-9:_]+\\))" in
  let f _ =
    let field = Str.matched_group 1 fmt in
    Printf.sprintf "<bib-entry-%s/>" field
  in
  let xml_s = Str.global_substitute re f fmt in
  Xtmpl.xml_of_string xml_s
;;

let escape_bib_entry_id s =
  let len = String.length s in
  let b = Buffer.create len in
  for i = 0 to len - 1 do
    match s.[i] with
      'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> Buffer.add_char b s.[i]
    | c -> Buffer.add_char b '-'
  done;
  Buffer.contents b
;;

let mk_bib_entry_anchor e =
  Printf.sprintf "bibentry_%s" (escape_bib_entry_id e.Bibtex.id)
;;

let mk_bib_entry_link hid e subs =
  let stog = Stog_plug.stog () in
  let (_, elt) = Stog_types.elt_by_human_id stog hid in
  let href =
    Neturl.modify_url
      ~fragment: (mk_bib_entry_anchor e)
      (Stog_html.elt_url stog elt)
  in
  Xtmpl.E (("", "a"), [("", "href"), Stog_types.string_of_url href], subs)
;;

let fun_cite env atts subs =
  match Xtmpl.get_arg atts ("", "href") with
    None ->
      error "Missing href in <cite>";
      subs
  | Some href ->
      try
        let (hid, entry) = Smap.find href !bib_entries in
        let env = add_bib_entry_env env entry in
        let xml =
          match subs with
            [] ->
              begin
                match Xtmpl.get_arg atts ("", "format") with
                  Some format -> [xml_of_format format]
                | None ->
                    let nodes = [Xtmpl.E (("", "cite-format"), [], [])] in
                    let nodes2 = Xtmpl.apply_to_xmls env nodes in
                    let res = if nodes = nodes2 then
                        [Xtmpl.E (("", "bib-field"), [("","name"), "rank"], [])]
                      else
                        nodes2
                    in
                    res
              end
          | _ -> subs
        in
        let text = Xtmpl.apply_to_xmls env xml in
        [mk_bib_entry_link hid entry text]
      with
        Not_found ->
          error (Printf.sprintf "Unknown bib entry %S" href);
          subs
;;

let xml_of_bib_entry env entry =
  let stog = Stog_plug.stog () in
  let tmpl = Filename.concat stog.Stog_types.stog_tmpl_dir "bib_entry.tmpl" in
  let env = add_bib_entry_env env entry in
  Xtmpl.E (("", "div"), [("", "class"), "bib-entry" ; ("", "id"), mk_bib_entry_anchor entry ],
   (Xtmpl.apply_to_file env tmpl))
;;

let get_in_env = Stog_html.get_in_env;;
let get_in_args_or_env = Stog_html.get_in_args_or_env;;
let get_hid = Stog_html.get_hid;;

let fun_bibliography env atts subs =
  let hid = get_hid env in
  let name = Xtmpl.opt_arg ~def: "default" atts ("", "name") in
  let entries =
    try
      let bib_map = Smap.find hid !bibs_by_hid in
      try Smap.find name bib_map
      with Not_found ->
          failwith (Printf.sprintf "Unknown bibliography %S in %S" name hid)
    with Not_found ->
        failwith (Printf.sprintf "No bibliographies for %S" hid)
  in
  List.map (xml_of_bib_entry env) entries
;;

let rules_bib = [
    ("", "bibliography"), fun_bibliography ;
    ("", "cite"), fun_cite ;
  ];;

let () = List.iter
  (fun (name,f) -> Stog_plug.register_rule name f) rules_bib
;;

(** Adding references to paragraphs and
  handling blocks (like environments in latex).
*)

let add_string b s =
  for i = 0 to String.length s - 1 do
    match s.[i] with
      'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> Buffer.add_char b s.[i]
    | _ -> ()
  done
;;

let rec text_of_xml b = function
  Xtmpl.D s -> add_string b s
| Xtmpl.E (_, _, subs) ->
    text_of_xmls b subs
and text_of_xmls b l = List.iter (text_of_xml b) l;;

let min_size = 12 ;;
let create_id ~hid title xmls =
  let b = Buffer.create 256 in
  text_of_xmls b xmls;
  let s = Stog_misc.strip_string (Buffer.contents b) in
  let len = String.length s in
  let init =
    Printf.sprintf "%s%s"
      (String.sub s 0 (min len min_size))
      (String.make (min_size - (min len min_size)) '_')
  in
  let rec iter id n =
    try
      Stog_plug.add_block ~on_dup: `Fail
        ~hid ~id ~short: title ~long: title ();
      id
    with
      Failure _ ->
        if n < len then
          iter (Printf.sprintf "%s%c" id s.[n]) (n+1)
        else
          iter (id^"_") (n+1)
  in
  iter init min_size
;;

let fun_p hid title tag env atts subs =
  match Xtmpl.get_arg atts ("", "id") with
    Some s ->
      (* id already present, return same node *)
      raise Xtmpl.No_change
  | None ->
      (* create a unique id *)
      let id = create_id ~hid: (Stog_types.string_of_human_id hid)
        title subs
      in
      let base_url =
        match Xtmpl.apply_to_string env "<site-url/>" with
          [Xtmpl.D s] -> s
        | xml ->
          let s = Xtmpl.string_of_xmls xml in
          failwith (Printf.sprintf "<site-url/> does not reduce to PCData but to %S" s)
      in
      let link =
        Xtmpl.E (("", "a"), [("", "class"), "paragraph-url" ; ("", "href"), "#"^id],
            [Xtmpl.E (("", "img"),
              [ ("", "src"), base_url^"/paragraph-url.png" ;
                ("", "alt"), "anchor" ;
              ], [])])
     in
     [Xtmpl.E (("", tag), (("", "id"), id) :: atts, link :: subs)]
;;

let rules_level2 stog elt_id elt = rules_notes;;

let rules_level4 stog elt_id elt =
  let b =
    try Smap.find elt.Stog_types.elt_type !automatic_ids_by_type
    with Not_found -> !automatic_ids_default
  in
  let rules = Stog_html.build_base_rules stog elt_id elt in
  if b then
    begin
      let fun_p = fun_p elt.Stog_types.elt_human_id
         (Xtmpl.xml_of_string elt.Stog_types.elt_title)
      in
      (("", "p"), fun_p "p") ::
      (("", "pre"), fun_p "pre") :: rules
    end
  else
    rules
;;

let () = Stog_plug.register_level_fun 2 (Stog_html.compute_elt rules_level2);;
let () = Stog_plug.register_level_fun 4 (Stog_html.compute_elt rules_level4);;



