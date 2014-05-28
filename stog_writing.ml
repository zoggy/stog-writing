(*********************************************************************************)
(*                Stog-writing                                                   *)
(*                                                                               *)
(*    Copyright (C) 2012-2014 Maxence Guesdon. All rights reserved.              *)
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

let module_name = "writing";;
let verbose = Stog_plug.verbose ~info: module_name;;
let warning = Stog_plug.warning ~info: module_name;;
let error = Stog_plug.error ~info: module_name;;

let rc_file stog = Stog_plug.plugin_config_file stog module_name;;

open Stog_types;;
module Smap = Stog_types.Str_map;;

type wdata =
  { auto_ids_default : bool ;
    auto_ids_by_type : bool Smap.t ;
    bib_entries : (Stog_path.path * Bibtex.entry) Smap.t ;
    bibs_by_path : Bibtex.entry list Smap.t Stog_path.Map.t;
    generated_by_doc : Stog_types.Str_set.t Stog_path.Map.t ;
  }

let empty_data = {
    auto_ids_default = true ;
    auto_ids_by_type = Smap.empty ;
    bib_entries = Smap.empty ;
    bibs_by_path = Stog_path.Map.empty;
    generated_by_doc = Stog_path.Map.empty ;
  }

let load_config env (stog, data) docs =
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

  let data =
    { data with
      auto_ids_default = o_ids_def#get ;
      auto_ids_by_type =
        List.fold_left (fun acc (t, b) -> Smap.add t b acc)
        data.auto_ids_by_type o_ids_typ#get;
    }
  in
  (stog, data)
;;


(** Notes *)

let note_source_id n = Printf.sprintf "source_note_%d" n;;
let note_target_id n = Printf.sprintf "target_note_%d" n;;

let fun_prepare_notes data env args subs =
  let count = ref 0 in
  let notes = ref [] in
  let rec iter = function
  | Xtmpl.D _ as x -> x
  | Xtmpl.E (tag, atts, subs) ->
      match tag with
      | ("", "note") ->
          incr count ;
          let note_id = Xtmpl.get_arg_cdata atts ("","id") in
          notes := (!count, note_id, subs) :: !notes ;
          let target =
            match note_id with
              None -> note_target_id !count
            | Some id -> id
          in
          let source = note_source_id !count in
          Xtmpl.E (("","sup"),
           Xtmpl.atts_of_list
             [ ("", "id"), [Xtmpl.D source] ;
               ("", "class"), [Xtmpl.D "footnote-link"] ;
             ],
           [
             Xtmpl.E (("", "a"), Xtmpl.atts_one ("", "href") [Xtmpl.D ("#"^target)],
              [  Xtmpl.D (string_of_int !count)])
           ])
      | _ ->
          Xtmpl.E (tag, atts, List.map iter subs)
  in
  let subs = List.map iter subs in
  let xml_of_note (n, note_id, xml) =
    let source = note_source_id n in
    let target = match note_id with None -> note_target_id n | Some id -> id in
    Xtmpl.E (("", "div"),
     Xtmpl.atts_of_list [ ("", "class"), [Xtmpl.D "note"] ; ("", "id"), [Xtmpl.D target] ],
     (Xtmpl.E (("", "sup"), Xtmpl.atts_empty,
       [Xtmpl.E (("", "a"),
          Xtmpl.atts_one ("", "href") [Xtmpl.D ("#"^source)],
          [Xtmpl.D (string_of_int n)])
       ]
      ) ::
      Xtmpl.D " " :: xml
     ))
  in
  let xml =
    Xtmpl.E (("", "div"),
     Xtmpl.atts_one ("", "class") [Xtmpl.D "notes"],
     List.rev_map xml_of_note !notes)
  in
  let atts = Xtmpl.atts_one ("", "notes") [ xml ] in
  (data, [ Xtmpl.E (("", Xtmpl.tag_env), atts, subs) ])
;;

let rules_notes = [ ("", "prepare-notes"), fun_prepare_notes ];;
let fun_level_notes = Stog_engine.fun_apply_stog_data_doc_rules (fun _ _ -> rules_notes);;

(** Bibliographies *)

let add_bib_entry data path e =
  try
    ignore(Smap.find e.Bibtex.id data.bib_entries);
    warning (Printf.sprintf "duplicate entry %S" e.Bibtex.id);
    raise Not_found
  with Not_found ->
      { data with
        bib_entries = Smap.add e.Bibtex.id (path, e) data.bib_entries ;
      }
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

let add_bibliography ?(name="default") ?(sort="id") ?(reverse=false) ?prefix doc (data, bib_map, rank) s_files =
  let sort_fields = Stog_misc.split_string sort [';' ; ',' ] in
  let sort_fields = List.map Stog_misc.strip_string sort_fields in
  let files =
    let l = Stog_misc.split_string s_files [','; ';'] in
    List.map Stog_misc.strip_string l
  in
  let files = List.map
    (fun file ->
       if Filename.is_relative file then
         Filename.concat (Filename.dirname doc.Stog_types.doc_src) file
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
  let data = List.fold_left
    (fun data e -> add_bib_entry data doc.Stog_types.doc_path e)
    data entries
  in
  try
    ignore(Smap.find name bib_map);
    let msg = Printf.sprintf "A bibliography %S is already defined in %S"
      name (Stog_path.to_string doc.Stog_types.doc_path)
    in
    failwith msg
  with Not_found ->
      (data, Smap.add name entries bib_map, rank)
;;

let init_bib env (stog,data) docs =
  let rec f_bib doc ?sort ?reverse ?prefix (data, bib_map, rank) = function
  | Xtmpl.E (("", "bibliography"), atts, subs) ->
      let name = Xtmpl.get_arg_cdata atts ("", "name") in
      let sort = match Xtmpl.get_arg_cdata atts ("", "sort") with None -> sort | x -> x in
      let prefix = match Xtmpl.get_arg_cdata atts ("", "prefix") with None -> prefix | x -> x in
      let reverse = match Xtmpl.get_arg_cdata atts ("", "reverse") with None -> reverse | x -> x in
      let reverse = Stog_misc.map_opt Stog_io.bool_of_string reverse in
      let files =
        match Xtmpl.get_arg_cdata atts ("", "files") with
          None -> failwith
            (Printf.sprintf "%s: No 'files' given for bibliography%s"
             (Stog_path.to_string doc.Stog_types.doc_path)
             (match name with None -> "" | Some s -> Printf.sprintf "%S" s)
            )
        | Some s -> s
      in
      add_bibliography ?name ?sort ?reverse ?prefix doc (data, bib_map, rank) files
  | Xtmpl.D _
  | Xtmpl.E _ -> (data, bib_map, rank)
   in
  let f_def doc (data, bib_map, rank) ((prefix, name), atts, xmls) =
    match prefix, name with
      "", "bib-files" ->
        add_bibliography doc (data, bib_map, rank) (Xtmpl.string_of_xmls xmls)
    | "", "bibliographies" ->
        let sort = Xtmpl.get_arg_cdata atts ("", "sort") in
        let prefix = Xtmpl.get_arg_cdata atts ("", "prefix") in
        let reverse = Xtmpl.get_arg_cdata atts ("", "reverse") in
        List.fold_left (f_bib doc ?sort ?reverse ?prefix) (data, bib_map, rank) xmls
    | _ -> (data, bib_map, rank)
  in
  let f_doc doc_id data =
    let doc = Stog_types.doc stog doc_id in
    let (data, bib_map, _) = List.fold_left (f_def doc)
      (data, Smap.empty, 0) doc.Stog_types.doc_defs
    in
    { data with
      bibs_by_path = Stog_path.Map.add
        doc.Stog_types.doc_path
        bib_map
        data.bibs_by_path ;
    }
  in
  let data = Stog_types.Doc_set.fold f_doc docs data in
  (stog, data)
;;

let fun_level_init = Stog_engine.Fun_stog_data init_bib;;

let fun_bib_field e data env atts _ =
  match Xtmpl.get_arg_cdata atts ("", "name") with
    None ->
      warning
        (Printf.sprintf "No \"name\" attribute for bib entry %S" (e.Bibtex.id));
      (data, [])
  | Some name ->
      (data, [Xtmpl.D (get_bib_entry_field e name)])
;;

let add_bib_entry_env env e =
  let env = List.fold_left
    (fun env (fd, v) ->
       Xtmpl.env_add_att
         (Printf.sprintf "bib-entry-%s" fd) [Xtmpl.D v] env
    )
      env
      e.Bibtex.fields
  in
  let env = Xtmpl.env_add "bib-field" (fun_bib_field e) env in
  let env = Xtmpl.env_add_att "bib-entry-id" [Xtmpl.D e.Bibtex.id] env in
  let env = Xtmpl.env_add_att "bib-entry-kind" [Xtmpl.D e.Bibtex.kind] env in
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

let mk_bib_entry_link stog path e subs =
  let href =
    (Stog_path.to_string path)^"#"^(mk_bib_entry_anchor e)
  in
  Xtmpl.E (("", "doc"),
   Xtmpl.atts_one ("", "href") [Xtmpl.D href],
   subs)
;;

let fun_cite (stog, data) env atts subs =
  match Xtmpl.get_arg_cdata atts ("", "href") with
    None ->
      error "Missing href in <cite>";
      ((stog, data), subs)
  | Some href ->
      try
        let refs = List.map Stog_misc.strip_string
          (Stog_misc.split_string href [','])
        in
        let get_def (stog, data) tag def =
          match Xtmpl.get_arg atts ("", tag) with
            Some xml -> ((stog, data), xml)
          | None ->
              let nodes = [Xtmpl.E (("", "cite-"^tag), Xtmpl.atts_empty, [])] in
              let ((stog, data), nodes2) = Xtmpl.apply_to_xmls (stog, data) env nodes in
              let res = if nodes = nodes2 then def else nodes2 in
              ((stog, data), res)
        in
        let ((stog, data), cite_begin) = get_def (stog,data) "begin" [] in
        let ((stog, data), cite_end) = get_def (stog,data) "end" [] in
        let ((stog, data), cite_sep) = get_def (stog,data) "sep" [Xtmpl.D ", "] in

        let f href ((stog, data), acc) =
          let (path, entry) = Smap.find href data.bib_entries in
          let env = add_bib_entry_env env entry in
          let ((stog, data), xml) =
            match subs with
              [] -> get_def (stog, data) "format"
                [Xtmpl.E (("", "bib-field"),
                   Xtmpl.atts_one ("","name") [Xtmpl.D "rank"],
                             []
                  )
                ]
            | _ -> ((stog, data), subs)
          in
          let ((stog, data), text) = Xtmpl.apply_to_xmls (stog, data) env xml in
          let link = mk_bib_entry_link stog path entry text in
          let acc =
            match acc with
              [] -> [link]
            | acc -> link :: cite_sep @ acc
          in
          ((stog, data), acc)
        in
        let ((stog, data), xmls) = List.fold_right f refs ((stog,data), []) in
        let xmls = cite_begin @ xmls @ cite_end in
        ((stog,data), xmls)
      with
        Not_found ->
          error (Printf.sprintf "Unknown bib entry %S" href);
          ((stog, data), subs)
;;

let xml_of_bib_entry env doc_id doc ((stog, data), acc) entry =
  let tmpl = Stog_tmpl.get_template_file stog doc "bib-entry.tmpl" in
  let env2 =
    let base_rules = Stog_html.build_base_rules stog doc_id in
    let env = Xtmpl.env_of_list base_rules in
    add_bib_entry_env env entry
  in
  let (stog, xmls) = Xtmpl.apply_to_file stog env2 tmpl in
  let ((stog, data), xmls) = Xtmpl.apply_to_xmls (stog,data) env xmls in
  ((stog, data),
   Xtmpl.E (("", "div"),
    Xtmpl.atts_of_list
      [ ("", "class"), [Xtmpl.D "bib-entry"] ;
        ("", "id"), [Xtmpl.D (mk_bib_entry_anchor entry)]
      ],
    xmls) :: acc
  )
;;

let get_in_env = Stog_html.get_in_env;;
let get_in_args_or_env = Stog_engine.get_in_args_or_env;;
let get_path = Stog_html.get_path;;

let fun_bibliography doc_id (stog, data) env atts subs =
  let ((stog, data), path) = get_path (stog, data) env in
  let name = Xtmpl.opt_arg_cdata ~def: "default" atts ("", "name") in
  let entries =
    try
      let bib_map = Stog_path.Map.find
        path data.bibs_by_path
      in
      try Smap.find name bib_map
      with Not_found ->
          failwith (Printf.sprintf "Unknown bibliography %S in %S"
           name (Stog_path.to_string path))
    with Not_found ->
        failwith (Printf.sprintf "No bibliographies for %S"
         (Stog_path.to_string path))
  in
  let entries =
    match Xtmpl.get_arg_cdata atts ("", "keywords") with
      None -> entries
    | Some s ->
        let kwds = Stog_misc.split_string s [',' ; ';'] in
        let pred entry =
          let e_kwds =
            try Stog_misc.split_string
              (List.assoc "keywords" entry.Bibtex.fields) [',' ; ';']
            with Not_found -> []
          in
          List.for_all (fun kwd -> List.mem kwd e_kwds) kwds
        in
        List.filter pred entries
  in
  let doc = Stog_types.doc stog doc_id in
  List.fold_left (xml_of_bib_entry env doc_id doc) ((stog, data), []) (List.rev entries)
;;

let rules_bib stog doc_id = [
    ("", "bibliography"), fun_bibliography doc_id ;
    ("", "cite"), fun_cite ;
  ];;

let fun_level_bib = Stog_engine.fun_apply_stog_data_doc_rules rules_bib ;;

(*let () = Stog_plug.register_level_fun 70 (Stog_plug.compute_doc rules_bib);;*)

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

let max_size = 24 ;;
let create_id xmls =
  let b = Buffer.create 256 in
  text_of_xmls b xmls;
  let s = Stog_misc.strip_string (Buffer.contents b) in
  let len = String.length s in
  String.sub s 0 (min len max_size)
;;

let fun_p tag doc (stog, data) env atts subs =
  let (id, atts) =
    match Xtmpl.get_arg_cdata atts ("", "id") with
      Some s -> (s, atts)
    | None ->
        (* create a hopefully unique id *)
        let id = create_id subs in
        (id, Xtmpl.atts_one ~atts ("", "id") [Xtmpl.D id])
  in
  let set =
    try Stog_path.Map.find
      doc.doc_path data.generated_by_doc
    with Not_found -> Stog_types.Str_set.empty
  in
  match Stog_types.Str_set.mem id set with
    true ->
      (* link code already generated, return same node *)
      raise Xtmpl.No_change
  | false ->
      let ((stog,data), xmls) = Xtmpl.apply_to_string (stog,data) env "<site-url/>" in
      let base_url =
        match xmls with
          [Xtmpl.D s] -> s
        | xml ->
            let s = Xtmpl.string_of_xmls xml in
            failwith (Printf.sprintf "<site-url/> does not reduce to PCData but to %S" s)
      in
      let link =
        Xtmpl.E (("", "a"),
         Xtmpl.atts_of_list
           [("", "class"), [Xtmpl.D "paragraph-url"] ;
             ("", "href"), [Xtmpl.D ("#"^id)]
           ],
         [Xtmpl.E (("", "img"),
            Xtmpl.atts_of_list
              [ ("", "src"), [Xtmpl.D (base_url^"/paragraph-url.png")] ;
                ("", "alt"), [Xtmpl.D "anchor"]
              ],
            [])
         ])
      in
      let set = Stog_types.Str_set.add id set in
      let generated_by_doc = Stog_path.Map.add
        doc.doc_path set data.generated_by_doc
      in
      let data = { data with generated_by_doc } in
      ((stog, data), [Xtmpl.E (("", tag), atts, link :: subs)])
;;

let rules_auto_ids stog doc_id =
  let doc = Stog_types.doc stog doc_id in
  let f tag (stog, data) env atts subs =
    let b =
      try Smap.find doc.Stog_types.doc_type data.auto_ids_by_type
      with Not_found -> data.auto_ids_default
    in
    if b then
      fun_p tag doc (stog, data) env atts subs
    else
      raise Xtmpl.No_change
  in
  [ (("", "p"), f "p") ;
    (("", "pre"), f "pre") ;
  ]
;;

let fun_level_auto_ids =
  Stog_engine.fun_apply_stog_data_doc_rules rules_auto_ids
;;

let level_funs =
  [
    "load-config", Stog_engine.Fun_stog_data load_config ;
    "init", fun_level_init ;
    "notes", fun_level_notes ;
    "bib", fun_level_bib ;
    "auto-ids", fun_level_auto_ids ;
  ]
;;

let default_levels =
  List.fold_left
    (fun map (name, levels) -> Stog_types.Str_map.add name levels map)
    Stog_types.Str_map.empty
    [
      "load-config", [ -2 ] ;
      "init", [ -1 ] ;
      "notes", [ 2 ] ;
      "bib", [ 3 ] ;
      "auto-ids", [ 4 ] ;
    ]

let make_engine ?levels () =
  let levels = Stog_html.mk_levels module_name level_funs default_levels ?levels () in
  let module M =
  struct
    type data = wdata
    let modul = {
        Stog_engine.mod_name = module_name ;
        mod_levels = levels ;
        mod_data = empty_data ;
       }

    type cache_data = {
        bibs : Bibtex.entry list Smap.t ;
      }

    let cache_load _stog data doc t =
      let path = doc.doc_path in
      let bibs_by_path = Stog_path.Map.add path t.bibs data.bibs_by_path in
      let data = { data with bibs_by_path } in
      Smap.fold
        (fun _ entries data -> List.fold_left
           (fun data e -> add_bib_entry data path e) data entries)
        t.bibs data

    let cache_store _stog data doc =
      let path = doc.doc_path in
      {
        bibs = (try Stog_path.Map.find path data.bibs_by_path with Not_found -> Smap.empty) ;
      }
  end
  in
  (module M : Stog_engine.Module)
;;

let f stog =
  let levels =
    try Some (Stog_types.Str_map.find module_name stog.Stog_types.stog_levels)
    with Not_found -> None
  in
  make_engine ?levels ()
;;

let () = Stog_engine.register_module module_name f;;
