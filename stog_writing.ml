(** *)

let info = "Writing";;
let verbose = Stog_plug.verbose ~info;;
let warning = Stog_plug.warning ~info;;
let error = Stog_plug.error ~info;;

(** Notes *)

let note_source_id n = Printf.sprintf "source_note_%d" n;;
let note_target_id n = Printf.sprintf "target_note_%d" n;;

let fun_prepare_notes env args subs =
  let count = ref 0 in
  let notes = ref [] in
  let rec iter = function
  | Xtmpl.D _ as x -> x
  | Xtmpl.T (tag, atts, subs) ->
    iter (Xtmpl.E ((("",tag), List.map (fun (a, v) -> (("",a), v)) atts), subs))
  | Xtmpl.E (tag, subs) ->
      match tag with
      | (("", "note"), atts) ->
          incr count ;
          notes := (!count, subs) :: !notes ;
          let target = note_target_id !count in
          let source = note_source_id !count in
          Xtmpl.T ("sup", ["id", source], [
            Xtmpl.T ("a", ["href", "#"^target],
             [  Xtmpl.D (string_of_int !count)])
           ])
      | _ ->
          Xtmpl.E (tag, List.map iter subs)
  in
  let subs = List.map iter subs in
  let xml_of_note (n, xml) =
    let source = note_source_id n in
    let target = note_target_id n in
    Xtmpl.T ("div", [ "class", "note" ; "id", target ],
     (Xtmpl.T ("sup", [], [Xtmpl.T ("a", ["href", "#"^source], [Xtmpl.D (string_of_int n)])]) ::
      Xtmpl.D " " ::
      xml
     ))
  in
  let xml =
    Xtmpl.T ("div", ["class","notes"], List.rev_map xml_of_note !notes)
  in
  let atts = [ "notes", Xtmpl.string_of_xml xml ] in
  [ Xtmpl.T (Xtmpl.tag_env, atts, subs) ]
;;

let () = Stog_plug.register_fun "prepare-notes" fun_prepare_notes;;

(** Bibliography *)

let bib_entries = ref Stog_types.Str_map.empty;;
let bib_page = ref "bibliography";;

let add_bib_entry e =
  let m =
    try
      ignore(Stog_types.Str_map.find e.Bibtex.id !bib_entries);
      warning (Printf.sprintf "duplicate entry %S" e.Bibtex.id);
      raise Not_found
    with Not_found ->
      Stog_types.Str_map.add e.Bibtex.id e !bib_entries
  in
  bib_entries := m
;;

let add_bib_file file =
  verbose (Printf.sprintf "loading bibtex file %S" file);
  try
    let inch = open_in file in
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
    in
    close_in inch;
    List.iter add_bib_entry entries
  with
    Sys_error s
  | Failure s ->
      error s
;;

let init () =
  let stog = Stog_plug.stog () in
  begin
    try
      bib_page := List.assoc "bib-page" stog.Stog_types.stog_vars;
    with Not_found ->
        let msg =
          Printf.sprintf "No bib-page field in stog variables ; using %s as page for the bibliography"
          !bib_page
        in
        warning msg;
  end;
  begin
    try ignore(Stog_types.page_by_human_id stog !bib_page)
    with Not_found ->
      let msg = Printf.sprintf "Bibliography page %S unknown." !bib_page in
      error msg
  end;
  let files =
    try
      let l =
        Stog_misc.split_string
          (List.assoc "bib-files" stog.Stog_types.stog_vars)
          [','; ';']
      in
      List.map Stog_misc.strip_string l
    with _ -> []
  in
  List.iter add_bib_file files
;;

let init =
  let init_done = ref false in
  fun () -> if !init_done then () else (init () ; init_done := true)
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

let fun_bib_field e env atts _ =
  match Xtmpl.get_arg atts "name" with
    None ->
      warning
        (Printf.sprintf "No \"name\" attribute for bib entry %S" (e.Bibtex.id));
      []
  | Some name ->
      [Xtmpl.xml_of_string (get_bib_entry_field e name)]
;;

let add_bib_entry_env env e =
  Xtmpl.env_add "bib-field" (fun_bib_field e) env
;;

let xml_of_format fmt =
  let re = Str.regexp "\\$(\\([a-zA-Z0-9:_]+\\))" in
  let f _ =
    let field = Str.matched_group 1 fmt in
    Printf.sprintf "<bib-field name=%S/>" field
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
let mk_bib_entry_link e subs =
  let href = Printf.sprintf "<site-url/>/%s.html#bibentry_%s" !bib_page
    (escape_bib_entry_id e.Bibtex.id)
  in
  Xtmpl.T ("a", ["href", href], subs)
;;

let fun_cite env atts subs =
  init();
  let format = Xtmpl.opt_arg ~def: "[$(id)]" atts "format" in
  match Xtmpl.get_arg atts "href" with
    None ->
      error "Missing href in <cite>";
      subs
  | Some href ->
      try
        let entry = Stog_types.Str_map.find href !bib_entries in
        let env = add_bib_entry_env env entry in
        let xml = xml_of_format format in
        let text = Xtmpl.apply_to_xmls env [xml] in
        [mk_bib_entry_link entry text]
      with
        Not_found ->
          error (Printf.sprintf "Unknown bib entry %S" href);
          subs
;;

let get_sorted_entries sort_fields =
  let entries = Stog_types.Str_map.fold
    (fun k v acc -> v :: acc) !bib_entries []
  in
  let comp e1 e2 =
    let l1 = List.map (get_bib_entry_field e1) sort_fields in
    let l2 = List.map (get_bib_entry_field e2) sort_fields in
    Pervasives.compare l1 l2
  in
  List.sort comp entries
;;

let xml_of_bib_entry env entry =
  let stog = Stog_plug.stog () in
  let tmpl = Filename.concat stog.Stog_types.stog_tmpl_dir "bib_entry.tmpl" in
  let env = add_bib_entry_env env entry in
  Xtmpl.T ("div", ["class", "bib-entry" ; "id", escape_bib_entry_id entry.Bibtex.id ],
   [Xtmpl.xml_of_string (Xtmpl.apply_from_file env tmpl)])
;;

let fun_bibliography env atts subs =
  init ();
  let sorting_fields = Xtmpl.opt_arg ~def: "id" atts "sort" in
  let sorting_fields = Stog_misc.split_string sorting_fields [ ',' ; ';' ] in
  let entries = get_sorted_entries sorting_fields in
  List.map (xml_of_bib_entry env) entries
;;


let () = Stog_plug.register_fun "bibliography" fun_bibliography;;
let () = Stog_plug.register_fun "cite" fun_cite;;



