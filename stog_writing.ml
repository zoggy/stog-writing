(** *)

let info = "Writing";;
let verbose = Stog_plug.verbose ~info;;
let warning = Stog_plug.warning ~info;;
let error = Stog_plug.error ~info;;

let rc_file stog =
  Filename.concat (Stog_config.config_dir stog.Stog_types.stog_dir) "config-writing"
;;

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

(*c==v=[String.string_of_opt]=1.0====*)
let string_of_opt = function
  None -> ""
| Some s -> s
(*/c==v=[String.string_of_opt]=1.0====*)

(*c==v=[String.opt_of_string]=1.0====*)
let opt_of_string = function
  "" -> None
| s -> Some s
(*/c==v=[String.opt_of_string]=1.0====*)

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

let () = Stog_plug.register_rule "prepare-notes" fun_prepare_notes;;

(** Bibliography *)

let bib_entries = ref Smap.empty;;
let bib_entries_by_hid = ref Smap.empty;;

let add_bib_entry hid e =
  let (m, by_hid) =
    try
      ignore(Smap.find e.Bibtex.id !bib_entries);
      warning (Printf.sprintf "duplicate entry %S" e.Bibtex.id);
      raise Not_found
    with Not_found ->
      let m = Smap.add e.Bibtex.id (hid, e) !bib_entries in
      let by_hid =
          let s_hid = Stog_types.string_of_human_id hid in
          let l =
            try Smap.find s_hid !bib_entries_by_hid
            with Not_found -> []
          in
          Smap.add s_hid (e :: l) !bib_entries_by_hid
      in
      (m, by_hid)
  in
  bib_entries := m;
  bib_entries_by_hid := by_hid
;;

let add_bib_file page file =
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
    List.iter (add_bib_entry page) entries
  with
    Sys_error s
  | Failure s ->
      error s
;;

let init_bib stog =
  let f_elt elt_id elt =
    match elt.Stog_types.elt_type with
      "bibliography" ->
        let files =
          try
            let l =
              Stog_misc.split_string
              (List.assoc "bib-files" elt.Stog_types.elt_vars)
              [','; ';']
            in
            List.map Stog_misc.strip_string l
          with Not_found ->
              warning (Printf.sprintf "No bib-files specified in %S" elt.Stog_types.elt_src);
              []
        in
        List.iter (add_bib_file elt.Stog_types.elt_human_id) files
    | _ -> ()
  in
  Stog_tmap.iter f_elt stog.Stog_types.stog_elts;
  stog
;;

let () = Stog_plug.register_stage0_fun init_bib;;

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
  let href = Printf.sprintf "%s#%s"
    (Stog_html.elt_url stog elt)
    (mk_bib_entry_anchor e)
  in
  Xtmpl.T ("a", ["href", href], subs)
;;

let fun_cite env atts subs =
  match Xtmpl.get_arg atts "href" with
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
                match Xtmpl.get_arg atts "format" with
                  Some format -> [xml_of_format format]
                | None ->
                    let node = "<cite-format/>" in
                    let s = Xtmpl.apply env node in
                    let s = if s = node then "[<bib-field name=\"id\"/>]" else s in
                    [ Xtmpl.xml_of_string s ]
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

let get_sorted_entries ~reverse sort_fields s_hid =
  let entries =
    try Smap.find s_hid !bib_entries_by_hid
    with
      Not_found -> []
  in
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

let xml_of_bib_entry env entry =
  let stog = Stog_plug.stog () in
  let tmpl = Filename.concat stog.Stog_types.stog_tmpl_dir "bib_entry.tmpl" in
  let env = add_bib_entry_env env entry in
  Xtmpl.T ("div", ["class", "bib-entry" ; "id", mk_bib_entry_anchor entry ],
   [Xtmpl.xml_of_string (Xtmpl.apply_from_file env tmpl)])
;;

let get_in_env env s =
  let node = "<"^s^"/>" in
  let s = Xtmpl.apply env node in
  if s = node then "" else s
;;

let get_in_args_or_env env args s =
  match Xtmpl.get_arg args s with
    None -> get_in_env env s
  | Some s -> s
;;

let get_hid env =
  let s = get_in_env env Stog_tags.elt_hid in
  assert (s <> "");
  s
;;

let fun_bibliography env atts subs =
  let reverse = Xtmpl.opt_arg ~def: "false" atts "reverse" = "true" in
  let sorting_fields = Xtmpl.opt_arg ~def: "id" atts "sort" in
  let sorting_fields = Stog_misc.split_string sorting_fields [ ',' ; ';' ] in
  let hid = get_hid env in
  let entries = get_sorted_entries ~reverse sorting_fields hid in
  List.map (xml_of_bib_entry env) entries
;;


let () = Stog_plug.register_rule "bibliography" fun_bibliography;;
let () = Stog_plug.register_rule "cite" fun_cite;;

(** Adding references to paragraphs and
  handling blocks (like environments in latex).
*)

module Sset = Set.Make
  (struct type t = string let compare = Pervasives.compare end);;


let blocks = ref Smap.empty;;
let add_block s_hid id ~short ~long =
  let map =
    try Smap.find s_hid !blocks
    with Not_found -> Smap.empty
  in
  (
   try
     ignore (Smap.find id map);
     warning (Printf.sprintf "Multiple blocks with id %S for hid=%S" id s_hid);
   with Not_found -> ()
  );
  let map = Smap.add id (short, long) map in
  blocks := Smap.add s_hid map !blocks
;;
let block_title s_hid id =
  try Smap.find id (Smap.find s_hid !blocks)
  with Not_found ->
    warning (Printf.sprintf "Unknown block for id=%S and hid=%S" id s_hid);
    ("???", "???")
;;

let counters = ref Smap.empty;;
let bump_counter s_hid name =
  let map =
    try Smap.find s_hid !counters
    with Not_found -> Smap.empty
  in
  let cpt =
    try Smap.find name map + 1
    with Not_found -> 1
  in
  let map = Smap.add name cpt map in
  counters := Smap.add s_hid map !counters;
  cpt
;;

let get_counter s_hid name =
  try Smap.find name (Smap.find s_hid !counters)
  with Not_found -> 0
;;

let add_string b s =
  for i = 0 to String.length s - 1 do
    match s.[i] with
      'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> Buffer.add_char b s.[i]
    | _ -> ()
  done
;;


let tag_block = "block";;
let tag_counter = "counter";;

let fun_counter env atts subs =
  match Xtmpl.get_arg atts "counter-name" with
    None -> subs
  | Some name ->
      let hid = get_hid env in
      let cpt = get_counter hid name in
      [Xtmpl.D (string_of_int cpt)]
;;

let fun_block env atts subs =
  match Xtmpl.get_arg atts "href" with
    Some s when s <> "" ->
      begin
        match Xtmpl.get_arg atts Stog_tags.elt_hid with
          Some _ -> raise Xtmpl.No_change
        | None ->
            let hid = get_hid env in
            [ Xtmpl.T (tag_block, [Stog_tags.elt_hid, hid ; "href", s], subs)]
      end
  | _ ->
      let stog = Stog_plug.stog () in
      let hid = get_hid env in
      let id_opt = Xtmpl.get_arg atts "id" in
      let label_opt = Xtmpl.get_arg atts "label" in
      let class_opt = Xtmpl.get_arg atts "class" in
      let title_opt = Xtmpl.get_arg atts "title" in
      let cpt_opt =
        match Xtmpl.get_arg atts "counter-name" with
          None -> None
        | Some name -> Some (bump_counter hid name)
      in
      let (short, long) =
        match label_opt with
          None -> failwith "No label for block"
        | Some label ->
            let short =
              match cpt_opt with
                None -> label
              | Some cpt -> Printf.sprintf "%s %d" label cpt
            in
            let long =
              match title_opt, cpt_opt with
                None, None -> short
              | None, Some cpt -> Printf.sprintf "%s." short
              | Some t, _ -> Printf.sprintf "%s: %s" short t
            in
            (short, long)
      in
      (match id_opt with None -> () | Some id -> add_block hid id ~short ~long);
      let env = Xtmpl.env_add_att "id" (string_of_opt id_opt) env in
      let env = Xtmpl.env_add_att "class" (string_of_opt class_opt) env in
      let env = Xtmpl.env_add_att "title" long env in
      match subs with
        [] ->
          let tmpl_file =
            match class_opt with
              None -> "block.tmpl"
            | Some c -> Printf.sprintf "block-%s.tmpl" c
          in
          let tmpl = Filename.concat stog.Stog_types.stog_tmpl_dir tmpl_file in
          [Xtmpl.xml_of_string (Xtmpl.apply_from_file env tmpl)]
      | _ ->
          Xtmpl.apply_to_xmls env subs
;;

let fun_block_stage2 env atts subs =
  match get_in_args_or_env env atts "href" with
    "" -> subs
  | href ->
      let stog = Stog_plug.stog () in
      let hid = match Xtmpl.get_arg atts Stog_tags.elt_hid with
          None -> assert false
        | Some hid -> hid
      in
      let (short, _) = block_title hid href in
      let (_,elt) = Stog_types.elt_by_human_id stog (Stog_types.human_id_of_string hid) in
      let url = Printf.sprintf "%s#%s" (Stog_html.elt_url stog elt) href in
      [Xtmpl.T ("a", ["href", url], [Xtmpl.xml_of_string short])]
;;

let () = Stog_plug.register_rule tag_counter fun_counter;;
let () = Stog_plug.register_rule tag_block fun_block;;
let () = Stog_plug.register_stage1_fun
  (fun _ -> Stog_plug.register_rule tag_block fun_block_stage2);;


let rec text_of_xml b = function
  Xtmpl.D s -> add_string b s
| Xtmpl.E (_,subs)
| Xtmpl.T (_, _, subs) ->
    text_of_xmls b subs
and text_of_xmls b l = List.iter (text_of_xml b) l;;

let min_size = 12 ;;
let create_id auto_ids xmls =
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
    if Sset.mem id !auto_ids then
      if n < len then
        iter (Printf.sprintf "%s%c" id s.[n]) (n+1)
      else
        iter (id^"_") (n+1)
    else
      id
  in
  let id = iter init min_size in
  auto_ids := Sset.add id !auto_ids;
  id
;;

let fun_p tag auto_ids env atts subs =
  match Xtmpl.get_arg atts "id" with
    Some s ->
      (* id already present, return same node *)
      raise Xtmpl.No_change
  | None ->
      (* create a unique id *)
      let id = create_id auto_ids subs in
      let base_url = Xtmpl.apply env "<site-url/>" in
      let link =
        Xtmpl.T ("a", ["class", "paragraph-url" ; "href", "#"^id],
            [Xtmpl.T ("img", ["src", base_url^"/paragraph-url.png"], [])])
     in
     [Xtmpl.T (tag, ("id", id) :: atts, subs @ [link])]
;;

let rec gather_existing_ids =
  let rec f map = function
    Xtmpl.D _ -> map
  | Xtmpl.E (((_,tag),atts),subs) ->
      let g acc = function
        (("",s), v) -> (s, v) :: acc
      | _ -> acc
      in
      let atts = List.fold_left g [] atts in
      f map (Xtmpl.T (tag, atts, subs))
  | Xtmpl.T (tag, atts, subs) ->
      let map =
        match tag with
          "p" | "pre" ->
            begin
              match Xtmpl.get_arg atts "id" with
                None -> map
              | Some id ->
                  try
                    ignore(Sset.add id map);
                    failwith (Printf.sprintf "id %S defined twice in the same element." id)
                  with Not_found ->
                      Sset.add id map
            end
        | _ -> map
      in
      List.fold_left f map subs
  in
  List.fold_left f
;;

let stage2_p stog elt =
  let b =
    try Smap.find elt.Stog_types.elt_type !automatic_ids_by_type
    with Not_found -> !automatic_ids_default
  in
  if b then
    begin
      let auto_ids = ref Sset.empty in
      auto_ids := gather_existing_ids !auto_ids elt.Stog_types.elt_out ;
      let rules = Stog_html.build_rules stog in
      let rules = ("p", fun_p "p" auto_ids) :: ("pre", fun_p "pre" auto_ids) :: rules in
      let env = Xtmpl.env_of_list rules in
      let out = Xtmpl.apply_to_xmls env elt.Stog_types.elt_out in
      { elt with Stog_types.elt_out = out }
    end
  else
    elt
;;

let () = Stog_plug.register_stage2_fun stage2_p;;
