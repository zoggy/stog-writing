(** *)

let info = "Writing";;
let verbose = Stog_plug.verbose ~info;;
let warning = Stog_plug.warning ~info;;
let error = Stog_plug.error ~info;;

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

let bib_entries = ref Stog_types.Str_map.empty;;

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

let fun_bibliography env _ _ =
  init ();
  [ Xtmpl.D "coucou" ]
;;

let fun_bib_field e env atts _ =
  match Xtmpl.get_arg atts "name" with
    None ->
      warning
        (Printf.sprintf "No \"name\" attribute for bib entry %S" (e.Bibtex.id));
      ""
  | Some name ->
      try List.assoc name e.Bibtex.fields
      with Not_found -> ""
;;

let () = Stog_plug.register_fun "bibliography" fun_bibliography;;



