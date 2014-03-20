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

{

(** Bibtex lexer *)

open Bib_parser;;

(*c==v=[String.strip_string]=1.0====*)
let strip_string s =
  let len = String.length s in
  let rec iter_first n =
    if n >= len then
      None
    else
      match s.[n] with
        ' ' | '\t' | '\n' | '\r' -> iter_first (n+1)
      | _ -> Some n
  in
  match iter_first 0 with
    None -> ""
  | Some first ->
      let rec iter_last n =
        if n <= first then
          None
        else
          match s.[n] with
            ' ' | '\t' | '\n' | '\r' -> iter_last (n-1)
          |	_ -> Some n
      in
      match iter_last (len-1) with
        None -> String.sub s first 1
      |	Some last -> String.sub s first ((last-first)+1)
(*/c==v=[String.strip_string]=1.0====*)

let line_number = ref 1;;
let string_buffer = Buffer.create 256 ;;
}

let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let digit = ['0'-'9']
let identchar = lowercase | uppercase | digit | '-' | '_' | ':'

let entry_kind = '@'(lowercase|uppercase)+

let ident = identchar+

rule main = parse
| '"' { Buffer.reset string_buffer; string lexbuf }
| '\n'
    {
      incr line_number;
      let module L = Lexing in
      let pos = lexbuf.L.lex_curr_p in
      lexbuf.L.lex_curr_p <-
        { pos with
          L.pos_lnum = pos.L.pos_lnum + 1 ;
          pos_bol = pos.L.pos_cnum ;
        };
      main lexbuf
    }
| ' ' | '\t' { main lexbuf }
| ',' { COMMA }
| '='(' '|'\t')*'{' { Buffer.reset string_buffer; braced lexbuf }
| '=' { EQUAL }
| '{' { LBRACE }
| '}' { RBRACE }

| entry_kind {
      let s = Lexing.lexeme lexbuf in
      let len = String.length s in
      Entry_kind (String.sub s 1 (len-1))
    }
| ident { Ident (Lexing.lexeme lexbuf) }

| eof { EOF }
| _ { failwith ("Unexpected token: "^(Lexing.lexeme lexbuf)) }

and string = parse
 "\\\""  { Buffer.add_char string_buffer '"'; string lexbuf }
| "\\\\" { Buffer.add_char string_buffer '\\'; string lexbuf }
| '"'  { String (strip_string (Buffer.contents string_buffer)) }
| _ { Buffer.add_string string_buffer (Lexing.lexeme lexbuf); string lexbuf }
| eof { failwith "String non terminated." }

and braced = parse
 "\\{"  { Buffer.add_char string_buffer '{'; braced lexbuf }
| "\\}"  { Buffer.add_char string_buffer '}'; braced lexbuf }
| "\\\\" { Buffer.add_char string_buffer '\\'; braced lexbuf }
| '}'  { Equal_braced_string (strip_string (Buffer.contents string_buffer)) }
| _ { Buffer.add_string string_buffer (Lexing.lexeme lexbuf); braced lexbuf }
| eof { failwith "Braced contents non terminated." }
