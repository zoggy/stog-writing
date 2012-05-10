(*********************************************************************************)
(*                OCamldoc-generators                                            *)
(*                                                                               *)
(*    Copyright (C) 2012 Institut National de Recherche en Informatique          *)
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

{

(** Bibtex lexer *)

open Bib_parser;;

let string_buffer = Buffer.create 256 ;;
}

let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let digit = ['0'-'9']
let identchar = lowercase | uppercase | digit | '_'

let entry_kind = '@'(lowercase|uppercase)+

let ident = identchar+

rule main = parse
| '"' { Buffer.reset string_buffer; string lexbuf }
| '\n'
    {
      let module L = Lexing in
      let pos = lexbuf.L.lex_curr_p in
      lexbuf.L.lex_curr_p <-
        { pos with
          L.pos_lnum = pos.L.pos_lnum + 1 ;
          pos_bol = pos.L.pos_cnum ;
        };
      main lexbuf
    }
| ' ' { main lexbuf }
| ',' { COMMA }
| '='' '*'{' { Buffer.reset string_buffer; braced lexbuf }
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

and string = parse
 "\\\""  { Buffer.add_char string_buffer '"'; string lexbuf }
| "\\\\" { Buffer.add_char string_buffer '\\'; string lexbuf }
| '"'  { String (Buffer.contents string_buffer) }
| _ { Buffer.add_string string_buffer (Lexing.lexeme lexbuf); string lexbuf }
| eof { failwith "String non terminated." }

and braced = parse
 "\\{"  { Buffer.add_char string_buffer '{'; braced lexbuf }
| "\\}"  { Buffer.add_char string_buffer '}'; braced lexbuf }
| "\\\\" { Buffer.add_char string_buffer '\\'; braced lexbuf }
| '}'  { Equal_braced_string (Buffer.contents string_buffer) }
| _ { Buffer.add_string string_buffer (Lexing.lexeme lexbuf); braced lexbuf }
| eof { failwith "Braced contents non terminated." }
