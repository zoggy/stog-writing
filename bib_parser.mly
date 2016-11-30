/*********************************************************************************/
/*                Stog-writing                                                   */
/*                                                                               */
/*    Copyright (C) 2012-2014 Maxence Guesdon. All rights reserved.              */
/*                                                                               */
/*    This program is free software; you can redistribute it and/or modify       */
/*    it under the terms of the GNU General Public License as                    */
/*    published by the Free Software Foundation, version 3 of the License.       */
/*                                                                               */
/*    This program is distributed in the hope that it will be useful,            */
/*    but WITHOUT ANY WARRANTY; without even the implied warranty of             */
/*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               */
/*    GNU Library General Public License for more details.                       */
/*                                                                               */
/*    You should have received a copy of the GNU General Public                  */
/*    License along with this program; if not, write to the Free Software        */
/*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   */
/*    02111-1307  USA                                                            */
/*                                                                               */
/*    As a special exception, you have permission to link this program           */
/*    with the OCaml compiler and distribute executables, as long as you         */
/*    follow the requirements of the GNU GPL in regard to all of the             */
/*    software in the executable aside from the OCaml compiler.                  */
/*                                                                               */
/*    Contact: Maxence.Guesdon@inria.fr                                          */
/*                                                                               */
/*********************************************************************************/

(** Bibtex parser. *)

%{
open Bibtex

%}

%token EOF
%token <string> Entry_kind
%token LBRACE RBRACE EQUAL COMMA
%token <string> Ident
%token <string> String
%token <string> Equal_braced_string

%start <Bibtex.entry list> entries

%%
%public entries: list(entry) option(EOF) { $1 }

entry:
  k=Entry_kind LBRACE id=Ident COMMA l=list(field) RBRACE
  {
    { kind = String.lowercase_ascii k ; id = id ; fields = l }
  }
field:
  n=Ident EQUAL s=String option(COMMA) { (String.lowercase_ascii n, s) }
| n=Ident s=Equal_braced_string option(COMMA) { (String.lowercase_ascii n, s) }