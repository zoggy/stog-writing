/*********************************************************************************/
/*                OCamldoc-generators                                            */
/*                                                                               */
/*    Copyright (C) 2012 Institut National de Recherche en Informatique          */
/*    et en Automatique. All rights reserved.                                    */
/*                                                                               */
/*    This program is free software; you can redistribute it and/or modify       */
/*    it under the terms of the GNU General Public License version 3             */
/*    or later as published by the Free Software Foundation.                     */
/*                                                                               */
/*    This program is distributed in the hope that it will be useful,            */
/*    but WITHOUT ANY WARRANTY; without even the implied warranty of             */
/*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              */
/*    GNU General Public License for more details.                               */
/*                                                                               */
/*    You should have received a copy of the GNU General Public License          */
/*    along with this program; if not, write to the Free Software Foundation,    */
/*    Inc., 59 Temple Place, Suite 330, Boston, MA                               */
/*    02111-1307  USA                                                            */
/*                                                                               */
/*    Contact: Maxence.Guesdon@inria.fr                                          */
/*                                                                               */
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
  k=Entry_kind LBRACE id=Ident COMMA l=separated_list(COMMA, field) RBRACE
  {
    { kind = k ; id = id ; fields = l }
  }
field:
  n=Ident EQUAL s=String { (n, s) }
| n=Ident s=Equal_braced_string { (n, s) }