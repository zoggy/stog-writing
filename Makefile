#################################################################################
#                Stog-writing                                                   #
#                                                                               #
#    Copyright (C) 2012-2013 Maxence Guesdon. All rights reserved.              #
#                                                                               #
#    This program is free software; you can redistribute it and/or modify       #
#    it under the terms of the GNU General Public License as                    #
#    published by the Free Software Foundation, version 3 of the License.       #
#                                                                               #
#    This program is distributed in the hope that it will be useful,            #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               #
#    GNU Library General Public License for more details.                       #
#                                                                               #
#    You should have received a copy of the GNU General Public                  #
#    License along with this program; if not, write to the Free Software        #
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   #
#    02111-1307  USA                                                            #
#                                                                               #
#    As a special exception, you have permission to link this program           #
#    with the OCaml compiler and distribute executables, as long as you         #
#    follow the requirements of the GNU GPL in regard to all of the             #
#    software in the executable aside from the OCaml compiler.                  #
#                                                                               #
#    Contact: Maxence.Guesdon@inria.fr                                          #
#                                                                               #
#################################################################################

VERSION=0.8.0

MKDIR=mkdir -p
CP=cp -f

OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDOC=ocamldoc
OCAMLLEX=ocamllex
OCAMLFIND=ocamlfind

PACKAGES=menhirLib,xtmpl,stog,config-file
MENHIR=menhir

INCLUDES=-I +ocamldoc
COMPFLAGS=$(INCLUDES) -g -annot -package $(PACKAGES) -rectypes
LINKFLAGS=$(INCLUDES)
LINKFLAGS_BYTE=$(INCLUDES)

PLUGIN=stog_writing.cmxs
PLUGIN_LIB=stog_writing.cmxa
PLUGIN_BYTE=$(PLUGIN:.cmxs=.cma)

all: byte opt
opt: $(PLUGIN) $(PLUGIN_LIB)
byte: $(PLUGIN_BYTE)

stog_writing.cmxs: bibtex.cmi bib_parser.cmi bib_parser.cmx bib_lexer.cmx stog_writing.cmx
	$(OCAMLFIND) ocamlopt -package menhirLib -linkpkg -shared -o $@ \
	$(LINKFLAGS) `ls $^ | grep -v cmi`

stog_writing.cmxa: bibtex.cmi bib_parser.cmi bib_parser.cmx bib_lexer.cmx stog_writing.cmx
	$(OCAMLFIND) ocamlopt -a -package menhirLib -o $@ \
	$(LINKFLAGS) `ls $^ | grep -v cmi`

stog_writing.cma: bibtex.cmi bib_parser.cmi bib_parser.cmo bib_lexer.cmo stog_writing.cmo
	$(OCAMLFIND) ocamlc -a -package menhirLib -o $@ \
	$(LINKFLAGS_BYTE) `ls $^ | grep -v cmi`

install:
	$(OCAMLFIND) install stog-writing META \
	$(PLUGIN) $(PLUGIN_BYTE) $(PLUGIN_LIB) $(PLUGIN_LIB:.cmxa=.a)

uninstall:
	$(OCAMLFIND) remove stog-writing

distclean: clean

clean:
	rm -f *.cm* *.o *.annot bib_parser.ml bib_parser.mli bib_lexer.ml

bib_parser.ml bib_parser.mli: bib_parser.mly
	$(MENHIR) $<

###########
archive:
	git archive --prefix=stog-writing-$(VERSION)/ HEAD | gzip > /tmp/stog-writing-$(VERSION).tar.gz

# headers :
###########
HEADFILES= Makefile stog_writing.ml bib_lexer.mll bib_parser.mly bibtex.mli
headers:
	echo $(HEADFILES)
	headache -h header -c .headache_config `ls $(HEADFILES) | grep -v plugin_example`

noheaders:
	headache -r -c .headache_config `ls $(HEADFILES)`

# Rules
.SUFFIXES: .mli .ml .cmi .cmo .cmx .mll .mly

%.cmi:%.mli
	$(OCAMLFIND) ocamlc $(COMPFLAGS) -c $<

%.cmo:%.ml
	if test -f `dirname $<`/`basename $< .ml`.mli && test ! -f `dirname $<`/`basename $< .ml`.cmi ; then \
	$(OCAMLFIND) ocamlc $(COMPFLAGS) -c `dirname $<`/`basename $< .ml`.mli; fi
	$(OCAMLFIND) ocamlc $(COMPFLAGS) -c $<

%.cmi %.cmo:%.ml
	if test -f `dirname $<`/`basename $< .ml`.mli && test ! -f `dirname $<`/`basename $< .ml`.cmi ; then \
	$(OCAMLFIND) ocamlc $(COMPFLAGS) -c `dirname $<`/`basename $< .ml`.mli; fi
	$(OCAMLFIND) ocamlc $(COMPFLAGS) -c $<

%.cmx %.o:%.ml
	$(OCAMLFIND) ocamlopt $(COMPFLAGS) -c $<

%.ml:%.mll
	$(OCAMLLEX) $<

.PHONY: clean depend

.depend depend:
	ocamldep *.ml > .depend


include .depend
