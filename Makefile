# ######################################################################
#                                                                      #
#          Jacques-Henri Jourdan, INRIA Paris-Rocquencourt             #
#                                                                      #
#  Copyright Institut National de Recherche en Informatique et en      #
#  Automatique.  All rights reserved.  This file is distributed        #
#  under the terms of the GNU General Public License as published by   #
#  the Free Software Foundation, either version 2 of the License, or   #
#  (at your option) any later version.  This file is also distributed  #
#  under the terms of the INRIA Non-Commercial License Agreement.      #
#                                                                      #
# ######################################################################

.PHONY: all clean realclean

MENHIR := menhir --no-stdlib --unused-token IMAGINARY -lg 1 -la 1 -v

all:
	ocamlbuild -menhir "$(MENHIR)" lexer.native
	mv lexer.native parse

clean:
	rm -rf _build parse
	rm -f parser.output parser.tab.c

parser.y: parser.mly
	$(MENHIR) --only-preprocess-uu $< > $@
	bison -r all $@

realclean: clean
	rm -f parser.y
