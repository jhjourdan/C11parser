##########################################################################
#                    Jacques-Henri Jourdan, Inria Paris                  #
#                      François Pottier, Inria Paris                     #
#                                                                        #
#  Copyright Inria. All rights reserved. This file is distributed under  #
#  the terms of the GNU General Public License as published by the Free  #
#  Software Foundation, either version 2 of the License, or (at your     #
#  option) any later version.                                            #
##########################################################################

.PHONY: all test clean realclean

MENHIR := menhir --no-stdlib --unused-token IMAGINARY -lg 1 -la 1 -v

all:
	ocamlbuild -menhir "$(MENHIR)" main.native
	mv main.native parse

test: all
	cram tests/tests.t

clean:
	rm -rf _build parse
	rm -f parser.output parser.tab.c

parser.y: parser.mly
	$(MENHIR) --only-preprocess-uu $< > $@
	bison -r all $@

realclean: clean
	rm -f parser.y
