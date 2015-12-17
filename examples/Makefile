# Makefile for examples
BYTES = bytes
CAML  = caml
JS    = js

# Port for the temp server
PORT  = 7777
HOST  = python -m SimpleHTTPServer $(PORT)

# If SANITIZE = 'yes', the temp files will be erased
# (temp files like .cmo, .byte etc.)
SANITIZE = 'yes'

# Compiler macros
OCAMLFIND = ocamlfind ocamlc	
PACKAGES  = -package js_of_ocaml -package js_of_ocaml.syntax
PACKAGES += -package drumaderian
SYNTAX    = -syntax camlp4o
COMPILER  = $(OCAMLFIND) $(PACKAGES) $(SYNTAX) -linkpkg -I $(CAML)


# ERASER
ifeq ($(SANITIZE), 'yes')
	RM = rm -rf $(BYTES)/* $(CAML)/*.cm*
endif

.PHONY: all
all: run 

initialize:
	mkdir -p $(BYTES) $(CAML) $(JS)

%.byte : $(CAML)/%.ml initialize
	$(COMPILER) -o $(BYTES)/$(@) $(<)

%.js : %.byte
	js_of_ocaml -o $(JS)/$(@) $(BYTES)/$(<)
	$(RM)

clean:
	rm -rf $(BYTES)
	rm -rf $(CAML)/*.cm*
	rm -rf *~
	rm -rf */*~

distclean: clean
	rm -rf $(JS)/*

run: game.js
	$(HOST)
