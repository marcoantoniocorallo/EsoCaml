TARGET=src/main

default: $(TARGET).native

$TARGET: default

native: $(TARGET).native

%.native:
	ocamlbuild -use-menhir -menhir "menhir --explain" -use-ocamlfind -package ppx_deriving.std $@
	mv main.native ./EsoCaml

clean:
	ocamlbuild -clean

.PHONY: clean default
