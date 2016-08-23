NAME=geoloc
PACKAGES=-package js_of_ocaml,gen_js_api,ocaml-googlemaps,js_of_ocaml.ppx
SYNTAX= -package lwt.ppx

all:
	ocamlfind ocamlc -c $(PACKAGES) $(SYNTAX) $(NAME).mli
	ocamlfind ocamlc -c $(PACKAGES) $(SYNTAX) $(NAME).ml
	ocamlfind ocamlc -a -no-check-prims -o $(NAME).cma $(PACKAGES) $(NAME).cmo

install: all
	ocamlfind install ocaml-geoloc META $(NAME).cma $(NAME).cmi

clean:
	rm *.cmo *.cmi
