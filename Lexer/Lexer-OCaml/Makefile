#llama-lexer

OCAMLC_FLAGS=-g
OCAMLC=ocamlc

%.cmo: %.ml %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmo %.cmi: %.ml
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

llama: Lexer.cmo
	$(OCAMLC) $(OCAMLC_FLAGS) -o $@ $^

Lexer.ml: Lexer.mll
	ocamllex -o $@ $<

.PHONY: clean distclean

clean:
	$(RM) Lexer.ml *.cmo *.cmi *~

distclean: clean
	$(RM) llama
