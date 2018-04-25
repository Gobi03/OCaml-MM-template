OCAMLC   = ocamlfind ocamlc
OCAMLDEP = ocamlfind ocamldep

OBJS = entities.cmi entities.cmo \
	main.cmi main.cmo

.PHONY: all
all: ${OBJS}
	${OCAMLC} -o a.out $(filter-out %.cmi,$^)

# make submit file
.PHONY: submit
submit:
	./make-submit-file.py

# Common rules
.SUFFIXES: .ml .mli .cmi .cmo
%.cmi: %.mli
	${OCAMLC} ${INCLUDES} $<
%.cmi: %.ml
	$(OCAMLC) ${INCLUDES} -c $<
%.cmo: %.ml %.cmi
	$(OCAMLC) ${INCLUDES} -c $<

# Clean up
.PHONY: clean
clean:
	${RM} `find -name "*.cm[ioxat]" -or -name "*.cmti" -or -name "*.out"`

# Dependencies
.PHONY: depend
depend:
	$(OCAMLDEP) `find -name "*.mli" -or -name "*.ml"` > .depend

include .depend
