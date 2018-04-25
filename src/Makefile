OCAMLC   = ocamlfind ocamlc
OCAMLDEP = ocamlfind ocamldep

OBJS = common.cmi common.cmo \
	entities.cmi entities.cmo \
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
	${RM} *.cm[ioxat] *.cmti *.out

# Dependencies
.PHONY: depend
depend:
	$(OCAMLDEP) *.mli *.ml > .depend

include .depend
