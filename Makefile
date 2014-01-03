
OCAMLDEP = ocamldep.opt
OCAMLC   = ocamlcp
OCAMLOPT = ocamlopt.opt

OCAMLCFLAGS   = unix.cma
OCAMLOPTFLAGS = unix.cmxa

EXEC = ant_conflict
FILE = constants.ml types.ml print.ml ant.ml main.ml

CMO=$(FILE:.ml=.cmo)
CMX=$(FILE:.ml=.cmx)
CMI=$(FILE:.ml=.cmi)

default: allopt

clean:
	rm -rf ${CMX} ${CMO} ${CMI} *.o ${EXEC} .depend log/

all: ${CMO} .depend
	${OCAMLC} ${OCAMLCFLAGS} ${CMO} -o ${EXEC}

allopt: ${CMX} .depend
	${OCAMLOPT} ${OCAMLOPTFLAGS} ${CMX} -o ${EXEC}

run: default
	./${EXEC}

# Common rules
.SUFFIXES: .ml .mli .cmo .cmx .cmi

.ml.cmo:
	$(OCAMLC) $(OCAMLCFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $< -o bin/$@

.depend:
	$(OCAMLDEP) ${FILE} >| .depend

include .depend
