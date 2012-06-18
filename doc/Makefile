include ../master.Makefile

all: genet.rdf

install:

genet.rdf: genet.ttl
	@$(RM) test
	@rdfproc -s file test parse $< turtle
	@rdfproc -s file test serialize > $@
	@$(RM) test

clean:
	$(RM) genet.rdf

distclean: clean

