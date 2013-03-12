include ../../master.Makefile

EXAMPLES= \
	average-0.1/average.x \
	average-0.2/average.x \
	average-0.3/average.x \
	split-text-0.1/split_text.x \
	split-text-0.2/split_text.x \
	words-0.2/words.x \
	words-0.4/words.x

all: $(EXAMPLES)

install: all
	mkdir -p /tmp/tools
	for i in */*.x; do cp -f $i /tmp/tools/`dirname $i`; done

%.x: %.ml
	$(OCAMLOPT) unix.cmxa -o $@ $<