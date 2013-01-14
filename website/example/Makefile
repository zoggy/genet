include ../../master.Makefile

EXAMPLES= \
	average-0.1/average.x \
	average-0.2/average.x \
	split-text-0.1/split_text.x \
	words-0.2/words.x \
	words-0.4/words.x

all: $(EXAMPLES)

%.x: %.ml
	$(OCAMLOPT) -o $@ $<