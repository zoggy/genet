STOGDIR=/home/guesdon/Personal/stog
STOG=$(STOGDIR)/stog
DEST_DIR=/home/guesdon/devel/genet-gh-pages
BASE_URL_OPTION=
STOG_OPTIONS=-d $(DEST_DIR) $(BASE_URL_OPTION)
LESSC=lessc

build:
	$(MAKE) site


site: genet_plugin.cmxs
	$(STOG) $(STOG_OPTIONS) --plugin $< .
	$(LESSC) less/style.less > $(DEST_DIR)/style.css

test:
	$(MAKE) BASE_URL_OPTION="--site-url file://$(DEST_DIR)" site

genet_plugin.cmxs: genet_plugin.ml
	ocamlfind ocamlopt -shared -package stog -rectypes -o $@  $<

clean:
	rm -f *.cmxs
