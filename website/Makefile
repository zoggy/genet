STOGDIR=/home/guesdon/Personal/stog
STOG=$(STOGDIR)/stog
DEST_DIR=/home/guesdon/devel/genet-gh-pages
BASE_URL_OPTION=
STOG_OPTIONS=-d $(DEST_DIR) $(BASE_URL_OPTION) --package stog.disqus
LESSC=lessc
CP=cp -f

build:
	$(MAKE) site

style.css: less/*.less
	$(LESSC) less/style.less > $@

site: genet_plugin.cmxs cmdline_doc.xml style.css arch.svg
	$(STOG) $(STOG_OPTIONS) --plugin $< .
	$(CP) style.css $(DEST_DIR)/style.css
	mkdir -p $(DEST_DIR)/refdoc
	(cd ../src && make doc && cp -r ocamldoc/* $(DEST_DIR)/refdoc/ )

cmdline_doc.xml: ../src/genet-cheat
	$< --doc > $@

arch.svg: arch.dot
	dot -Tsvg $^ | tail -n +4 > $@

test:
	$(MAKE) BASE_URL_OPTION="--site-url file://$(DEST_DIR)" site

genet_plugin.cmxs: genet_plugin.ml
	ocamlfind ocamlopt -shared -package stog -rectypes -o $@  $<

clean:
	rm -f *.cmxs

examples:
	cd example && $(MAKE) all
