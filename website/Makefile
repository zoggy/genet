STOGDIR=/home/guesdon/Personal/stog
STOG=$(STOGDIR)/stog.byte
DEST_DIR=/tmp/genet
BASE_URL_OPTION=
STOG_OPTIONS=-d $(DEST_DIR) $(BASE_URL_OPTION)
LESSC=lessc

build:
	rm -fr $(DEST_DIR)
	$(MAKE) site


site:
	$(STOG) $(STOG_OPTIONS) .
	$(LESSC) tmpl/style.less > $(DEST_DIR)/style.css
	cp -f *.svg $(DEST_DIR)/

test:
	$(MAKE) BASE_URL_OPTION="--base-url file://$(DEST_DIR)" site
