include ../master.Makefile

SYSTEM_INCLUDES= \
	-I +ocamldoc \
	$(LABLGTK2_INCLUDES) \
	$(LABLGTKEXTRAS_INCLUDES) \
	$(MENHIR_INCLUDES) \
	$(BOXES_INCLUDES) \
	$(OCAMLRDF_INCLUDES) \
	$(XMLM_INCLUDES) \
	`ocamlfind query -i-format xtmpl` \
	`ocamlfind query -i-format equeue` \
	`ocamlfind query -i-format pcre` \
	`ocamlfind query -i-format netcgi2` \
	`ocamlfind query -i-format netsys` \
	`ocamlfind query -i-format netstring` \
	`ocamlfind query -i-format rpc` \
	`ocamlfind query -i-format netplex` \
	`ocamlfind query -i-format nethttpd` \
	`ocamlfind query -i-format easy-format biniou yojson`

MY_INCLUDES= \
	-I base \
	-I grdf \
	-I chains \
	-I rest \
	-I driver \
	-I ocsigen

INCLUDES= \
	$(MY_INCLUDES) \
	$(SYSTEM_INCLUDES)

COMPFLAGS=$(INCLUDES)
LINKFLAGS=$(INCLUDES)
LINKFLAGS_BYTE=$(INCLUDES)

#OCAMLPP=-pp "$(OCAMLBIN)/camlp4o "

TOOLS=$(GENET) $(GENRDFS) $(GENET_QUERY) $(GENET_REST)
TOOLS_BYTE=$(GENET_BYTE) $(GENRDFS_BYTE) $(GENET_QUERY_BYTE) \
	$(GENET_REST_BYTE)

SYSTEM_LIBS=str.cmxa unix.cmxa $(MENHIR_LINK_FLAGS) config_file.cmx
SYSTEM_LIBS_BYTE=str.cma unix.cma $(MENHIR_LINK_FLAGS_BYTE) config_file.cmo

LIBS=$(LIB_BASE) $(LIB_GRDF) $(LIB_REST)
LIBS_BYTE=$(LIB_BASE_BYTE) $(LIB_GRDF_BYTE) $(LIB_REST_BYTE)

LIBS_GUI=$(LIB_BASE_GUI) $(LIB_GRDF_GUI)
LIBS_BYTE=$(LIB_BASE_GUI_BYTE) $(LIB_GRDF_GUI_BYTE)

GTK_LIBS=lablgtk.cmxa lablglade.cmxa lablgnomecanvas.cmxa gmylist.cmx boxes.cmxa
GTK_LIBS_BYTE=lablgtk.cma lablglade.cma lablgnomecanvas.cma gmylist.cmo boxes.cma

RDF_LIBS=ocamlrdf.cmxa
RDF_LIBS_BYTE=ocamlrdf.cma

XTMPL_LIBS=xmlm.cmxa xtmpl.cmx
XTMPL_LIBS_BYTE=xmlm.cma xtmpl.cmo

OCAMLNET_LIBS= \
	bigarray.cmxa pcre.cmxa \
	netsys_oothr_mt.cmxa netsys.cmxa equeue.cmxa \
	netstring.cmxa netcgi.cmxa rpc.cmxa \
	netplex.cmxa nethttpd.cmxa netsys_oothr_mt_init.cmx
OCAMLNET_LIBS_BYTE= \
	bigarray.cma pcre.cma \
	netsys_oothr_mt.cma netsys.cma equeue.cma \
	netstring.cma netcgi.cma rpc.cma \
	netplex.cma nethttpd.cma netsys_oothr_mt_init.cmo

JSON_LIBS= easy_format.cmx biniou.cmxa yojson.cmx
JSON_LIBS_BYTE= easy_format.cmo biniou.cma yojson.cmo

# Compilation
#############

# Base library

LIB_BASE=base/base.cmxa
LIB_BASE_BYTE=$(LIB_BASE:.cmxa=.cma)
LIB_BASE_CMI=$(LIB_BASE:.cmxa=.cmi)
LIB_BASE_A=$(LIB_BASE:.cmxa=.a)

BASE_CMXFILES=base/version.cmx \
	base/install.cmx \
	base/sset.cmx \
	base/misc.cmx \
	base/config.cmx \
	base/checks.cmx \
	base/loc.cmx \
	base/cmdline.cmx

#	base/constant.cmx \
#	base/base_types.cmx \
#	base/colors.cmx \
#	base/additional.cmx \
#	base/checks.cmx \
#	base/tmap.cmx \
#	base/graph.cmx \
#	base/loc.cmx \
#	base/tokens.cmx \
#	base/lexer.cmx \
#	base/history.cmx \
#	base/twdict.cmx \
#	base/zipper.cmx \
#	base/env.cmx
BASE_CMOFILES=$(BASE_CMXFILES:.cmx=.cmo)
BASE_CMIFILES=$(BASE_CMXFILES:.cmx=.cmi)

BASE_LIBS=$(LIB_BASE) $(LIB_BASE_BYTE) $(LIB_BASE_CMI) $(LIB_BASE_A)

BASE_GUI_CMXFILES=
BASE_GUI_CMIFILES=$(BASE_GUI_CMXFILES:.cmx=.cmi)
BASE_GUI_CMOFILES=$(BASE_GUI_CMXFILES:.cmx=.cmo)

LIB_BASE_GUI=base/basegui.cmxa
LIB_BASE_GUI_BYTE=$(LIB_BASE_GUI:.cmxa=.cma)
LIB_BASE_GUI_CMI=$(LIB_BASE_GUI:.cmxa=.cmi)
LIB_BASE_GUI_A=$(LIB_BASE_GUI:.cmxa=.a)

BASE_GUI_LIBS=$(LIB_BASE_GUI) $(LIB_BASE_GUI_BYTE) $(LIB_BASE_GUI_CMI) $(LIB_BASE_GUI_A)

# Grdf library

LIB_GRDF=grdf/grdf.cmxa
LIB_GRDF_BYTE=$(LIB_GRDF:.cmxa=.cma)
LIB_GRDF_CMI=$(LIB_GRDF:.cmxa=.cmi)
LIB_GRDF_A=$(LIB_GRDF:.cmxa=.a)

GRDF_CMXFILES=grdf/grdf_constant.cmx \
	grdf/grdf_types.cmx \
	grdf/rdf_sparql.cmx \
	grdf/grdfs.cmx \
	grdf/grdf_branch.cmx \
	grdf/grdf_ftype.cmx \
	grdf/grdf_version.cmx \
	grdf/grdf_intf.cmx \
	grdf/grdf_tool.cmx \
	grdf/grdf_init.cmx \
	grdf/grdf_dot.cmx

#	grdf/constant.cmx \
#	grdf/grdf_types.cmx \
#	grdf/colors.cmx \
#	grdf/additional.cmx \
#	grdf/checks.cmx \
#	grdf/tmap.cmx \
#	grdf/graph.cmx \
#	grdf/loc.cmx \
#	grdf/tokens.cmx \
#	grdf/lexer.cmx \
#	grdf/history.cmx \
#	grdf/twdict.cmx \
#	grdf/zipper.cmx \
#	grdf/env.cmx
GRDF_CMOFILES=$(GRDF_CMXFILES:.cmx=.cmo)
GRDF_CMIFILES=$(GRDF_CMXFILES:.cmx=.cmi)

GRDF_LIBS=$(LIB_GRDF) $(LIB_GRDF_BYTE) $(LIB_GRDF_CMI) $(LIB_GRDF_A)

GRDF_GUI_CMXFILES=
GRDF_GUI_CMIFILES=$(GRDF_GUI_CMXFILES:.cmx=.cmi)
GRDF_GUI_CMOFILES=$(GRDF_GUI_CMXFILES:.cmx=.cmo)

LIB_GRDF_GUI=grdf/grdfgui.cmxa
LIB_GRDF_GUI_BYTE=$(LIB_GRDF_GUI:.cmxa=.cma)
LIB_GRDF_GUI_CMI=$(LIB_GRDF_GUI:.cmxa=.cmi)
LIB_GRDF_GUI_A=$(LIB_GRDF_GUI:.cmxa=.a)

GRDF_GUI_LIBS=$(LIB_GRDF_GUI) $(LIB_GRDF_GUI_BYTE) $(LIB_GRDF_GUI_CMI) $(LIB_GRDF_GUI_A)

# Chains library
LIB_CHN=chains/chains.cmxa
LIB_CHN_BYTE=$(LIB_CHN:.cmxa=.cma)
LIB_CHN_CMI=$(LIB_CHN:.cmxa=.cmi)
LIB_CHN_A=$(LIB_CHN:.cmxa=.a)

CHN_CMXFILES=chains/chn_types.cmx \
	chains/chn_ast.cmx \
	chains/chn_parser.cmx

#	grdf/constant.cmx \
#	grdf/grdf_types.cmx \
#	grdf/colors.cmx \
#	grdf/additional.cmx \
#	grdf/checks.cmx \
#	grdf/tmap.cmx \
#	grdf/graph.cmx \
#	grdf/loc.cmx \
#	grdf/tokens.cmx \
#	grdf/lexer.cmx \
#	grdf/history.cmx \
#	grdf/twdict.cmx \
#	grdf/zipper.cmx \
#	grdf/env.cmx
CHN_CMOFILES=$(CHN_CMXFILES:.cmx=.cmo)
CHN_CMIFILES=$(CHN_CMXFILES:.cmx=.cmi)

CHN_LIBS=$(LIB_CHN) $(LIB_CHN_BYTE) $(LIB_CHN_CMI) $(LIB_CHN_A)

CHN_GUI_CMXFILES=
CHN_GUI_CMIFILES=$(CHN_GUI_CMXFILES:.cmx=.cmi)
CHN_GUI_CMOFILES=$(CHN_GUI_CMXFILES:.cmx=.cmo)

LIB_CHN_GUI=grdf/grdfgui.cmxa
LIB_CHN_GUI_BYTE=$(LIB_CHN_GUI:.cmxa=.cma)
LIB_CHN_GUI_CMI=$(LIB_CHN_GUI:.cmxa=.cmi)
LIB_CHN_GUI_A=$(LIB_CHN_GUI:.cmxa=.a)

CHN_GUI_LIBS=$(LIB_CHN_GUI) $(LIB_CHN_GUI_BYTE) $(LIB_CHN_GUI_CMI) $(LIB_CHN_GUI_A)

# Rest library

LIB_REST=rest/rest.cmxa
LIB_REST_BYTE=$(LIB_REST:.cmxa=.cma)
LIB_REST_CMI=$(LIB_REST:.cmxa=.cmi)
LIB_REST_A=$(LIB_REST:.cmxa=.a)

REST_CMXFILES= \
	rest/rest_types.cmx \
	rest/rest_xpage.cmx \
	rest/rest_xhtml.cmx \
	rest/rest_query.cmx

#	grdf/constant.cmx \
#	grdf/grdf_types.cmx \
#	grdf/colors.cmx \
#	grdf/additional.cmx \
#	grdf/checks.cmx \
#	grdf/tmap.cmx \
#	grdf/graph.cmx \
#	grdf/loc.cmx \
#	grdf/tokens.cmx \
#	grdf/lexer.cmx \
#	grdf/history.cmx \
#	grdf/twdict.cmx \
#	grdf/zipper.cmx \
#	grdf/env.cmx
REST_CMOFILES=$(REST_CMXFILES:.cmx=.cmo)
REST_CMIFILES=$(REST_CMXFILES:.cmx=.cmi)

REST_LIBS=$(LIB_REST) $(LIB_REST_BYTE) $(LIB_REST_CMI) $(LIB_REST_A)

# Genet files
GENET_CMXFILES=	driver/options.cmx \
	driver/main.cmx
GENET_CMOFILES=$(GENET_CMXFILES:.cmx=.cmo)
GENET_CMIFILES=$(GENET_CMXFILES:.cmx=.cmi)

# Genet-genrdfs files
GENRDFS_CMXFILES= driver/options.cmx \
	driver/main_genrdfs.cmx
GENRDFS_CMOFILES=$(GENRDFS_CMXFILES:.cmx=.cmo)
GENRDFS_CMIFILES=$(GENRDFS_CMXFILES:.cmx=.cmi)

# Genet-query files
GENET_QUERY_CMXFILES=	driver/options.cmx \
	driver/main_query.cmx
GENET_QUERY_CMOFILES=$(GENET_QUERY_CMXFILES:.cmx=.cmo)
GENET_QUERY_CMIFILES=$(GENET_QUERY_CMXFILES:.cmx=.cmi)

# Genet-rest files
GENET_REST_CMXFILES=\
	driver/options.cmx \
	driver/main_rest.cmx
GENET_REST_CMOFILES=$(GENET_REST_CMXFILES:.cmx=.cmo)
GENET_REST_CMIFILES=$(GENET_REST_CMXFILES:.cmx=.cmi)

opt: libs tools
all: opt byte
byte: libs_byte tools_byte
libs: $(LIBS) $(LIBS_GUI)
libs_byte: $(LIB_BYTE) $(LIBS_GUI_BYTE)

tools: libs $(TOOLS)
tools_byte: libs_byte $(TOOLS_BYTE)

$(GENET): $(LIB_BASE)	$(LIB_GRDF) $(LIB_CHN) $(GENET_CMXFILES)
	$(OCAMLOPT) $(OCAMLCFLAGS) -o $@ $(LINKFLAGS) $(SYSTEM_LIBS) \
	$(RDF_LIBS)	$^

$(GENET_BYTE): $(LIB_BASE_BYTE)	$(LIB_GRDF_BYTE) $(LIB_CHN_BYTE) $(GENET_CMOFILES)
	$(OCAMLC) $(OCAMLCFLAGS) -o $@ $(LINKFLAGS) $(SYSTEM_LIBS_BYTE) \
	$(RDF_LIBS_BYTE) $^

$(GENRDFS): $(LIB_BASE)	$(LIB_GRDF) $(GENRDFS_CMXFILES)
	$(OCAMLOPT) $(OCAMLCFLAGS) -o $@ $(LINKFLAGS) $(SYSTEM_LIBS) \
	$(RDF_LIBS)	$^

$(GENRDFS_BYTE): $(LIB_BASE_BYTE)	$(LIB_GRDF_BYTE) $(GENRDFS_CMOFILES)
	$(OCAMLC) $(OCAMLCFLAGS) -o $@ $(LINKFLAGS) $(SYSTEM_LIBS_BYTE) \
	$(RDF_LIBS_BYTE) $^

$(GENET_QUERY): $(LIB_BASE)	$(LIB_GRDF) $(LIB_CHN) $(GENET_QUERY_CMXFILES)
	$(OCAMLOPT) $(OCAMLCFLAGS) -o $@ $(LINKFLAGS) $(SYSTEM_LIBS) \
	$(RDF_LIBS)	$^

$(GENET_QUERY_BYTE): $(LIB_BASE_BYTE)	$(LIB_GRDF_BYTE) $(LIB_CHN_BYTE) $(GENET_QUERY_CMOFILES)
	$(OCAMLC) $(OCAMLCFLAGS) -o $@ $(LINKFLAGS) $(SYSTEM_LIBS_BYTE) \
	$(RDF_LIBS_BYTE) $^

$(GENET_REST): $(LIB_BASE)	$(LIB_GRDF) $(LIB_CHN) $(LIB_REST) $(GENET_REST_CMXFILES)
	$(OCAMLOPT) $(OCAMLCFLAGS) -o $@ $(LINKFLAGS) $(SYSTEM_LIBS) \
	$(RDF_LIBS)	-thread threads.cmxa $(XTMPL_LIBS) $(JSON_LIBS) $(OCAMLNET_LIBS) $^

$(GENET_REST_BYTE): $(LIB_BASE_BYTE)	$(LIB_GRDF_BYTE) $(LIB_CHN_BYTE) $(LIB_REST_BYTE) $(GENET_REST_CMOFILES)
	$(OCAMLC) $(OCAMLCFLAGS) -o $@ $(LINKFLAGS) $(SYSTEM_LIBS_BYTE) \
	$(RDF_LIBS_BYTE) -thread threads.cma $(XTMPL_LIBS_BYTE) $(JSON_LIBS_BYTE) $(OCAMLNET_LIBS_BYTE) $^

$(LIB_BASE): $(BASE_CMIFILES) $(BASE_CMXFILES)
	$(OCAMLOPT) -a -o $@ $(LINKFLAGS) $(BASE_CMXFILES)
$(LIB_BASE_BYTE): $(BASE_CMIFILES) $(BASE_CMOFILES)
	$(OCAMLC) -a -o $@ $(LINKFLAGS) $(BASE_CMOFILES)

$(LIB_BASE_GUI): $(BASE_GUI_CMIFILES) $(BASE_GUI_CMXFILES)
	$(OCAMLOPT) -a -o $@ $(LINKFLAGS) $(BASE_GUI_CMXFILES)
$(LIB_BASE_GUI_BYTE): $(BASE_GUI_CMIFILES) $(BASE_GUI_CMOFILES)
	$(OCAMLC) -a -o $@ $(LINKFLAGS) $(BASE_GUI_CMOFILES)


$(LIB_GRDF): $(GRDF_CMIFILES) $(GRDF_CMXFILES)
	$(OCAMLOPT) -a -o $@ $(LINKFLAGS) $(GRDF_CMXFILES)
$(LIB_GRDF_BYTE): $(GRDF_CMIFILES) $(GRDF_CMOFILES)
	$(OCAMLC) -a -o $@ $(LINKFLAGS) $(GRDF_CMOFILES)

$(LIB_GRDF_GUI): $(GRDF_GUI_CMIFILES) $(GRDF_GUI_CMXFILES)
	$(OCAMLOPT) -a -o $@ $(LINKFLAGS) $(GRDF_GUI_CMXFILES)
$(LIB_GRDF_GUI_BYTE): $(GRDF_GUI_CMIFILES) $(GRDF_GUI_CMOFILES)
	$(OCAMLC) -a -o $@ $(LINKFLAGS) $(GRDF_GUI_CMOFILES)

$(LIB_CHN): $(CHN_CMIFILES) $(CHN_CMXFILES)
	$(OCAMLOPT) -a -o $@ $(LINKFLAGS) $(CHN_CMXFILES)
$(LIB_CHN_BYTE): $(CHN_CMIFILES) $(CHN_CMOFILES)
	$(OCAMLC) -a -o $@ $(LINKFLAGS) $(CHN_CMOFILES)

$(LIB_CHN_GUI): $(CHN_GUI_CMIFILES) $(CHN_GUI_CMXFILES)
	$(OCAMLOPT) -a -o $@ $(LINKFLAGS) $(CHN_GUI_CMXFILES)
$(LIB_CHN_GUI_BYTE): $(CHN_GUI_CMIFILES) $(CHN_GUI_CMOFILES)
	$(OCAMLC) -a -o $@ $(LINKFLAGS) $(CHN_GUI_CMOFILES)


$(LIB_REST): $(REST_CMIFILES) $(REST_CMXFILES)
	$(OCAMLOPT) -a -o $@ $(LINKFLAGS) $(REST_CMXFILES)
$(LIB_REST_BYTE): $(REST_CMIFILES) $(REST_CMOFILES)
	$(OCAMLC) -a -o $@ $(LINKFLAGS) $(REST_CMOFILES)

KO='\033[1;31m'"*KO\033[1m\033[0m"
OK='\033[1;32m'"OK\033[1m\033[0m"


# Documentation :
#################
dump.odoc: base/*.ml base/*.mli \
	grdf/*.ml grdf/*.mli \
	chains/*.ml chains/*.mli \
	rest/*.ml rest/*.mli \
	driver/*.ml driver/*.mli
	$(OCAMLDOCOPT) \
	-v \
	-m A \
	$(INCLUDES) -keep-code $(COMPFLAGS) -rectypes -sort -dump $@ \
	`ls $^ | grep  -v -e "_parser.ml$$" `

doc: all dump.odoc
	$(MKDIR) ocamldoc
	$(CP) ../doc/ocamldoc_style.css ocamldoc/style.css
	$(OCAMLDOCOPT) $(COMPFLAGS) -v -d ocamldoc -load dump.odoc \
	-t "Genet library reference" -html \
	-intro ../doc/ocamldoc_index.text

dot: all dump.odoc
	$(OCAMLDOCOPT) -dot -load dump.odoc -o syndex.dot
	dot -Tpng -o syndex.png syndex.dot


# backup, clean and depend :
############################

GENERATED_FILES=

cleanocamldoc:
	$(RM) -r ocamldoc
	$(MKDIR) ocamldoc

distclean: clean
	$(RM) base/version.ml
	$(MAKE) cleanocamldoc

clean: dummy
	$(RM) *~ \#*\#
	$(RM) */*.a */*.so *.odoc
	$(RM) $(TOOLS) $(TOOLS_BYTE)
	$(RM) $(LIBS) $(LIBS_BYTE)
	$(RM) $(GENERATED_FILES)
	(cd base && $(RM) *~ \#*\# *.cmo *.cmi *.cmx *.a *.o *.cma *.cmxa *.annot *.odoc)
	(cd driver && $(RM) *~ \#*\# *.cmo *.cmi *.cmx *.a *.o *.cma *.cmxa *.annot *.odoc)
	(cd grdf && $(RM) *~ \#*\# *.cmo *.cmi *.cmx *.a *.o *.cma *.cmxa *.annot *.odoc)
	(cd chains && $(RM) *~ \#*\# *.cmo *.cmi *.cmx *.a *.o *.cma *.cmxa *.annot *.odoc)
	(cd rest && $(RM) *~ \#*\# *.cmo *.cmi *.cmx *.a *.o *.cma *.cmxa *.annot *.odoc)


.depend depend:
	$(RM) .depend
	$(OCAMLDEP) $(MY_INCLUDES) \
	base/*.ml base/*.mli \
	driver/*.ml driver/*.mli \
	grdf/*.ml grdf/*.mli \
	chains/*.ml chains/*.mli \
	rest/*.ml rest/*.mli \
	  > .depend

alldepend: $(GENERATED_FILES)
	$(MAKE) depend
dummy:

include .depend


# distribution
# by now just a tar to allow alpha/beta-testing
DISTRIB_DIR=/tmp/syndex-distrib
DISTRIB_TAR=/tmp/syndex-$(VERSION).tar.gz
distrib: all
	$(RM) $(DISTRIB_DIR)
	$(MKDIR) $(DISTRIB_DIR)/manual
	$(MKDIR) $(DISTRIB_DIR)/manual/illustrations
	$(CP) $(TOOLS) */*.glade $(DISTRIB_DIR)/
	$(CP) $(ROOT)/doc/syndex_manual.pdf \
	$(ROOT)/doc/syndex_manual*.html \
	$(ROOT)/doc/ocamldoc_style.css \
	$(DISTRIB_DIR)/manual/
	$(CP) $(ROOT)/doc/illustrations/*.png $(DISTRIB_DIR)/manual/illustrations/
	(cd `dirname $(DISTRIB_DIR)` && tar cvfz $(DISTRIB_TAR) `basename $(DISTRIB_DIR)`)


#################
# Installation
#################
install: install-web-files

install-web-files:
	$(MKDIR) $(SHAREDIR)/web
	$(CP) -r tmpl $(SHAREDIR)/web/
	$(CP) $(ROOT)/website/genet-logo.svg $(SHAREDIR)/web/
	$(LESSC) $(ROOT)/website/tmpl/style.less > $(SHAREDIR)/web/style.css

###########################
# additional dependencies
###########################
chains/chn_parser.ml chains/chn_parser.mli: chains/chn_parser.mly
	$(MENHIR) --explain --base chains/chn_parser $^


base/tokens.ml base/tokens.mli: base/tokens.mly
	$(MENHIR) $<

base/lexer.ml base/lexer.mli: base/lexer.mll
	$(OCAMLLEX) $<
	$(OCAMLC) $(INCLUDES) -i base/lexer.ml > base/lexer.mli

arch/arch_lang_parser.ml arch/arch_lang_parser.mli: base/tokens.mly arch/arch_lang_parser.mly
	$(MENHIR) --base arch/arch_lang_parser --external-tokens Tokens $^

adeq/adeq_durations_parser.ml adeq/adeq_durations_parser.mli: base/tokens.mly algo/algo_lang_parser.mly adeq/adeq_durations_parser.mly
	$(MENHIR) --base adeq/adeq_durations_parser --external-tokens Tokens $^

adeq/adeq_constraints_parser.ml adeq/adeq_constraints_parser.mli: base/tokens.mly adeq/adeq_constraints_parser.mly
	$(MENHIR) --base adeq/adeq_constraints_parser --external-tokens Tokens $^

driver/app_parser.ml driver/app_parser.mli: \
	base/tokens.mly \
	algo/algo_lang_parser.mly \
	arch/arch_lang_parser.mly \
	adeq/adeq_durations_parser.mly \
	adeq/adeq_constraints_parser.mly \
	driver/app_parser.mly
	$(MENHIR) --base driver/app_parser --external-tokens Tokens $^

driver/test_parser.ml driver/test_parser.mli: \
	base/tokens.mly \
	algo/algo_lang_parser.mly \
	arch/arch_lang_parser.mly \
	adeq/adeq_durations_parser.mly \
	adeq/adeq_constraints_parser.mly \
	driver/test_parser.mly
	$(MENHIR) --base driver/test_parser --external-tokens Tokens $^

sdxparser/sdx_parser.cmo sdxparser/sdx_parser.cmx: sdxparser/sdx_parser.cmi
sdxparser/sdx_parser.ml sdxparser/sdx_parser.mli: sdxparser/sdx_parser.mly
	$(MENHIR) $^
