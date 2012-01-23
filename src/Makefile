include ../master.Makefile

SYSTEM_INCLUDES= \
	-I +ocamldoc \
	$(LABLGTK2_INCLUDES) \
	$(LABLGTKEXTRAS_INCLUDES) \
	$(MENHIR_INCLUDES) \
	$(BOXES_INCLUDES)

MY_INCLUDES= \
	-I base \
	-I driver

INCLUDES= \
	$(SYSTEM_INCLUDES) \
	$(MY_INCLUDES)

COMPFLAGS=$(INCLUDES)
LINKFLAGS=$(INCLUDES)
LINKFLAGS_BYTE=$(INCLUDES)

#OCAMLPP=-pp "$(OCAMLBIN)/camlp4o "

TOOLS=$(GENET)
TOOLS_BYTE=$(GENET_BYTE)

SYSTEM_LIBS=str.cmxa unix.cmxa $(MENHIR_LINK_FLAGS)
SYSTEM_LIBS_BYTE=str.cma unix.cma $(MENHIR_LINK_FLAGS_BYTE)

LIBS=$(LIB_BASE)
LIBS_BYTE=$(LIB_BASE_BYTE)

LIBS_GUI=$(LIB_BASE_GUI)
LIBS_BYTE=$(LIB_BASE_GUI_BYTE)

GTK_LIBS=lablgtk.cmxa lablglade.cmxa lablgnomecanvas.cmxa gmylist.cmx boxes.cmxa
GTK_LIBS_BYTE=lablgtk.cma lablglade.cma lablgnomecanvas.cma gmylist.cmo boxes.cma

# Compilation
#############

# Base library

LIB_BASE=base/base.cmxa
LIB_BASE_BYTE=$(LIB_BASE:.cmxa=.cma)
LIB_BASE_CMI=$(LIB_BASE:.cmxa=.cmi)
LIB_BASE_A=$(LIB_BASE:.cmxa=.a)

BASE_CMXFILES=base/version.cmx \

#	base/constant.cmx \
#	base/base_types.cmx \
#	base/colors.cmx \
#	base/misc.cmx \
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

# Genet files
GENET_CMXFILES=	driver/options.cmx \
	driver/main.cmx
GENET_CMOFILES=$(GENET_CMXFILES:.cmx=.cmo)
GENET_CMIFILES=$(GENET_CMXFILES:.cmx=.cmi)

opt: libs tools
all: opt byte
byte: libs_byte tools_byte
libs: $(LIBS) $(LIBS_GUI)
libs_byte: $(LIB_BYTE) $(LIBS_GUI_BYTE)

tools: libs $(TOOLS)
tools_byte: libs_byte $(TOOLS_BYTE)

$(GENET): $(LIB_BASE)	$(GENET_CMXFILES)
	$(OCAMLOPT) $(OCAMLCFLAGS) -o $@ $(LINKFLAGS) $(SYSTEM_LIBS) \
	$^

$(GENET_BYTE): $(LIB_BASE_BYTE)	$(GENET_CMOFILES)
	$(OCAMLC) $(OCAMLCFLAGS) -o $@ $(LINKFLAGS) $(SYSTEM_LIBS_BYTE) \
	$^

$(LIB_BASE): $(BASE_CMIFILES) $(BASE_CMXFILES)
	$(OCAMLOPT) -a -o $@ $(LINKFLAGS) $(BASE_CMXFILES)
$(LIB_BASE_BYTE): $(BASE_CMIFILES) $(BASE_CMOFILES)
	$(OCAMLC) -a -o $@ $(LINKFLAGS) $(BASE_CMOFILES)

$(LIB_BASE_GUI): $(BASE_GUI_CMIFILES) $(BASE_GUI_CMXFILES)
	$(OCAMLOPT) -a -o $@ $(LINKFLAGS) $(BASE_GUI_CMXFILES)
$(LIB_BASE_GUI_BYTE): $(BASE_GUI_CMIFILES) $(BASE_GUI_CMOFILES)
	$(OCAMLC) -a -o $@ $(LINKFLAGS) $(BASE_GUI_CMOFILES)

KO='\033[1;31m'"*KO\033[1m\033[0m"
OK='\033[1;32m'"OK\033[1m\033[0m"


# Documentation :
#################
dump.odoc: base/*.ml base/*.mli \
	driver/*.ml driver/*.mli
	$(OCAMLDOCOPT) \
	-v \
	-m A \
	$(INCLUDES) -keep-code $(COMPFLAGS) -sort -dump $@ \
	`ls $^ | grep  -v -e "_parser.ml$$" `

doc: all dump.odoc
	$(MKDIR) ocamldoc
	$(CP) ../doc/ocamldoc_style.css ocamldoc/style.css
	$(OCAMLDOCOPT) $(COMPFLAGS) -v -d ocamldoc -load dump.odoc \
	-t "Genet library reference" \
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


.depend depend:
	$(RM) .depend
	$(OCAMLDEP) $(MY_INCLUDES) \
	base/*.ml base/*.mli \
	driver/*.ml driver/*.mli \
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
	$(RM) $(DISTRIB_DIR)/$(GENET_COMPILE_TEST)
	$(CP) $(ROOT)/doc/syndex_manual.pdf \
	$(ROOT)/doc/syndex_manual*.html \
	$(ROOT)/doc/ocamldoc_style.css \
	$(DISTRIB_DIR)/manual/
	$(CP) $(ROOT)/doc/illustrations/*.png $(DISTRIB_DIR)/manual/illustrations/
	(cd `dirname $(DISTRIB_DIR)` && tar cvfz $(DISTRIB_TAR) `basename $(DISTRIB_DIR)`)


#################
# Installation
#################
install:

###########################
# additional dependencies
###########################
base/tokens.ml base/tokens.mli: base/tokens.mly
	$(MENHIR) $<

base/lexer.ml base/lexer.mli: base/lexer.mll
	$(OCAMLLEX) $<
	$(OCAMLC) $(INCLUDES) -i base/lexer.ml > base/lexer.mli

algo/algo_lang_parser.ml algo/algo_lang_parser.mli: base/tokens.mly algo/algo_lang_parser.mly
	$(MENHIR) --explain --base algo/algo_lang_parser --external-tokens Tokens $^

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
