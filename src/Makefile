#################################################################################
#                Genet                                                          #
#                                                                               #
#    Copyright (C) 2012-2013 Institut National de Recherche en Informatique     #
#    et en Automatique. All rights reserved.                                    #
#                                                                               #
#    This program is free software; you can redistribute it and/or modify       #
#    it under the terms of the GNU General Public License version 3             #
#    or later as published by the Free Software Foundation.                     #
#                                                                               #
#    This program is distributed in the hope that it will be useful,            #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              #
#    GNU General Public License for more details.                               #
#                                                                               #
#    You should have received a copy of the GNU General Public License          #
#    along with this program; if not, write to the Free Software Foundation,    #
#    Inc., 59 Temple Place, Suite 330, Boston, MA                               #
#    02111-1307  USA                                                            #
#                                                                               #
#    Contact: Maxence.Guesdon@inria.fr                                          #
#                                                                               #
#                                                                               #
#################################################################################

include ../master.Makefile

PACKAGE=genet

# DO NOT FORGET TO UPDATE META.in ACCORDINGLY WHEN MODIFYING *_PACKAGES
BASE_PACKAGES=unix,str,config-file,$(RDF_MYSQL),$(RDF_POSTGRESQL),xtmpl,menhirLib
NET_PACKAGES=equeue,pcre,netcgi2,netsys,netstring,rpc,netplex,nethttpd,threads.posix
REST_PACKAGES=#,yojson
GUI_PACKAGES=lablgtk2.auto-init,lablgtk2-extras,lablgtk2.glade,dot.gtk

ALL_PACKAGES=$(BASE_PACKAGES),$(NET_PACKAGES),$(REST_PACKAGES),$(GUI_PACKAGES)
OF_FLAGS=-package $(ALL_PACKAGES)

SYSTEM_INCLUDES= \
	-I +ocamldoc

MY_INCLUDES= \
	-I base \
	-I grdf \
	-I indata \
	-I chains \
	-I rest \
	-I gui \
	-I driver

INCLUDES= \
	$(MY_INCLUDES) \
	$(SYSTEM_INCLUDES)

COMPFLAGS=$(INCLUDES) -thread # -thread for postgresql
LINKFLAGS=$(INCLUDES) -thread -linkall
LINKFLAGS_BYTE=$(INCLUDES) -linkall

#OCAMLPP=-pp "$(OCAMLBIN)/camlp4o "

TOOLS=$(GENET) $(GENRDFS) $(GENET_REST) $(GENET_CHEAT) $(GENET_GUI)
TOOLS_BYTE=$(GENET_BYTE) $(GENRDFS_BYTE) \
	$(GENET_REST_BYTE) $(GENET_CHEAT_BYTE) $(GENET_GUI_BYTE)

SYSTEM_LIBS=
SYSTEM_LIBS_BYTE=

LIBS=$(LIB_BASE) $(LIB_GRDF) $(LIB_IND) $(LIB_CHN) $(LIB_REST)
LIBS_BYTE=$(LIB_BASE_BYTE) $(LIB_GRDF_BYTE) $(LIB_IND_BYTE) $(LIB_CHN_BYTE) $(LIB_REST_BYTE)

LIBS_GUI=$(LIB_BASE_GUI) $(LIB_GRDF_GUI) $(LIB_CHN_GUI)
LIBS_GUI_BYTE=$(LIB_BASE_GUI_BYTE) $(LIB_GRDF_GUI_BYTE) $(LIB_CHN_GUI_BYTE)

RDF_LIBS=ocamlrdf.cmxa
RDF_LIBS_BYTE=ocamlrdf.cma

#JSON_LIBS= easy_format.cmx biniou.cmxa yojson.cmx
#JSON_LIBS_BYTE= easy_format.cmo biniou.cma yojson.cmo

# Compilation
#############

# Base library

LIB_BASE=base/genet_base.cmxa
LIB_BASE_BYTE=$(LIB_BASE:.cmxa=.cma)
LIB_BASE_CMI=$(LIB_BASE:.cmxa=.cmi)
LIB_BASE_A=$(LIB_BASE:.cmxa=.a)

BASE_CMXFILES=base/version.cmx \
	base/install.cmx \
	base/sset.cmx \
	base/smap.cmx \
	base/find.cmx \
	base/misc.cmx \
	base/fname.cmx \
	base/reporter.cmx \
	base/config.cmx \
	base/checks.cmx \
	base/loc.cmx \
	base/cmdline.cmx \
	base/graph.cmx \
	base/tokens.cmx \
	base/lexer.cmx

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

LIB_BASE_GUI=base/genet_basegui.cmxa
LIB_BASE_GUI_BYTE=$(LIB_BASE_GUI:.cmxa=.cma)
LIB_BASE_GUI_CMI=$(LIB_BASE_GUI:.cmxa=.cmi)
LIB_BASE_GUI_A=$(LIB_BASE_GUI:.cmxa=.a)

BASE_GUI_LIBS=$(LIB_BASE_GUI) $(LIB_BASE_GUI_BYTE) $(LIB_BASE_GUI_CMI) $(LIB_BASE_GUI_A)

# Grdf library

LIB_GRDF=grdf/genet_grdf.cmxa
LIB_GRDF_BYTE=$(LIB_GRDF:.cmxa=.cma)
LIB_GRDF_CMI=$(LIB_GRDF:.cmxa=.cmi)
LIB_GRDF_A=$(LIB_GRDF:.cmxa=.a)

GRDF_CMXFILES=grdf/grdf_constant.cmx \
	grdf/grdf_types.cmx \
	grdf/grdfs.cmx \
	grdf/grdf_branch.cmx \
	grdf/grdf_parser.cmx \
	grdf/grdf_ftype.cmx \
	grdf/grdf_version.cmx \
	grdf/grdf_port.cmx \
	grdf/grdf_intf.cmx \
	grdf/grdf_diff.cmx \
	grdf/grdf_tool.cmx \
	grdf/grdf_init.cmx \
	grdf/grdf_dot.cmx

GRDF_CMOFILES=$(GRDF_CMXFILES:.cmx=.cmo)
GRDF_CMIFILES=$(GRDF_CMXFILES:.cmx=.cmi)

GRDF_LIBS=$(LIB_GRDF) $(LIB_GRDF_BYTE) $(LIB_GRDF_CMI) $(LIB_GRDF_A)

GRDF_GUI_CMXFILES=gui/toolgui.cmx
GRDF_GUI_CMIFILES=$(GRDF_GUI_CMXFILES:.cmx=.cmi)
GRDF_GUI_CMOFILES=$(GRDF_GUI_CMXFILES:.cmx=.cmo)

LIB_GRDF_GUI=grdf/genet_grdfgui.cmxa
LIB_GRDF_GUI_BYTE=$(LIB_GRDF_GUI:.cmxa=.cma)
LIB_GRDF_GUI_CMI=$(LIB_GRDF_GUI:.cmxa=.cmi)
LIB_GRDF_GUI_A=$(LIB_GRDF_GUI:.cmxa=.a)

GRDF_GUI_LIBS=$(LIB_GRDF_GUI) $(LIB_GRDF_GUI_BYTE) $(LIB_GRDF_GUI_CMI) $(LIB_GRDF_GUI_A)

# Indata library
LIB_IND=indata/genet_indata.cmxa
LIB_IND_BYTE=$(LIB_IND:.cmxa=.cma)
LIB_IND_CMI=$(LIB_IND:.cmxa=.cmi)
LIB_IND_A=$(LIB_IND:.cmxa=.a)

IND_CMXFILES=indata/ind_types.cmx \
	indata/ind_io.cmx

IND_CMOFILES=$(IND_CMXFILES:.cmx=.cmo)
IND_CMIFILES=$(IND_CMXFILES:.cmx=.cmi)

IND_LIBS=$(LIB_IND) $(LIB_IND_BYTE) $(LIB_IND_CMI) $(LIB_IND_A)

IND_GUI_CMXFILES=
IND_GUI_CMIFILES=$(IND_GUI_CMXFILES:.cmx=.cmi)
IND_GUI_CMOFILES=$(IND_GUI_CMXFILES:.cmx=.cmo)

LIB_IND_GUI=indata/genet_indatagui.cmxa
LIB_IND_GUI_BYTE=$(LIB_IND_GUI:.cmxa=.cma)
LIB_IND_GUI_CMI=$(LIB_IND_GUI:.cmxa=.cmi)
LIB_IND_GUI_A=$(LIB_IND_GUI:.cmxa=.a)

IND_GUI_LIBS=$(LIB_IND_GUI) $(LIB_IND_GUI_BYTE) $(LIB_IND_GUI_CMI) $(LIB_IND_GUI_A)

# Chains library
LIB_CHN=chains/genet_chains.cmxa
LIB_CHN_BYTE=$(LIB_CHN:.cmxa=.cma)
LIB_CHN_CMI=$(LIB_CHN:.cmxa=.cmi)
LIB_CHN_A=$(LIB_CHN:.cmxa=.a)

CHN_CMXFILES=chains/chn_types.cmx \
	chains/chn_ast.cmx \
	chains/chn_parser.cmx \
	chains/chn_io.cmx \
	chains/chn_flat.cmx \
	chains/chn_run.cmx \
	chains/chn_inst.cmx \
	chains/chn_inst_query.cmx \
	chains/chn_exec.cmx \
	chains/chn_lookup.cmx \
	chains/chn_diff.cmx

CHN_CMOFILES=$(CHN_CMXFILES:.cmx=.cmo)
CHN_CMIFILES=$(CHN_CMXFILES:.cmx=.cmi)

CHN_LIBS=$(LIB_CHN) $(LIB_CHN_BYTE) $(LIB_CHN_CMI) $(LIB_CHN_A)

CHN_GUI_CMXFILES= \
	gui/chngui.cmx \
	gui/instgui.cmx
CHN_GUI_CMIFILES=$(CHN_GUI_CMXFILES:.cmx=.cmi)
CHN_GUI_CMOFILES=$(CHN_GUI_CMXFILES:.cmx=.cmo)

LIB_CHN_GUI=gui/genet_chainsgui.cmxa
LIB_CHN_GUI_BYTE=$(LIB_CHN_GUI:.cmxa=.cma)
LIB_CHN_GUI_CMI=$(LIB_CHN_GUI:.cmxa=.cmi)
LIB_CHN_GUI_A=$(LIB_CHN_GUI:.cmxa=.a)

CHN_GUI_LIBS=$(LIB_CHN_GUI) $(LIB_CHN_GUI_BYTE) $(LIB_CHN_GUI_CMI) $(LIB_CHN_GUI_A)

# Rest library

LIB_REST=rest/genet_rest.cmxa
LIB_REST_BYTE=$(LIB_REST:.cmxa=.cma)
LIB_REST_CMI=$(LIB_REST:.cmxa=.cmi)
LIB_REST_A=$(LIB_REST:.cmxa=.a)

REST_CMXFILES= \
	rest/rest_types.cmx \
	rest/rest_xpage.cmx \
	rest/rest_xhtml.cmx \
	rest/rest_query.cmx

REST_CMOFILES=$(REST_CMXFILES:.cmx=.cmo)
REST_CMIFILES=$(REST_CMXFILES:.cmx=.cmi)

REST_LIBS=$(LIB_REST) $(LIB_REST_BYTE) $(LIB_REST_CMI) $(LIB_REST_A)

# Genet files
GENET_CMDCMXFILES= \
	driver/options.cmx \
	driver/main_cmd.cmx \
	driver/main_diff.cmx \
	driver/main_query.cmx \
	driver/main_chain.cmx \
	driver/main_exec.cmx \
	driver/main_init.cmx \
	driver/main_rdf.cmx \
	driver/main_edit.cmx
GENET_CMXFILES= \
	$(GENET_CMDCMXFILES) \
	driver/main.cmx
GENET_CMOFILES=$(GENET_CMXFILES:.cmx=.cmo)
GENET_CMIFILES=$(GENET_CMXFILES:.cmx=.cmi)

GENET_CHEAT_CMXFILES= \
	$(GENET_CMDCMXFILES) \
	driver/main_cheat.cmx
GENET_CHEAT_CMOFILES=$(GENET_CHEAT_CMXFILES:.cmx=.cmo)
GENET_CHEAT_CMIFILES=$(GENET_CHEAT_CMXFILES:.cmx=.cmi)

# Genet-genrdfs files
GENRDFS_CMXFILES= driver/options.cmx \
	driver/main_genrdfs.cmx
GENRDFS_CMOFILES=$(GENRDFS_CMXFILES:.cmx=.cmo)
GENRDFS_CMIFILES=$(GENRDFS_CMXFILES:.cmx=.cmi)

# Genet-rest files
GENET_REST_CMXFILES=\
	driver/options.cmx \
	driver/main_rest.cmx
GENET_REST_CMOFILES=$(GENET_REST_CMXFILES:.cmx=.cmo)
GENET_REST_CMIFILES=$(GENET_REST_CMXFILES:.cmx=.cmi)

# Genet-gui files
GENET_GUI_CMXFILES=\
	gui/gui_install.cmx \
	gui/gui_base.cmx \
	gui/gui_main.cmx \
	driver/options.cmx \
	driver/main_gui.cmx
GENET_GUI_CMOFILES=$(GENET_GUI_CMXFILES:.cmx=.cmo)
GENET_GUI_CMIFILES=$(GENET_GUI_CMXFILES:.cmx=.cmi)

opt: libs tools
all: opt byte
byte: libs_byte tools_byte
libs: $(LIBS) $(LIBS_GUI)
libs_byte: $(LIB_BYTE) $(LIBS_GUI_BYTE)

tools: libs $(TOOLS)
tools_byte: libs_byte $(TOOLS_BYTE)

$(GENET): $(LIB_BASE)	$(LIB_GRDF) $(LIB_IND) $(LIB_CHN) $(GENET_CMXFILES)
	$(OCAMLFIND) ocamlopt -package $(BASE_PACKAGES) -linkpkg \
	$(OCAMLCFLAGS)-o $@ \
	$(LINKFLAGS) $(SYSTEM_LIBS) $^

$(GENET_BYTE): $(LIB_BASE_BYTE)	$(LIB_GRDF_BYTE) $(LIB_IND_BYTE) $(LIB_CHN_BYTE) $(GENET_CMOFILES)
	$(OCAMLFIND) ocamlc -package $(BASE_PACKAGES) -linkpkg \
	$(OCAMLCFLAGS) -o $@ \
	$(LINKFLAGS) $(SYSTEM_LIBS_BYTE) $^

$(GENET_CHEAT): $(LIB_BASE)	$(LIB_GRDF) $(LIB_IND) $(LIB_CHN) $(GENET_CHEAT_CMXFILES)
	$(OCAMLFIND) ocamlopt -package $(BASE_PACKAGES) -linkpkg \
	$(OCAMLCFLAGS)-o $@ \
	$(LINKFLAGS) $(SYSTEM_LIBS) $^

$(GENET_CHEAT_BYTE): $(LIB_BASE_BYTE)	$(LIB_GRDF_BYTE) $(LIB_IND_BYTE) $(LIB_CHN_BYTE) $(GENET_CHEAT_CMOFILES)
	$(OCAMLFIND) ocamlc -package $(BASE_PACKAGES) -linkpkg \
	$(OCAMLCFLAGS) -o $@ \
	$(LINKFLAGS) $(SYSTEM_LIBS_BYTE) $^

$(GENRDFS): $(LIB_BASE)	$(LIB_GRDF) $(GENRDFS_CMXFILES)
	$(OCAMLFIND) ocamlopt -package $(BASE_PACKAGES) -linkpkg \
	$(OCAMLCFLAGS) -o $@ $(LINKFLAGS) $(SYSTEM_LIBS) $^

$(GENRDFS_BYTE): $(LIB_BASE_BYTE)	$(LIB_GRDF_BYTE) $(GENRDFS_CMOFILES)
	$(OCAMLFIND) ocamlc -package $(BASE_PACKAGES) -linkpkg \
	$(OCAMLCFLAGS) -o $@ $(LINKFLAGS) $(SYSTEM_LIBS_BYTE) $^

$(GENET_REST): $(LIB_BASE)	$(LIB_GRDF) $(LIB_IND) $(LIB_CHN) $(LIB_REST) $(GENET_REST_CMXFILES)
	$(OCAMLFIND) ocamlopt -package $(BASE_PACKAGES),$(REST_PACKAGES),$(NET_PACKAGES) -linkpkg \
	$(OCAMLCFLAGS) -o $@ $(LINKFLAGS) $(SYSTEM_LIBS) $^

$(GENET_REST_BYTE): $(LIB_BASE_BYTE)	$(LIB_GRDF_BYTE) $(LIB_IND_BYTE) $(LIB_CHN_BYTE) $(LIB_REST_BYTE) $(GENET_REST_CMOFILES)
	$(OCAMLFIND) ocamlc -package $(BASE_PACKAGES),$(REST_PACKAGES),$(NET_PACKAGES) -linkpkg \
	$(OCAMLCFLAGS) -o $@ $(LINKFLAGS) $(SYSTEM_LIBS_BYTE) $^

$(GENET_GUI): $(GENET_GUI_CMIFILES) $(LIB_BASE)	$(LIB_GRDF) $(LIB_IND) $(LIB_CHN) $(LIB_REST) $(LIBS_GUI) $(GENET_GUI_CMXFILES)
	$(OCAMLFIND) ocamlopt -verbose -package $(BASE_PACKAGES),$(GUI_PACKAGES) -linkpkg \
	$(OCAMLCFLAGS) -o $@ $(LINKFLAGS) $(SYSTEM_LIBS) \
	$(LIB_BASE)	$(LIB_GRDF) $(LIB_IND) $(LIB_CHN) $(LIB_REST) $(LIBS_GUI) $(GENET_GUI_CMXFILES)

$(GENET_GUI_BYTE): $(GENET_GUI_CMIFILES) $(LIB_BASE_BYTE)	$(LIB_GRDF_BYTE) \
						$(LIB_IND_BYTE) $(LIB_CHN_BYTE) $(LIB_REST_BYTE) $(LIBS_GUI_BYTE)	$(GENET_GUI_CMOFILES)
	$(OCAMLFIND) ocamlc -package $(BASE_PACKAGES),$(GUI_PACKAGES) -linkpkg \
	$(OCAMLCFLAGS) -o $@ $(LINKFLAGS) $(SYSTEM_LIBS_BYTE) \
	$(LIB_BASE_BYTE)	$(LIB_GRDF_BYTE) $(LIB_IND_BYTE) $(LIB_CHN_BYTE) $(LIB_REST_BYTE) $(LIBS_GUI_BYTE)	$(GENET_GUI_CMOFILES)


$(LIB_BASE): $(BASE_CMIFILES) $(BASE_CMXFILES)
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -a -o $@ $(LINKFLAGS) $(BASE_CMXFILES)
$(LIB_BASE_BYTE): $(BASE_CMIFILES) $(BASE_CMOFILES)
	$(OCAMLFIND) ocamlc $(OF_FLAGS) -a -o $@ $(LINKFLAGS) $(BASE_CMOFILES)

$(LIB_BASE_GUI): $(BASE_GUI_CMIFILES) $(BASE_GUI_CMXFILES)
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -a -o $@ $(LINKFLAGS) $(BASE_GUI_CMXFILES)
$(LIB_BASE_GUI_BYTE): $(BASE_GUI_CMIFILES) $(BASE_GUI_CMOFILES)
	$(OCAMLFIND) ocamlc $(OF_FLAGS) -a -o $@ $(LINKFLAGS) $(BASE_GUI_CMOFILES)

$(LIB_GRDF): $(GRDF_CMIFILES) $(GRDF_CMXFILES)
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -a -o $@ $(LINKFLAGS) $(GRDF_CMXFILES)
$(LIB_GRDF_BYTE): $(GRDF_CMIFILES) $(GRDF_CMOFILES)
	$(OCAMLFIND) ocamlc $(OF_FLAGS) -a -o $@ $(LINKFLAGS) $(GRDF_CMOFILES)

$(LIB_GRDF_GUI): $(GRDF_GUI_CMIFILES) $(GRDF_GUI_CMXFILES)
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -a -o $@ $(LINKFLAGS) $(GRDF_GUI_CMXFILES)
$(LIB_GRDF_GUI_BYTE): $(GRDF_GUI_CMIFILES) $(GRDF_GUI_CMOFILES)
	$(OCAMLFIND) ocamlc $(OF_FLAGS) -a -o $@ $(LINKFLAGS) $(GRDF_GUI_CMOFILES)

$(LIB_IND): $(IND_CMIFILES) $(IND_CMXFILES)
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -a -o $@ $(LINKFLAGS) $(IND_CMXFILES)
$(LIB_IND_BYTE): $(IND_CMIFILES) $(IND_CMOFILES)
	$(OCAMLFIND) ocamlc $(OF_FLAGS) -a -o $@ $(LINKFLAGS) $(IND_CMOFILES)

$(LIB_IND_GUI): $(IND_GUI_CMIFILES) $(IND_GUI_CMXFILES)
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -a -o $@ $(LINKFLAGS) $(IND_GUI_CMXFILES)
$(LIB_IND_GUI_BYTE): $(IND_GUI_CMIFILES) $(IND_GUI_CMOFILES)
	$(OCAMLFIND) ocamlc $(OF_FLAGS) -a -o $@ $(LINKFLAGS) $(IND_GUI_CMOFILES)

$(LIB_CHN): $(CHN_CMIFILES) $(CHN_CMXFILES)
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -a -o $@ $(LINKFLAGS) $(CHN_CMXFILES)
$(LIB_CHN_BYTE): $(CHN_CMIFILES) $(CHN_CMOFILES)
	$(OCAMLFIND) ocamlc $(OF_FLAGS) -a -o $@ $(LINKFLAGS) $(CHN_CMOFILES)

$(LIB_CHN_GUI): $(CHN_GUI_CMIFILES) $(CHN_GUI_CMXFILES)
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -a -o $@ $(LINKFLAGS) $(CHN_GUI_CMXFILES)
$(LIB_CHN_GUI_BYTE): $(CHN_GUI_CMIFILES) $(CHN_GUI_CMOFILES)
	$(OCAMLFIND) ocamlc $(OF_FLAGS) -a -o $@ $(LINKFLAGS) $(CHN_GUI_CMOFILES)


$(LIB_REST): $(REST_CMIFILES) $(REST_CMXFILES)
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -a -o $@ $(LINKFLAGS) $(REST_CMXFILES)
$(LIB_REST_BYTE): $(REST_CMIFILES) $(REST_CMOFILES)
	$(OCAMLFIND) ocamlc $(OF_FLAGS) -a -o $@ $(LINKFLAGS) $(REST_CMOFILES)

KO='\033[1;31m'"*KO\033[1m\033[0m"
OK='\033[1;32m'"OK\033[1m\033[0m"


# Documentation :
#################
dump.odoc: base/*.ml base/*.mli \
	grdf/*.ml grdf/*.mli \
	indata/*.ml indata/*.mli \
	chains/*.ml chains/*.mli \
	rest/*.ml rest/*.mli \
	gui/*.ml gui/*.mli \
	driver/*.ml driver/*.mli
	OCAMLFIND_COMMANDS="ocamldoc=ocamldoc.opt" \
	$(OCAMLFIND) ocamldoc $(OF_FLAGS) -predicates native \
	-v \
	-m A \
	-keep-code $(COMPFLAGS) -rectypes -sort -dump $@ \
	`ls $^ | grep  -v -e "_parser.ml$$" `

doc: all dump.odoc
	$(MKDIR) ocamldoc
	$(CP) ../doc/ocamldoc_style.css ocamldoc/style.css
	OCAMLFIND_COMMANDS="ocamldoc=ocamldoc.opt" \
	$(OCAMLFIND) ocamldoc $(OF_FLAGS) -predicates native \
	$(COMPFLAGS) -v -d ocamldoc -load dump.odoc \
	-t "Genet library reference" -g odoc_depgraph.cmxs \
	-intro ../doc/ocamldoc_index.text -dot-reduce

dot: all dump.odoc

missingdocmods:
	@for i in `ls */*.ml */*.mli | cut -d'/' -f 2 | cut -d'.' -f 1 | sort -u`; do \
		grep -i $$i ../doc/ocamldoc_index.text > /dev/null 2>&1 || echo missing $$i; \
	done


# backup, clean and depend :
############################

GENERATED_FILES=base/lexer.ml base/lexer.mli base/tokens.ml base/tokens.mli

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
	(cd indata && $(RM) *~ \#*\# *.cmo *.cmi *.cmx *.a *.o *.cma *.cmxa *.annot *.odoc)
	(cd chains && $(RM) *~ \#*\# *.cmo *.cmi *.cmx *.a *.o *.cma *.cmxa *.annot *.odoc)
	(cd rest && $(RM) *~ \#*\# *.cmo *.cmi *.cmx *.a *.o *.cma *.cmxa *.annot *.odoc)
	(cd gui && $(RM) *~ \#*\# *.cmo *.cmi *.cmx *.a *.o *.cma *.cmxa *.annot *.odoc)

.depend depend:
	$(RM) .depend
	$(OCAMLDEP) $(MY_INCLUDES) \
	base/*.ml base/*.mli \
	driver/*.ml driver/*.mli \
	grdf/*.ml grdf/*.mli \
	indata/*.ml indata/*.mli \
	chains/*.ml chains/*.mli \
	rest/*.ml rest/*.mli \
	gui/*.ml gui/*.mli \
	 > .depend

alldepend: $(GENERATED_FILES)
	$(MAKE) depend
dummy:

include .depend

#################
# Installation
#################
install: install-web-files install-libs install-bin

install-bin:
	$(MKDIR) $(BINDIR) $(GLADEDIR)
	$(CP) $(TOOLS) $(TOOLS_BYTE) $(BINDIR)/
	$(CP) gui/genet.glade $(GLADEDIR)/

install-libs:
	$(OCAMLFIND) install $(PACKAGE) META ../LICENSE \
	$(LIBS) $(LIBS_BYTE) $(LIBS:.cmxa=.a) \
	$(BASE_CMIFILES) $(GRDF_CMIFILES) $(IND_CMIFILES) $(CHN_CMIFILES) $(REST_CMIFILES)

install-web-files:
	$(MKDIR) $(SHAREDIR)/genet-web
	$(CP) -r tmpl $(SHAREDIR)/genet-web/
	$(CP) $(ROOT)/website/genet-logo.svg $(ROOT)/website/star.svg $(SHAREDIR)/genet-web/
	$(CP) $(ROOT)/website/style.css $(SHAREDIR)/genet-web/style.css

uninstall: uninstall-web-files uninstall-libs uninstall-bin

uninstall-bin:
	for i in $(TOOLS) $(TOOLS_BYTE); do $(RM) $(BINDIR)/$$i; done
	$(RM) $(GLADEDIR)/genet.glade

uninstall-libs:
	$(OCAMLFIND) remove genet

uninstall-web-files:
	$(RM) -fr $(SHAREDIR)/genet-web

###########################
# additional dependencies
###########################
chains/chn_parser.ml chains/chn_parser.mli: base/tokens.mly grdf/grdf_parser.mly chains/chn_parser.mly
	$(MENHIR) --explain --base chains/chn_parser --external-tokens Tokens $^

base/lexer.ml base/lexer.mli: base/lexer.mll
	$(OCAMLLEX) $<
	$(OCAMLFIND) ocamlc $(OF_FLAGS) $(OCAMLCFLAGS) -rectypes $(INCLUDES) -i base/lexer.ml > base/lexer.mli

grdf/grdf_parser.ml grdf/grdf_parser.mli: base/tokens.mly grdf/grdf_parser.mly
	$(MENHIR) --base grdf/grdf_parser --external-tokens Tokens $^

base/tokens.ml base/tokens.mli: base/tokens.mly
	$(MENHIR) $<

gui/gui_base.ml: gui/genet.glade
	$(LABLGLADECC) -hide-default $< > $@
