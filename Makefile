#################################################################################
#                Genet                                                          #
#                                                                               #
#    Copyright (C) 2012 Institut National de Recherche en Informatique          #
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

include master.Makefile

SCRIPTS=

IMAGES=

# Compilation
#############

work: utils src

all: work srcdoc doc

src: dummy
	cd src && $(MAKE) all

utils: dummy
	cd utils && $(MAKE) all

re : depend clean all

help:
	@echo "work:      utils src"
	@echo "all:       work srcdoc doc"
	@echo "src:       src/all"
	@echo "utils:     utils/all"
	@echo "re:        depend clean all"

# Documentation :
#################
srcdoc: dummy
	cd src && $(MAKE) doc

doc: dummy
	cd doc && $(MAKE) all

# myself

master.Makefile: master.Makefile.in config.status
	./config.status

config.status: configure master.Makefile.in src/base/version.ml.in
	./config.status --recheck

configure: configure.ac
	autoconf

# headers :
###########
HEADFILES= configure.ac configure \
	master.Makefile.in Makefile \
	src/*/*.ml src/*/*.mli src/*/*.mll src/*/*.mly src/*/*.in \
	src/Makefile doc/Makefile utils/Makefile utils/checkocaml.ml \
	draft/Makefile
headers: dummy
	echo $(HEADFILES)
	headache -h header.txt -c .headache_config `ls $(HEADFILES) `

noheaders: dummy
	headache -r -c .headache_config `ls $(HEADFILES) `

# backup, clean and depend :
############################

distclean: clean
	cd src && $(MAKE) distclean
	cd doc && $(MAKE) distclean
	cd utils && $(MAKE) distclean
	$(RM) autom4te.cache config.cache config.log config.status master.Makefile
	$(RM) config_check.log ocaml_config.sh

clean: dummy
	$(RM) *~ \#*\#
	cd src && $(MAKE) clean
	cd doc && $(MAKE) clean
	cd utils && $(MAKE) clean

depend: dummy
	cd src && $(MAKE) depend
alldepend: dummy
	cd src && $(MAKE) alldepend

dummy:

#################
# code count
#################
codecount:
	(cd src && wc -l `ls */*.ml */*.mli \
	| grep -v adeq/adeq_durations_parser.ml \
	| grep -v base/tokens.ml \
  | grep -v algo/algo_lang_parser.ml \
  | grep -v sdxparser/sdx_parser.ml \
  | grep -v arch/arch_lang_parser.ml \
  | grep -v base/lexer.ml \
  | grep -v sdxparser/sdx_lexer.ml ` \
  adeq/adeq_durations_parser.mly \
  base/tokens.mly \
  algo/algo_lang_parser.mly \
  sdxparser/sdx_parser.mly \
  arch/arch_lang_parser.mly \
  base/lexer.mll \
  sdxparser/sdx_lexer.mll \
  )


#################
# installation
#################

install: dummy installimages
	cd src && $(MAKE) install
#	$(MAKE) installspecsfiles
	$(MAKE) installdoc

installspecsfiles:
	$(MKDIR) $(LANGUAGESSPECSDIR)
	$(CP) utils/ocaml.lang $(LANGUAGESSPECSDIR)

installdoc:
	cd doc && $(MAKE) install

installimages: dummy $(IMAGES)
	$(MKDIR) $(PIXMAPSDIR)
	$(CP) $(IMAGES) $(PIXMAPSDIR)

installscripts: dummy
#	$(MKDIR) $(DIR_UTILS)
#	$(CP) $(SCRIPTS) $(DIR_UTILS)

###########################
# additional dependencies
###########################

# DO NOT DELETE
