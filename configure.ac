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

# check for one particular file of the sources
AC_INIT(master.Makefile.in)

REVISION=`(git log | head -n 1 | cut -d' ' -f 2) 2>/dev/null || echo`
if test "${REVISION}" != "" ; then ADD_REVISION=+${REVISION}; fi
VERSION=0.6${ADD_REVISION}

# The root directory where we will compile
ROOT=`pwd`

# Check for Ocaml compilers

WINDOWS=0
AC_ARG_ENABLE(windows,
[  to use when compiling under windows/cygwin
],
[ WINDOWS=1])

if test ${WINDOWS} = 1 ; then
  INCS='-I "C:\ocamlmgw\lib" -I "C:\ocamlmgw\lib\lablgtk2" -I "C:\ocamlmgw\lib\ocamldoc" -I "C:\ocamlmgw\lib\boxes" -ccopt "-LC:/opt/gtk/lib"';
  echo '
OCAMLDEP="ocamldep.opt"
OCAMLYACC="ocamlyacc"
OCAMLDOC="ocamldoc"
OCAMLLIB="/cygdrive/c/ocamlmgw/lib"
OCAMLDOC_OPT="ocamldoc.opt"
OCAMLMKTOP="ocamlmktop"
OCAMLPROF="ocamlprof"
OCAMLBIN="/cygdrive/c/ocamlmgw/bin"
OCAMLC="ocamlc.opt"
OCAMLDOC_PLUGINSDIR="/cygdrive/c/ocamlmgw/lib/ocamldoc/custom"
OCAMLFIND="ocamlfind"
CAMLP4="camlp4"
OCAMLLEX="ocamllex.opt"
OCAML="ocaml"
OCAMLOPT="ocamlopt.opt"
OCAMLMKLIB="ocamlmklib"
OCAMLVERSION="4.00.0"
MENHIR="menhir"
LABLGLADECC="lablgladecc2"
' > ./ocaml_config.sh
else
  ocaml unix.cma str.cma utils/checkocaml.ml ocaml_config.sh || exit 1
fi;


. ./ocaml_config.sh

echo "ocaml library path is $OCAMLLIB"

# set the default prefix to the ocaml directory
if test "$prefix" = "NONE" ; then
  prefix=`dirname $OCAMLBIN`
fi


if test "$bindir" = "\${exec_prefix}/bin" ; then
        bindir=$prefix/bin
fi

if test "$libdir" = "\${exec_prefix}/lib" ; then
        libdir=$OCAMLLIB/genet
fi

if test "$mandir" = "\${prefix}/man" ; then
        mandir=$prefix/man
fi

if test "$datadir" = "\${datarootdir}" ; then
        datadir=${prefix}/share
fi
if test "$datadir" = "\${prefix}/share" ; then
        datadir=$prefix/share
fi

INSTALLDIR=$prefix
BINDIR=$bindir
SHAREDIR=$datadir
LIBDIR=$libdir
GLADEDIR=$datadir/glade

############################
# Flags
############################
COMPFLAGS=

MENHIR="$MENHIR --table"
##########################
# debugging
##########################

DEBUG_LEVEL=0
AC_ARG_WITH(debug-level,
[  --with-debug-level=n   Set debug level to n [[default=0]]
],
[ DEBUG_LEVEL="$withval"])

##############
# Windows shit
##############
if test $WINDOWS = 1 ; then
  OCAMLDOC="$OCAMLDOC $INCS";
  OCAMLDOC_OPT="$OCAMLDOC_OPT $INCS";
  OCAMLMKTOP="$OCAMLMKTOP $INCS";
  OCAMLC="$OCAMLC $INCS";
  OCAML="$OCAML $INCS";
  OCAMLOPT="$OCAMLOPT $INCS";
  OCAMLMKLIB="$OCAMLMKLIB $INCS";
fi

#############################
# substitutions to perform

AC_SUBST(VERSION)

AC_SUBST(OCAMLC)
AC_SUBST(OCAMLOPT)
AC_SUBST(OCAMLDEP)
AC_SUBST(OCAMLLEX)
AC_SUBST(OCAMLYACC)
AC_SUBST(OCAMLVERSION)
AC_SUBST(OCAMLLIB)
AC_SUBST(OCAMLMKTOP)
AC_SUBST(OCAMLMKLIB)
AC_SUBST(CAMMKTOP)
AC_SUBST(OCAMLBIN)
AC_SUBST(OCAMLFIND)
AC_SUBST(OCAMLDOC)
AC_SUBST(OCAMLDOC_OPT)
AC_SUBST(OCAMLDOC_PLUGINSDIR)
AC_SUBST(LABLGLADECC)
AC_SUBST(MENHIR)
AC_SUBST(EXEC_PREFIX)
AC_SUBST(INSTALLDIR)
AC_SUBST(BINDIR)
AC_SUBST(LIBDIR)
AC_SUBST(SHAREDIR)
AC_SUBST(GLADEDIR)
AC_SUBST(ROOT)
AC_SUBST(DEBUG_LEVEL)
AC_SUBST(RDF_MYSQL)
AC_SUBST(RDF_POSTGRESQL)
AC_SUBST(MT_FLAGS)

ACFILES="master.Makefile src/META src/base/version.ml src/base/install.ml src/gui/gui_install.ml"

AC_OUTPUT($ACFILES)
chmod a-w master.Makefile
chmod a-w src/META
chmod a-w src/base/version.ml
chmod a-w src/base/install.ml
chmod a-w src/gui/gui_install.ml

# List values
echo "
### Results of configuration ###

Binaries will be installed in ${BINDIR}
Shared files will be installed in ${SHAREDIR}
Glade file will be installed in ${GLADEDIR}

Debug level is set to $DEBUG_LEVEL

Version is $VERSION"
