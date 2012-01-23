
# check for one particular file of the sources
AC_INIT(master.Makefile.in)

REVISION=`git log | head -n 1 | cut -d' ' -f 2`
VERSION=0.1+$REVISION

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
OCAMLVERSION="3.11.0"
MENHIR="menhir"
HEVEA="hevea"
HACHA="hacha"
BIBTEX="bibtex"
LABLGLADECC="lablgladecc2"
' > ./ocaml_config.sh
else
  ocaml unix.cma str.cma utils/checkocaml.ml ocaml_config.sh || exit 1
  fi;


. ./ocaml_config.sh

echo "ocaml library path is $OCAMLLIB"

# set the default prefix to the ocaml directory
AC_PREFIX_PROGRAM(`basename $OCAML`)
exec_prefix=$prefix


if test "$bindir" = "\${exec_prefix}/bin" ; then
        bindir=$prefix/bin
fi

if test "$libdir" = "\${exec_prefix}/lib" ; then
        libdir=$OCAMLLIB/cameleon2
fi

if test "$mandir" = "\${prefix}/man" ; then
        mandir=$prefix/man
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

PIXMAPSDIR=$datadir/pixmaps
TEMPLATESDIR=$datadir/templates
PLUGINSDIR=$datadir/plugins
GLADEDIR=$datadir/glade
DTDDIR=$datadir/dtds
LANGUAGESSPECSDIR=$datadir/language-specs

############################
# Flags
############################
COMPFLAGS=

MENHIR="$MENHIR --table"
MENHIR_INCLUDES=`$MENHIR --suggest-comp-flags`
MENHIR_LINK_FLAGS=`$MENHIR --suggest-link-flags-opt`
MENHIR_LINK_FLAGS_BYTE=`$MENHIR --suggest-link-flags-byte`
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
AC_SUBST(LABLGLADECC)
AC_SUBST(OCAMLDOC)
AC_SUBST(OCAMLDOC_OPT)
AC_SUBST(OCAMLDOC_PLUGINSDIR)
AC_SUBST(MENHIR)
AC_SUBST(MENHIR_INCLUDES)
AC_SUBST(MENHIR_LINK_FLAGS)
AC_SUBST(MENHIR_LINK_FLAGS_BYTE)
AC_SUBST(HEVEA)
AC_SUBST(HACHA)
AC_SUBST(BIBTEX)
AC_SUBST(EXEC_PREFIX)
AC_SUBST(INSTALLBIN)
AC_SUBST(TEMPLATESDIR)
AC_SUBST(PLUGINSDIR)
AC_SUBST(PIXMAPSDIR)
AC_SUBST(GLADEDIR)
AC_SUBST(DTDDIR)
AC_SUBST(LANGUAGESSPECSDIR)
AC_SUBST(ROOT)

AC_SUBST(GTK_INCLUDES)
AC_SUBST(LABLGTK2_INCLUDES)
AC_SUBST(LABLGTKEXTRAS_INCLUDES)
AC_SUBST(BOXES_INCLUDES)
AC_SUBST(LABLGTK2DIR)

AC_SUBST(DEBUG_LEVEL)

# Finally create the master.Makefile.in
ACFILES="master.Makefile"

AC_OUTPUT($ACFILES)
chmod a-w master.Makefile

# List values
echo "
### Results of configuration ###

Binaries will be installed in $bindir
Libraries will be installed in $libdir
Pixmaps will be installed in $PIXMAPSDIR
Glade templates will be installed in $GLADEDIR

Debug level is set to $DEBUG_LEVEL

Version is $VERSION"
