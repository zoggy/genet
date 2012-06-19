#!/bin/sh
ROOT=~/devel/genet
DIR=/tmp/genet
rm -fr ${DIR}
mkdir ${DIR}
(cd ${DIR} && ${ROOT}/src/genet init-dir . ; cp ${ROOT}/src/config.txt .)
cp -R test1 ${DIR}/in/data/
cp chains/*.gnt ${DIR}/in/chains/
(cd ${DIR}/in/ ; \
  git init . ; (cd chains ; git add *.gnt ; git commit -am"test" ) ; \
  for i in "data/test1/same_fringe.mlw" ; do touch $i; git add $i; git commit -am"add" ; done; \
)


