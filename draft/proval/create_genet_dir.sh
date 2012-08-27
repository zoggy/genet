#!/bin/sh
ROOT=~/devel/genet
DIR=/tmp/genet
rm -fr ${DIR}
mkdir ${DIR}
(cd ${DIR} && ${ROOT}/src/genet init-dir . ; cp ${ROOT}/src/config.txt .)
cp -R test1 test2 ${DIR}/in/data/
cp chains/*.gnt ${DIR}/in/chains/
(cd ${DIR}/in/ ; \
  git init . ; (git add chains/*.gnt data/*/spec.in ; git commit -am"test" ) ; \
  for i in "data/test1/same_fringe.mlw data/test2/*why" ; do touch $i; git add $i; git commit -am"add" ; done; \
)


