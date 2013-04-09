#!/bin/sh

export PATH=/home/guesdon/devel/genet/src:$PATH


#id=initdir
genet init-dir /tmp/genet-example
git init /tmp/genet-example/in

cp config.txt /tmp/genet-example/
cp -f in/chains/test.gnt /tmp/genet-example/in/chains/
cp -r in/data/test1 /tmp/genet-example/in/data/

#id=initdb
genet --config /tmp/genet-example/config.txt init-db

#id=changedir
cd /tmp/genet-example

#id=addfiletypes
genet add filetype "text" txt "Source text file"
genet add filetype "words" txt "Words, one per line"
genet add filetype "number" nb "A real number"

#id=showfiletypes
genet query filetypes

#id=addsplittexttool
genet add tool split-text
 # http://localhost:8082/tools/split-text

#id=addsplittextbranch
genet add branch http://localhost:8082/tools/split-text 0.x
 # http://localhost:8082/tools/split-text/branches/0.x

#id=addsplittextversion
genet add version http://localhost:8082/tools/split-text/branches/0.x 0.1
  # http://localhost:8082/tools/split-text/versions/0.1

#id=addsplittextintf
genet add interface -p "/tmp/tools/split-text-%v" \
  http://localhost:8082/tools/split-text split-in-words
  # http://localhost:8082/tools/split-text/interfaces/split-in-words

#id=addsplittextin
genet add port http://localhost:8082/tools/split-text/interfaces/split-in-words "in" "text"
  # http://localhost:8082/tools/split-text/interfaces/split-in-words/in/1

#id=addsplittextout
genet add port http://localhost:8082/tools/split-text/interfaces/split-in-words "out" "words"
  # http://localhost:8082/tools/split-text/interfaces/split-in-words/out/1

#id=addwords
TOOL=`genet add tool words` # http://localhost:8082/tools/words
BRANCH=`genet add branch ${TOOL} 0.x` # http://localhost:8082/tools/words/branches/0.x
VERSION=`genet add version ${BRANCH} 0.2`
  # http://localhost:8082/tools/words/versions/0.2
INTF=`genet add interface -p "/tmp/tools/words-%v" ${BRANCH} unique-words`
  # http://localhost:8082/tools/words/interfaces/unique-words
genet add port ${INTF} "in" "words"
genet add port ${INTF} "out" "words"

#id=addaverage
TOOL=`genet add tool average` # http://localhost:8082/tools/average
BRANCH=`genet add branch ${TOOL} 0.x` # http://localhost:8082/tools/average/branches/0.x
VERSION=`genet add version ${BRANCH} 0.1`
  # http://localhost:8082/tools/average/versions/0.1
INTF=`genet add interface -p "/tmp/tools/average-%v" ${BRANCH} line-length`
  # http://localhost:8082/tools/average/interfaces/line-length
genet add port ${INTF} "in" "words"
genet add port ${INTF} "out" "number"

#id=showinterfaces
genet query interfaces
 # http://localhost:8082/tools/split-text/interfaces/split-in-words : text -> words

#id=gitaddchain
(cd in/chains && \
  git add test.gnt && \
  git commit -am"add test chain")

#id=testchain
genet chain test in/chains/test.gnt

#id=gitaddinput
(cd in/data && \
  git add test1 && \
  git commit -am"add input test1")

#id=flattenchain
genet flatten Test.words_avg_length
 # Test.words_avg_length => http://localhost:8082/flat-chains/Test/words_avg_length/...

#id=execchain
genet exec test1

#id=addnewwords
genet add version http://localhost:8082/tools/words/branches/0.x 0.4
  # http://localhost:8082/tools/words/versions/0.4

genet exec test1

#id=addnewaverage
genet add version http://localhost:8082/tools/average/branches/0.x 0.2
  # http://localhost:8082/tools/average/versions/0.2

#id=setactive
genet set active http://localhost:8082/tools/words/versions/0.2 false

genet exec test1
