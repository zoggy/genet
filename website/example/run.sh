#!/bin/sh

export PATH=/home/guesdon/devel/genet/src:$PATH

mkdir /tmp/tools
for i in */*.x; do echo cp -f `dirname $i`/`basename $i .x` /tmp/tools/`dirname $i`; done

#id=initdir
genet init-dir /tmp/genet-example
cp config.txt /tmp/genet-example/


#id=initdb
genet --config /tmp/genet-example/config.txt init-db

#id=changedir
cd /tmp/genet-example

#id=addfiletypes
genet add filetype "text" txt "Source text file"
genet add filetype "words" txt "words, one per line"
genet add filetype "average" avg "computed average length of words"

#id=showfiletypes
genet-query --filetypes



#id=addsplittexttool
genet add tool split-text
 # http://localhost:8082/tools/split-text

#id=addsplittextbranch
genet add branch http://localhost:8082/tools/split-text 0.x
 # http://localhost:8082/tools/split-text/branches/0.x

#id=addsplittextversion
genet add version http://localhost:8082/tools/split-text \
   http://localhost:8082/tools/split-text/branches/0.x \
   0.1
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
INTF=`genet add interface -p "/tmp/tools/words-%v" ${BRANCH} unique-words`
  # http://localhost:8082/tools/words/interfaces/unique-words
genet add port ${INTF} "in" "words"
genet add port ${INTF} "out" "words"

#id=addaverage
TOOL=`genet add tool average` # http://localhost:8082/tools/average
BRANCH=`genet add branch ${TOOL} 0.x` # http://localhost:8082/tools/average/branches/0.x
genet add version ${TOOL} ${BRANCH} 0.1
  # http://localhost:8082/tools/average/versions/0.1
INTF=`genet add interface -p "/tmp/tools/average-%v" ${BRANCH} line-length`
  # http://localhost:8082/tools/average/interfaces/line-length
genet add port ${INTF} "in" "words"
genet add port ${INTF} "out" "average"

#id=showinterfaces
genet-query --interfaces
 # http://localhost:8082/tools/split-text/interfaces/split-in-words : text -> words</sh>

genet add version http://localhost:8082/tools/words \
  http://localhost:8082/tools/words/branches/0.x 0.2
  # http://localhost:8082/tools/words/versions/0.2

genet add version http://localhost:8082/tools/average \
  http://localhost:8082/tools/average/branches/0.x 0.2
  # http://localhost:8082/tools/average/versions/0.2
