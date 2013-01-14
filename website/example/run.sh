#!/bin/sh

export PATH=/home/guesdon/devel/genet/src:$PATH
#id=initdir
genet init-dir /tmp/genet-example
cp config.txt /tmp/genet-example/


#id=initdb
genet --config /tmp/genet-example/config.txt init-db

#id=changdir
cd /tmp/genet-example

#id=addfiletypes
genet add filetype "text" txt "Source text file"
genet add filetype "words" txt "words, one per line"
genet add filetype "average" avg "computed average length of words"
