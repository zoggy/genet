#!/bin/sh
DIR=`dirname $0`
${DIR}/alt-ergo $1 > $2 2>&1
exit 1
