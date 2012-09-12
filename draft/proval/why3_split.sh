#!/bin/sh
DIR=`dirname $0`
${DIR}/why3ml -P alt-ergo -a split_goal $1 -o $2

