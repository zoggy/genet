#!/bin/sh

export PATH=/home/guesdon/devel/genet/src:$PATH


#id=addsplittextversion
genet add version http://localhost:8082/tools/split-text/branches/0.x 0.2
  # http://localhost:8082/tools/split-text/versions/0.2

#id=addsplittextintf
INTF=genet add interface -p "/tmp/tools/split-text-%v -p" \
  http://localhost:8082/tools/split-text/versions/0.2 split-in-pars
  # http://localhost:8082/tools/split-text/interfaces/split-in-pars

#id=addsplittextin
genet add port ${INTF} "in" "text"
  # http://localhost:8082/tools/split-text/interfaces/split-in-pars/in/1

#id=addsplittextout
genet add port ${INTF} "out" "text set"
  # http://localhost:8082/tools/split-text/interfaces/split-in-pars/out/1

#id=addnotintfsplittext
genet add no-interface http://localhost:8082/tools/split-text/versions/0.1 ${INTF}

#id=addaverageversion
genet add version http://localhost:8082/tools/average/branches/0.x  0.3
  # http://localhost:8082/tools/average/versions/0.3

#id=addaverage-c
INTF=`genet add interface -p "/tmp/tools/average-%v -c" \
  http://localhost:8082/tools/average/branches/0.x text-length`
  # http://localhost:8082/tools/average/interfaces/text-length
genet add port ${INTF} "in" "text"
genet add port ${INTF} "out" "number"

#id=addnointfaverage-c
genet add no-interface http://localhost:8082/tools/average/versions/0.1 ${INTF}
genet add no-interface http://localhost:8082/tools/average/versions/0.2 ${INTF}

#id=addaverage-n
INTF=`genet add interface -p "/tmp/tools/average-%v -n" \
  http://localhost:8082/tools/average/branches/0.x average-of-set`
  # http://localhost:8082/tools/average/interfaces/average-of-set
genet add port ${INTF} "in" "number set"
genet add port ${INTF} "out" "number"

#id=addnointfaverage-n
genet add no-interface http://localhost:8082/tools/average/versions/0.1 ${INTF}
genet add no-interface http://localhost:8082/tools/average/versions/0.2 ${INTF}
