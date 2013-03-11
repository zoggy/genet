#!/bin/sh

export PATH=/home/guesdon/devel/genet/src:$PATH

#id=addtest2gnt
cp -f in/chains/test2.gnt /tmp/genet-example/in/chains/
(cd /tmp/genet-example/in/chains && git add test2.gnt && git commit -m"add new chain" test2.gnt)

cp -f spec.in2 /tmp/genet-example/in/data/test1/spec.in
(cd /tmp/genet-example/in/data/test1 && git commit -m"add new chain" spec.in)

cd /tmp/genet-example


#id=addsplittextversion
genet add version http://localhost:8082/tools/split-text/branches/0.x 0.2
  # http://localhost:8082/tools/split-text/versions/0.2

#id=addsplittextintf
INTF=`genet add interface -p "/tmp/tools/split-text-%v -p" \
  http://localhost:8082/tools/split-text/branches/0.x split-in-pars`
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

#id=flattenall
genet flatten-all
 #flatten Test2.par_avg_length: No type for port http://localhost:8082/flat-chains/Test2/par_avg_length/fee5-6166-604b-7ef0/split/in/1

#id=execall
genet exec --all
