cd /tmp/genet/
(cd ./in && echo >> chains/altergo.gnt && git commit -am"touch")
~/devel/genet/src/genet-chain --dot flat.dot -f Altergo.prove && dot -Tsvg -o flat.svg flat.dot && dot -Tpng -o flat.png flat.dot
