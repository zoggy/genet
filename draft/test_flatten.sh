cd /tmp/genet/
(cd ./in && echo >> chains/chain1.gnt && git commit -am"touch")
~/devel/genet/src/genet-chain --dot flat.dot -f Chain1.test1 && dot -Tsvg -o flat.svg flat.dot && dot -Tpng -o flat.png flat.dot
