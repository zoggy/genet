cd /tmp/genet/
(cd ./in && echo >> chains/dummy.gnt && git commit -am"touch")
echo "flattening Why3.dummy"
~/devel/genet/src/genet-chain --dot flat.dot -f Why3.dummy && dot -Tsvg -o flat.svg flat.dot && dot -Tpng -o flat.png flat.dot
