cd /tmp/genet/
(cd ./in && echo >> chains/dummy.gnt && git commit -am"touch")
echo "flattening dummy"
~/devel/genet/src/genet-chain --dot flat.dot -f Dummy.dummy && dot -Tsvg -o flat.svg flat.dot && dot -Tpng -o flat.png flat.dot
echo "flattening dummy_r"
~/devel/genet/src/genet-chain --dot flat.dot -f Dummy.dummy_r && dot -Tsvg -o flat.svg flat.dot && dot -Tpng -o flat.png flat.dot
