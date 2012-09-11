cd /tmp/genet/
(cd ./in && echo >> chains/why3.gnt && git commit -am"touch" && git status)
echo "flattening dummy"
~/devel/genet/src/genet-chain --dot flat.dot -f Why3.dummy && dot -Tsvg -o flat.svg flat.dot && dot -Tpng -o flat.png flat.dot
echo "flattening dummy_r"
~/devel/genet/src/genet-chain --dot flat.dot -f Why3.dummy_r && dot -Tsvg -o flat.svg flat.dot && dot -Tpng -o flat.png flat.dot
