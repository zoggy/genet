cd /tmp/genet/
(cd ./in && echo >> chains/why3.gnt && git commit -am"touch")
~/devel/genet/src/genet-chain --dot flat.dot -f Why3.prove_with_ae && dot -Tsvg -o flat.svg flat.dot && dot -Tpng -o flat.png flat.dot
