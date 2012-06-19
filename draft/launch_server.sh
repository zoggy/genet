export LD_LIBRARY_PATH=/opt/raptor/lib:/opt/librdf/lib:$LD_LIBRARY_PATH
( \
rm -fr genet ; \
mkdir genet ; \
cd genet ; \
~/devel/genet/src/genet init-dir . ; \
cp ~/devel/genet/src/config.txt . ; \
mkdir in/data/test1 ;
cp ~/devel/genet/draft/spec.in in/data/test1/ ; \
cp ~/devel/genet/draft/*.gnt in/chains/ ; \
(cd in/ ; git init . ; cd chains ; git add *.gnt ; git commit -am"test" ); \
~/devel/genet/src/genet-rest )
