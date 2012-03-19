export LD_LIBRARY_PATH=/opt/librdf/lib:$LD_LIBRARY_PATH
(rm -fr genet ; mkdir genet ; cd genet ; ~/devel/genet/src/genet init-dir . ; cp ~/devel/genet/src/config.txt . ; ~/devel/genet/src/genet-rest)
