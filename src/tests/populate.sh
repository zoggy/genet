WHY=`./genet add tool why`
./genet add branch ${WHY} 1.x
./genet add branch ${WHY} 2.x
./genet add branch ${WHY} 3.x
./genet add version ${WHY} ${WHY}/branches/1.x 1.1
./genet add version ${WHY} ${WHY}/branches/1.x 1.2
./genet add version ${WHY} ${WHY}/branches/1.x 1.3
./genet add version ${WHY} ${WHY}/branches/2.x 2.1
./genet add interface ${WHY}/branches/1.x prove
./genet add interface ${WHY}/branches/1.x prove2
./genet add filetype "coq" v "coq file"
./genet add filetype "ocaml-impl" ml "ocaml implementation"
./genet add filetype "ocaml-intf" mli "ocaml interface"
