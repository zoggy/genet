WHY=`./genet add tool why`
./genet add branch ${WHY} 1.x
./genet add branch ${WHY} 2.x
./genet add branch ${WHY} 3.x
./genet add version ${WHY} ${WHY}/branches/1.x 1.1
./genet add version ${WHY} ${WHY}/branches/1.x 1.2
./genet add version ${WHY} ${WHY}/branches/1.x 1.3
./genet add version ${WHY} ${WHY}/branches/2.x 2.1
./genet add version ${WHY} ${WHY}/branches/2.x 07a10043b2f4ee1d9edae560b1c567029e59dd21
./genet add version ${WHY} ${WHY}/branches/2.x ff51ebabd5953c54607fdd3a2251fb8095806cfe
PROVE=`./genet add interface ${WHY}/branches/1.x prove`
PROVE2=`./genet add interface ${WHY}/branches/1.x prove2`
./genet add filetype "coq" v "coq file"
./genet add filetype "coqo" vo "coq object file"
./genet add filetype "ocamlimpl" ml "ocaml implementation"
./genet add filetype "ocamlintf" mli "ocaml interface"
./genet add port ${PROVE} in coq
./genet add port ${PROVE} in coqo
./genet add port ${PROVE} out ocamlimpl
./genet add port ${PROVE} out coq
./genet add port ${PROVE2} in ocamlimpl
./genet add port ${PROVE2} in ocamlimpl
./genet add port ${PROVE2} out coq


ALTERGO=`./genet add tool altergo`
./genet add branch ${ALTERGO} 1.x
./genet add branch ${ALTERGO} 2.x
./genet add branch ${ALTERGO} 3.x
./genet add version ${ALTERGO} ${ALTERGO}/branches/1.x 1.1
./genet add version ${ALTERGO} ${ALTERGO}/branches/1.x 1.2
./genet add version ${ALTERGO} ${ALTERGO}/branches/2.x 2.1
./genet add version ${ALTERGO} ${ALTERGO}/branches/2.x 2.2
AE_PROVE=`./genet add interface ${ALTERGO}/branches/1.x ae-prove`
AE_PROVE2=`./genet add interface ${ALTERGO}/branches/2.x ae-prove2`
./genet add port ${AE_PROVE} in coq
./genet add port ${AE_PROVE} in coqo
./genet add port ${AE_PROVE} out ocamlimpl
./genet add port ${AE_PROVE} out coq
./genet add port ${AE_PROVE2} in ocamlimpl
./genet add port ${AE_PROVE2} in ocamlimpl
./genet add port ${AE_PROVE2} out coq
