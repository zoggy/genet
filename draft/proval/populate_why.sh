GENET="../../src/genet --config ../../src/config.txt"
WHY_GIT_ID=15515d3efeeca8d2af20c99db75d6681edac610a
cp -f why3_split.sh /opt/why3-${WHY_GIT_ID}/bin/

WHY=`${GENET} add tool why3`
${GENET} add branch ${WHY} 0.x
${GENET} add version ${WHY} ${WHY}/branches/0.x ${WHY_GIT_ID}
SPLIT=`${GENET} add interface -p "/opt/why3-%v/bin/why3_split.sh" ${WHY}/branches/0.x split-2-altergo`
${GENET} add filetype "mlw" mlw "why3 file"
${GENET} add filetype "why"  why "why/alt-ergo file"
${GENET} add filetype "ae_result"  ae_result "alt-ergo result file"
${GENET} add port ${SPLIT} in mlw
${GENET} add port ${SPLIT} out "why set"

ALTERGO=`${GENET} add tool altergo`
cp -f alt-ergo_prove.sh /opt/alt-ergo-0.94/bin/
${GENET} add branch ${ALTERGO} 0.x
${GENET} add version ${ALTERGO} ${ALTERGO}/branches/0.x 0.94
AE_PROVE=`${GENET} add interface -p "/opt/alt-ergo-%v/bin/alt-ergo_prove.sh" ${ALTERGO}/branches/0.x ae-prove`
${GENET} add port ${AE_PROVE} in why
${GENET} add port ${AE_PROVE} out "ae_result"
