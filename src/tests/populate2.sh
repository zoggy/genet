for i in 1 2 3 4 5 6 7 8 9; do
  for j in 0 1 2 3 4 5 6 7 8 9; do
    URI=`./genet add tool pipotool${i}${j}`;\
    for k in 0 1 2 3 4 5 6 7 8 9; do
      URIB=`./genet add branch ${URI} ${k}.x` ;\
      for v in 0 1 2 3 4 5 6 7 8 9; do
        ./genet add version ${URI} ${URIB} ${k}.${v}; done; done; done; done

