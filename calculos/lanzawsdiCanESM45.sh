#!/bin/bash
f1i="tasmax_day_CanESM2_rcp45_r1i1p1_1971-2100.nc"
f1f="wsdiCanESM45.nc"

# De esta manera puedo lanzar a la vez muchos calculos, uno por nodo/cpu
# Y se hace con ./lanza.sh sin mas

sbatch calculawsdimaria3.sh $f1i $f1f 
