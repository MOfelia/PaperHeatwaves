#!/bin/bash
f1i="tasmax_EUR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_CLMcom-CCLM4-8-17_v1_day_1971-2100.nc"
f1f="hwmi44MPI-CCLM45.nc"

# De esta manera puedo lanzar a la vez muchos calculos, uno por nodo/cpu
# Y se hace con ./lanza.sh sin mas

sbatch calculahwmimaria.sh $f1i $f1f 
