#!/bin/bash
f1i="tasmax_EUR-44_ICHEC-EC-EARTH_rcp45_r12i1p1_SMHI-RCA4_v1_day_1971-2100.nc"
f1f="wsdi130arca44ecearth45.nc"
date
Rscript wsdi130arcm.R ${f1i} ${f1f}
date
