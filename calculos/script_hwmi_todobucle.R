#Este script calcula el hwmi para un área de 5x5

library(extRemes)
library(ncdf4)
library(Matrix)
#setwd("/home/kike/Escritorio/Dropbox/investigacion/tesis/tesismariamolina/olasdecaloreurocordex/calculos/datos/")
setwd("/home/UCLM/e.sanchez/pruebasR/datos/")
#tas5x5 <- nc_open("5x5_tasmax_EUR-44_CCCma-CanESM2_historical_r1i1p1_SMHI-RCA4_v1_day_1970-2000.nc")
nchis <- nc_open("tasmax_EUR-44_CCCma-CanESM2_historical_r1i1p1_SMHI-RCA4_v1_day_1971-2000.nc")
tasmaxhis <- ncvar_get(nchis, "tasmax")
#temp5x5<- nc_open("5x5_tasmax_EUR-44_CCCma-CanESM2_rcp85_r1i1p1_SMHI-RCA4_v1_day_2071-2100.nc")
ncfut<- nc_open("tasmax_EUR-44_CCCma-CanESM2_rcp85_r1i1p1_SMHI-RCA4_v1_day_2071-2100.nc")
tasmaxfut<- ncvar_get(ncfut, "tasmax")


nchis$dim$x$vals->xlon
nchis$dim$y$vals->xlat
xmax<-length(xlon)
ymax<-length(xlat)
hwmimatrixi<-Matrix(0,nrow=xmax,ncol=ymax)
hwmimatrixd<-Matrix(0,nrow=xmax,ncol=ymax)
hwmimatrixs<-Matrix(0,nrow=xmax,ncol=ymax)

#res<-apply(tasmaxhis, 1:2,FUN= function(x) apply(tasmaxfut, 1:2,FUN= function(y) hwmi(1971, x, 2071, y)))

for (xx in 1:xmax){
for (yy in 1:ymax){
res<-hwmi(1971,tasmaxhis[xx,yy,],2071,tasmaxfut[xx,yy,])
hwmitot<-res$hwmi
hwmiindex<-sum(hwmitot[1:30,1]/30.)
hwmidur<-sum(hwmitot[1:30,2]/30.)
hwmistart<-sum(hwmitot[1:30,3]/30.)
hwmimatrixi[xx,yy]<-hwmiindex
hwmimatrixd[xx,yy]<-hwmidur
hwmimatrixs[xx,yy]<-hwmistart
}}
#setwd("/home/kike/Escritorio/Dropbox/investigacion/tesis/tesismariamolina/olasdecaloreurocordex/calculos/")
setwd("/home/UCLM/e.sanchez/pruebasR/")
ncdout<-"hwmiavgp2.nc"
londim <- ncdim_def("lon","degrees_east",as.double(xlon))
latdim <- ncdim_def("lat","degrees_north",as.double(xlat))
vname<-"hwdii"
fillValue <- 1e32
hwmidef<-ncvar_def("hwdii","",list(londim,latdim),fillValue,vname,prec="single")
hwmiddef<-ncvar_def("hwdid","days",list(londim,latdim),fillValue,vname,prec="single")
hwmisdef<-ncvar_def("hwdis","day",list(londim,latdim),fillValue,vname,prec="single")
ncsfin<-nc_create(ncdout,list(hwmidef,hwmiddef,hwmisdef),force_v4=T)
ncvar_put(ncsfin,hwmidef,hwmimatrixi)
ncvar_put(ncsfin,hwmiddef,hwmimatrixd)
ncvar_put(ncsfin,hwmisdef,hwmimatrixs)
ncatt_put(ncsfin,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
ncatt_put(ncsfin,"lat","axis","Y")
nc_close(ncsfin)


#str(res)
#save(res, file="hwmiresults.Rdata")
#write.table(res[[1:5]][[1:5]][[1]],file="resultados.dat",col.names=F,quote=F)
#for (xx in c(1:5)) {for (yy in c(1:5)) {write.table(data.frame(xx,yy,res[[xx]][[yy]][[1]]),file="resultadosv2.dat",col.names=F,quote=F,append=T)}}
#for (xx in c(1:5)) {for (yy in c(1:5)) {print(res[[xx]][[yy]][[1]])}}
