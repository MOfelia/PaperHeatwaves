#Este script calcula el hwmi para un área de 0.44: 106x106
# Mirar el script para wsdi.R, porque los comentarios
# son similares. Aqui se usan los ficheros de clima actual
# y futuro, todavía no lo retoque para poder meter
# como argumento los ficheros, y asi no tener que usar para
# cada para de simulaciones un script R distinto.
# Cuando probe con el apply-apply en vez de los bucles
# en vez de tardar 1h en los calculos, se quedaba colgado,
# asi que de momento lo dejo así.
library(extRemes)
library(ncdf4)
library(Matrix)
setwd("/home/UCLM/mofelia.molina/taller")
args <- commandArgs(trailingOnly = TRUE)


atot<-130
nchis <- nc_open(args[1])
tasmax <- ncvar_get(nchis, "tasmax")
#ncfut<- nc_open(args[2])
tasmaxhis<-tasmax[,,1:11688] # KIKE: 12053 hasta 32a (2002)
# 11688 sería hasta 2002
tasmaxfut<- ncvar_get(nchis, "tasmax")
tasmaxfut<- tasmaxfut[,,36532:47482] # ultimos 30a


nchis$dim$x$vals->xlon
nchis$dim$y$vals->xlat
xmax<-length(xlon)
ymax<-length(xlat)
hwmimatrixi<-array(0,c(xmax,ymax,atot)) # Para los valores anuales
hwmimatrixl<-array(0,c(xmax,ymax,atot))
hwmidmatrixi<-array(0,c(xmax,ymax,atot))
#hwmimatrixi<-Matrix(0,nrow=xmax,ncol=ymax) # Para los valores anuales
#hwmidmatrixi<-Matrix(0,nrow=xmax,ncol=ymax)
#hwmimatrixd<-Matrix(0,nrow=xmax,ncol=ymax)


#res<-apply(tasmaxhis, 1:2,FUN= function(x) apply(tasmaxfut, 1:2,FUN= function(y) hwmi(1971, x, 2071, y)))

for (xx in 1:xmax){
  for (yy in 1:ymax){
    res<-hwmi(1971,tasmaxhis[xx,yy,],2071,tasmaxfut[xx,yy,])
    resd<-hwmid(1971,tasmaxhis[xx,yy,],2071,tasmaxfut[xx,yy,])
    #hwmitot<-res[[xx]][[yy]]$hwmi
    hwmitot<-res$hwmi
    hwmidtot<-resd$hwmi
    hwmiindex<-hwmitot[1:30,1]
    hwmilen<-hwmitot[1:30,2]
    hwmid<-hwmidtot[1:30,1]
    hwmimatrixi[xx,yy,1:30]<-hwmiindex
    hwmimatrixl[xx,yy,1:30]<-hwmilen
    hwmidmatrixi[xx,yy,1:30]<-hwmid
    #hwmimatrixi[xx,yy]<-hwmiindex
    #hwmimatrixd[xx,yy]<-hwmidur
    #hwmidmatrixi[xx,yy]<-hwmistart
  }}
aa<-c(1:30)
setwd("/home/UCLM/mofelia.molina/taller/")
ncdout<-args[2]
londim <- ncdim_def("lon","degrees_east",as.double(xlon))
latdim <- ncdim_def("lat","degrees_north",as.double(xlat))
aadim <- ncdim_def("time","year",as.double(aa))
vnamei<-"hwdii"
vnamel<-"hwdil"
vnamed<-"hwdid"
fillValue <- 1e32
#hwmidef<-ncvar_def("hwmii","",list(londim,latdim),fillValue,vnamei,prec="single")
#hwmiddef<-ncvar_def("hwmidur","days",list(londim,latdim),fillValue,vnamed,prec="single")
#hwmisdef<-ncvar_def("hwmid","",list(londim,latdim),fillValue,vnames,prec="single")
hwmidefi<-ncvar_def("hwmii","",list(londim,latdim,aadim),fillValue,vnamei,prec="single")
hwmidefl<-ncvar_def("hwmil","days",list(londim,latdim,aadim),fillValue,vnamel,prec="single")
hwmiddef<-ncvar_def("hwmid","days",list(londim,latdim,aadim),fillValue,vnamed,prec="single")
ncsfin<-nc_create(ncdout,list(hwmidefi,hwmidefl,hwmiddef),force_v4=T)
ncvar_put(ncsfin,hwmidefi,hwmimatrixi)
ncvar_put(ncsfin,hwmidefl,hwmimatrixl)
ncvar_put(ncsfin,hwmiddef,hwmidmatrixi)
ncatt_put(ncsfin,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
ncatt_put(ncsfin,"lat","axis","Y")
nc_close(ncsfin)

