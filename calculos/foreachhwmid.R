library(extRemes)
library(ncdf4)
library(Matrix)
library(ncdf4.helpers)
library(parallel)
library(foreach)

setwd("/home/UCLM/mofelia.molina/taller")
args <- commandArgs(trailingOnly = TRUE)

nchis <- nc_open(args[1])
#n=nc_open("tasmax_EUR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_CLMcom-CCLM4-8-17_v1_day_1971-2100.nc")
tasmax <- ncvar_get(nchis, "tasmax")
#ts <- nc.get.time.series(nchis) #Para pasar a PCIC el formato de fechas
tasmaxhis<-tasmax[,,1:11688]
#tasmaxfut<-tasmax[,,12784:47482]
tasmaxfut<-tasmax

#nc.get.dim.axes(n)
nchis$dim$rlon$vals->xlon
nchis$dim$rlat$vals->xlat
xmax<-length(xlon) #103
ymax<-length(xlat) #106

#CNRM pide las dimensiones de esta forma:
#nchis$dim$x$vals->xlon
#nchis$dim$y$vals->xlat
#xmax<-length(xlon) #xmax=117
#ymax<-length(xlat) #ymax=117

atot<-130
hwmidmatrix<-array(0,c(xmax,ymax,atot))
#hwmidmatrixl<-array(0,c(xmax,ymax,atot))
#hwmidmatrixs<-array(0,c(xmax,ymax,atot))

no_cores=detectCores()
cl=makeCluster(no_cores)
library(doParallel)
registerDoParallel(cores=2)

foreach (xx = 1:xmax) %dopar% {
  foreach (yy = 1:ymax) %dopar% {
    
    resd<-hwmid(1971,tasmaxhis[xx,yy,],2006,tasmaxfut[xx,yy,])
    
    hwmidtot<-resd$hwmid
    
    hwmid<-hwmidtot[1:atot,1]
    #hwmidlen<-hwmidtot[1:atot,2]
    #hwmidstart<-hwmidtot[1:atot,3]
    
    hwmidmatrix[xx,yy,1:atot]<-hwmid
    #hwmidmatrixl[xx,yy,1:atot]<-hwmidlen
    #hwmidmatrixs[xx,yy,1:atot]<-hwmidstart
  }}
stopCluster()

dim=dim(hwmidtot)

aa<-c(1:atot)
setwd("/home/UCLM/mofelia.molina/taller/")
ncdout<-args[2]
londim <- ncdim_def("lon","degrees_east",as.double(xlon))
latdim <- ncdim_def("lat","degrees_north",as.double(xlat))
aadim <- ncdim_def("time","year",as.double(aa))
vnamei<-"hwmid"
#vnamel<-"hwmidl"
#vnamed<-"hwmids"
fillValue <- 1e32

hwmiddefi<-ncvar_def("hwmid","",list(londim,latdim,aadim),fillValue,vnamei,prec="single")
#hwmiddefl<-ncvar_def("hwmidl","days",list(londim,latdim,aadim),fillValue,vnamel,prec="single")
#hwmiddefs<-ncvar_def("hwmids","days",list(londim,latdim,aadim),fillValue,vnamed,prec="single")
ncsfin<-nc_create(ncdout,list(hwmiddefi),force_v4=T)
ncvar_put(ncsfin,hwmiddefi,hwmidmatrix)
#ncvar_put(ncsfin,hwmiddefl,hwmidmatrixl)
#ncvar_put(ncsfin,hwmiddefs,hwmidmatrixs)
ncatt_put(ncsfin,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
ncatt_put(ncsfin,"lat","axis","Y")
nc_close(ncsfin)