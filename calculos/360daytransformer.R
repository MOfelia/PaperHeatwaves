####Script para transformar los archivos ncdf con calendario 360 días para que puedan ser leídos por la librería extRemes

library(data.table)
library(ncdf4)
library(ncdf4.helpers)
library(extRemes)
library(Matrix)

setwd("/home/UCLM/mofelia.molina/taller")
args <- commandArgs(trailingOnly = TRUE)

model <- nc_open(args[1])
time <- ncvar_get(model, "time")
time.vector <- as.vector(time)
time <- as.character(time)
time.dataframe <- as.data.frame(time)

tt<- as.Date(time.vector, origin = "1949-12-01") #desde 1970-09-12 a 2097-10-04

model <- stack(args[1])
modelZ <- setZ(model, tt)
nchis <- ncvar_get(modelZ, "tasmax")

tasmax <- ncvar_get(nchis, "tasmax")

tasmaxhis<-tasmax[,,1:11688]

tasmaxfut<-tasmax

#nc.get.dim.axes(n)
nchis$dim$rlon$vals->xlon
nchis$dim$rlat$vals->xlat
xmax<-length(xlon) #103
ymax<-length(xlat) #106

atot<-130
hwmidmatrix<-array(0,c(xmax,ymax,atot))
hwmidmatrixl<-array(0,c(xmax,ymax,atot))
hwmidmatrixs<-array(0,c(xmax,ymax,atot))

for (xx in 1:xmax){
  for (yy in 1:ymax){
    
    resd<-hwmid(1971,tasmaxhis[xx,yy,],1971,tasmaxfut[xx,yy,])
    
    hwmidtot<-resd$hwmid
    
    hwmid<-hwmidtot[1:atot,1]
    hwmidlen<-hwmidtot[1:atot,2]
    hwmidstart<-hwmidtot[1:atot,3]
    
    hwmidmatrix[xx,yy,1:atot]<-hwmid
    hwmidmatrixl[xx,yy,1:atot]<-hwmidlen
    hwmidmatrixs[xx,yy,1:atot]<-hwmidstart
  }}
dim=dim(hwmidtot)

aa<-c(1:atot)
setwd("/home/UCLM/mofelia.molina/taller/")
ncdout<-args[2]
londim <- ncdim_def("lon","degrees_east",as.double(xlon))
latdim <- ncdim_def("lat","degrees_north",as.double(xlat))
aadim <- ncdim_def("time","year",as.double(aa))
vnamei<-"hwmid"
vnamel<-"hwmidl"
vnamed<-"hwmids"
fillValue <- 1e32

hwmiddefi<-ncvar_def("hwmid","",list(londim,latdim,aadim),fillValue,vnamei,prec="single")
hwmiddefl<-ncvar_def("hwmidl","days",list(londim,latdim,aadim),fillValue,vnamel,prec="single")
hwmiddefs<-ncvar_def("hwmids","days",list(londim,latdim,aadim),fillValue,vnamed,prec="single")
ncsfin<-nc_create(ncdout,list(hwmiddefi,hwmiddefl,hwmiddefs),force_v4=T)
ncvar_put(ncsfin,hwmiddefi,hwmidmatrix)
ncvar_put(ncsfin,hwmiddefl,hwmidmatrixl)
ncvar_put(ncsfin,hwmiddefs,hwmidmatrixs)
ncatt_put(ncsfin,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
ncatt_put(ncsfin,"lat","axis","Y")
nc_close(ncsfin)


#dt360 <- data.table(time.vector, tt)

#mutate(time.dataframe, time = as.Date(as.numeric(time), origin = "1949-12-01"))



