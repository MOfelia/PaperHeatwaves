setwd("~/Data")
library(ncdf4)
library(climdex.pcic)
Temp <- nc_open("1x1_tasmax_EUR-44_CCCma-CanESM2_rcp85_r1i1p1_SMHI-RCA4_v1_day_2071-2100prueba.nc")
vartemp <- ncvar_get(Temp, "tasmax")
tt<-ncvar_get(Temp,"time")
xdias <- seq(as.Date(tt[1]+30, format = "%d/%m/%Y",origin="01/12/1949"),by = "days", length = length(tt)+7)
xdias365<-xdias[format(xdias,"%m-%d")!="02-29"]
tmax.dates.rcm <- as.character(xdias365)
cal<-"365_day"
tt<-as.PCICt(tmax.dates.rcm,cal)
circm<-climdexInput.raw(vartemp,vartemp,vartemp,tt,tt,tt, base.range=c(2071, 2100))
wsdi<-climdex.wsdi(circm)

Tref <- nc_open("1x1_tasmax_EUR-44_CCCma-CanESM2_historical_r1i1p1_SMHI-RCA4_v1_day_1971-2000prueba.nc")
vartref <- ncvar_get(Tref, "tasmax")
ttref<- ncvar_get(Tref,"time")
xdiasref <- seq(as.Date(ttref[1]+5, format = "%d/%m/%Y",origin="01/12/1949"),by = "days", length = length(ttref)+8)
###He cambiado a +5 para que empiece el 1 de enero de 1971 y al final length +8 para que tenga la misma legth que vartref
xdias365ref<-xdiasref[format(xdiasref,"%m-%d")!="02-29"]
tmax.dates.ref <- as.character(xdias365ref)
cal<-"365_day"
ttref<-as.PCICt(tmax.dates.ref,cal)
ciref<-climdexInput.raw(vartref,vartref,vartref,ttref,ttref,ttref, base.range=c(1971, 2000))
wsdiref<- climdex.wsdi(ciref)

#Se consigue calcular el wsdi con 4 puntos cambiando el length del fichero de fechas:
Tref4<-nc_open("1x1_tasmax_EUR-44_CCCma-CanESM2_historical_r1i1p1_SMHI-RCA4_v1_day_1971-2000.nc")
vartref4 <- ncvar_get(Tref4, "tasmax")
ttref4<- ncvar_get(Tref4,"time")
xdiasref4 <- seq(as.Date(ttref4[1]+5, format = "%d/%m/%Y",origin="01/12/1949"),by = "days", length = length(ttref4)+8)
 ###He cambiado a +5 para que empiece el 1 de enero de 1971 y al final length +8 para que tenga la misma legth que vartref
 xdias365ref4<-xdiasref4[format(xdiasref4,"%m-%d")!="02-29"]
tmax.dates.ref4 <- as.character(xdias365ref4)
cal<-"365_day"
ttref4<-as.PCICt(tmax.dates.ref4,cal)
length(ttref4)
#[1] 10950
ttref4<- rep(ttref4,4)
 length(ttref4)
#[1] 43800
 ciref4<-climdexInput.raw(vartref4,vartref4,vartref4,ttref4,ttref4,ttref4, base.range=c(1971, 2000))
 wsdiref4<- climdex.wsdi(ciref4)
 wsdiref4
 
 #Prueba con fichero  completo:
 Tref.his<-nc_open("tasmax_EUR-44_CCCma-CanESM2_historical_r1i1p1_SMHI-RCA4_v1_day_1971-2000.nc")
 tthis<- ncvar_get(Tref.his,"time")
 var.his <- ncvar_get(Tref.his, "tasmax")
 xdiashis <- seq(as.Date(tthis[1]+5, format = "%d/%m/%Y",origin="01/12/1949"),by = "days", length = length(tthis)+8)
 ###He cambiado a +5 para que empiece el 1 de enero de 1971 y al final length +8 para que tenga la misma legth que vartref
   xdias365his<-xdiashis[format(xdiashis,"%m-%d")!="02-29"]
 tmax.dates.his <- as.character(xdias365his)
 cal<-"365_day"
 tthis<-as.PCICt(tmax.dates.his,cal)
 #length(tthis)
 #length(var.his)
 #[1] 119552100
 #> 119552100/10950
 #[1] 10918
 tthis<- rep(tthis,10918)
 length(tthis)
 cihis<-climdexInput.raw(var.his,var.his,var.his,tthis,tthis,tthis,base.range=c(1971, 2000))
 wsdihis<- climdex.wsdi(cihis)
 wsdihis
save(wsdi85, file= "wsdihis.Rdata")


##Fichero rcp85 completo:

T85<- nc_open("tasmax_EUR-44_CCCma-CanESM2_rcp85_r1i1p1_SMHI-RCA4_v1_day_2071-2100.nc")
 tt85<- ncvar_get(T85, "time")
 var85<- ncvar_get(T85, "tasmax")
 xdias85 <- seq(as.Date(tt85[1]+30, format = "%d/%m/%Y",origin="01/12/1949"),by = "days", length = length(tt85)+7)
  ###He cambiado a length +8 para que tenga la misma legth que vartref
   xdias365.85<-xdias85[format(xdias85,"%m-%d")!="02-29"]
  tmax.dates.85 <- as.character(xdias365.85)
  cal<-"365_day"
  tt85<-as.PCICt(tmax.dates.85,cal)
  tt85<- rep(tt85,10918)
 length(tt85)
 ci85<-climdexInput.raw(var85,var85,var85,tt85,tt85,tt85,base.range=c(2071, 2100))
 wsdi85<- climdex.wsdi(ci85)
 wsdi85
save(wsdi85, file= "wsdi85.Rdata")

