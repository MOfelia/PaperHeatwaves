library(raster)
library(ncdf4)
library(ncdf4.helpers)
library(extRemes)
library(climdex.pcic)
setwd("/home/kike/Escritorio/Dropbox/investigacion/tesis/tesismariamolina/olasdecaloreurocordex/calculos/")
#setwd("/home/kike/Escritorio/")
#Tref <- nc_open("TrefHWMI_tasmax_EUR-44_CCCma-CanESM2_historical_r1i1p1_SMHI-RCA4_v1_day.nc")
#vartref <- ncvar_get(Tref, "tasma")
#t55ref <- vartref[5,5,]
#Temp <- nc_open("tasmax_EUR-44_CCCma-CanESM2_rcp85_r1i1p1_SMHI-RCA4_v1_day_2071-2100.nc")
Temp <- nc_open("5x5_tasmax_EUR-44_CCCma-CanESM2_rcp85_r1i1p1_SMHI-RCA4_v1_day_2071-2100.nc")
vartemp <- ncvar_get(Temp, "tasmax")
tt<-ncvar_get(Temp,"time")
#as.data.frame(tt)
# Sabiendo que el día 1 es, según ncdump time:units = "days since 1949-12-1 00:00:00" ; 
xdias <- seq(as.Date(tt[1]+30, format = "%d/%m/%Y",origin="01/12/1949"),by = "days", length = length(tt)+7)
# Al ser calendario de 365dias, empezaria el 2070-12-02, faltan todos los bisiestos. Ese 30 hace que empiece el 1-1-2071
# La libreria lubridate sirve para usar funciones tipo year, month...
library("lubridate")
year(xdias[1]) # Para probar inicio
year(xdias[length(xdias)]) # Para probar las funciones
xdias365<-xdias[format(xdias,"%m-%d")!="02-29"]
                                        # xdias365 es igual, pero quitando los bisiestos, que es lo que queremos: mide 10950, en vez de los 10957 de xdias, que incluye bisiestos
tmax.dates <- as.character(xdias365)
tmax.dates <- as.PCICt(tmax.dates, cal="365_day", format="%d/%m/%Y")
ci <- climdexInput.raw(vartemp,tmax.dates, base.range=c(1970-2100))


#Calcular el fichero de fechas del periodo de referencia:
Tref <- nc_open("1x1_tasmax_EUR-44_CCCma-CanESM2_historical_r1i1p1_SMHI-RCA4_v1_day_1971-2000.nc")
vartref <- ncvar_get(Tref, "tasmax")
ttref <- ncvar_get(Tref,"time")
xdiasref <- seq(as.Date(ttref[1]+30, format = "%d/%m/%Y",origin="01/12/1949"),by = "days", length = length(ttref)+7)
year(xdiasref[1]) # Para probar inicio
year(xdiasref[length(xdiasref)]) # Para probar las funciones
###El periodo termina en 2001, por que??
###Si en vez de sumarle 7 al final, le resto 17, ya termina en el año 2000
xdias365tref<-xdiasref[format(xdiasref,"%m-%d")!="02-29"]
tmax.dates.ref <- as.character(xdias365tref)
tmax.dates.ref <- as.PCICt(tmax.dates.ref, cal="365_day", format="%d/%m/%Y")
ci <- climdexInput.raw(vartref,tmax.dates.ref, base.range=c(1970-2100))

#Crear la variable de precipitación y tasmin:
pr_his <- nc_open("1x1_pr_EUR-44_CCCma-CanESM2_historical_r1i1p1_SMHI-RCA4_v1_day_1971-2000.nc")
var_pr_his <- ncvar_get(pr_his, "pr")
tasmin_his <- nc_open("1x1_tasmin_EUR-44_CCCma-CanESM2_historical_r1i1p1_SMHI-RCA4_v1_day_1971-2000.nc")
var_tasmin_his <- ncvar_get(tasmin_his, "tasmin")
tasmin_rcp85 <- nc_open("1x1_tasmin_EUR-44_CCCma-CanESM2_rcp85_r1i1p1_SMHI-RCA4_v1_day_2071-2100.nc")
var_tasmin_rcp85 <- ncvar_get(tasmin_rcp85, "tasmin")
