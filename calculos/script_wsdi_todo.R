## Sugerencias:

## Los comentarios con 2 signos # (bueno, esto es casi obligatorio).

## En lugar de cambiar el directorio de trabajo cada vez, que además cada uno tendremos uno distinto, se puede poner la ruta en la lectura del fichero. Si los datos están siempre en una carpeta datos, por ejemplo, esa ruta no haría falta cambiarla. 

## Dejo estos comentarios pero en el futuro hay que borrarlos para que quede más claro.

##setwd("~/Data")
##setwd("/home/kike/Escritorio/Dropbox/investigacion/tesis/tesismariamolina/olasdecaloreurocordex/calculos/")

## A mi no me sirve la siguiente sentencia, así que la comento.

##setwd("/home/kike/taller/olas/") # directorio con el fichero completo 106x103

################################################

## 1. Cargo librerías

library(ncdf4)
library(climdex.pcic)
library(Matrix) ## necesario para definir la matrix para asignar wsdiavg en cada punto

library(raster)
library(rasterVis)
library(maps)
library(mapdata)
library(maptools)

data('worldEnv') ## para que salgan las líneas de los paísesposteriores al 91

#################################################

## 2. Lectura del fichero .nc y las variables necesarias.  

## con la librería ncdf4:

ncfich<-nc_open("tasmax_EUR-44_CCCma-CanESM2_rcp85_r1i1p1_SMHI-RCA4_v1_day_2071-2100.nc")
xtasmax<-ncvar_get(ncfich,"tasmax")

## las siguientes instrucciones no te dan el valor de la latitud y la longitud, sino el valor de las celdas, es decir las x y las y. La latitud y la longitud son variables en el nc y hay que extraerlas como tal. Por eso en la representación que hacías, los ejes no eran correctos.

ncfich$dim$x$vals->xlon
ncfich$dim$y$vals->xlat

## xv1<-xtasmax[,,1] estas dos instrucciones son solo para hacer pruebas para pintar campos 2d
## image(xlon,xlat,xv1)

## 2.a Leo el fichero con raster:

## obtengo un raster con los valores de temperatura:

mync <- stack("/home/automata/maria/tasmax_EUR-44_CCCma-CanESM2_rcp85_r1i1p1_SMHI-RCA4_v1_day_2071-2100.nc", varname='tasmax')

## puedo obtener los valores de lat y lon de la misma manera:
mynclat <- raster("/home/automata/maria/tasmax_EUR-44_CCCma-CanESM2_rcp85_r1i1p1_SMHI-RCA4_v1_day_2071-2100.nc", varname='lat')
mynclon <- raster("/home/automata/maria/tasmax_EUR-44_CCCma-CanESM2_rcp85_r1i1p1_SMHI-RCA4_v1_day_2071-2100.nc", varname='lon')

## Estas sentencias crean una matriz con el numero de celda y el valor de lat y lon.

lat <- rasterToPoints(mynclat)
lon <- rasterToPoints(mynclon)
lonlat <- cbind(plon[,3], plat[,3])

## Si observais el raster mync, veréis que no hay una proyección asignada. Para poder representar en R necesitamos conocer la proyección y asignársela. Ferret debe hacer lo mismo que os pongo aquí a continuación, que es estimarla con los valores de lat y lon y después, según la imagen que mandaste, Kike, proyectarlo a latlon.

## defino la proyección lonlat:

crslonlat <- CRS("+proj=longlat +datum=WGS84")

## defino la proyección LCC aunque la original la desconozco y los valores con los que está definida podrían cambiar un poco:

mycrs <- CRS("+proj=lcc +lat_1=43.f +lat_0=43.f +lon_0=15.f +k=0.684241 +units=m +datum=WGS84 +no_defs")

## creo un objeto espacial, es decir, con una proyección definida con los datos de lat y lon de my nc

splonlat <- SpatialPoints(lonlat,proj4string=crs("+proj=longlat +datum=WGS84"))

## transformo los puntos lonlat a LCC
plonlat <- spTransform(splonlat, CRSobj = mycrs)

## Asigno esta proyección al raster:

projection(mync) <- mycrs
extent(mync) <- extent(plonlat)

## 2.b representación

## Con plot (base de R)##

## utiliza maps para las líneas de costa y países. Para que sea más sencillo, representamos en latlon. Para ello pasamos el raster a latlon.

## el raster tiene tantas capas como pasos de tiempo, por lo que selecciono una única capa para representar:

mync2 <- mync[[1]]

## paso a latlon

mync2 <- projectRaster(mync2, crslonlat)
extent(mync2) <- extent(splonlat)

## represento:

pdf("bc.pdf")
plot(mync2)
maps(add=TRUE)
dev.off()

## con levelplot (y proyeccion latlon)

pdf("bclevel.pdf")
levelplot(mync2) ## añadir el mapa requiere más pasos.
dev.off()

########################################################

tt<-ncvar_get(ncfich,"time")
xdias <- seq(as.Date(tt[1]+30, format = "%d/%m/%Y",origin="01/12/1949"),by = "days", length = length(tt)+7)
xdias365<-xdias[format(xdias,"%m-%d")!="02-29"]
tmax.dates.rcm <- as.character(xdias365)
cal<-"365_day"
tt<-as.PCICt(tmax.dates.rcm,cal)
# Estas instrucciones son necesarias para adaptar el eje de tiempos a lo que pide la funcion climdex
wsdiavg<-Matrix(0,nrow=length(xlon),ncol=length(xlat))
# Para cada celdilla, se va calculando wsdi
for (xx in 1:length(xlon)){
  for (yy in 1:length(xlat)){
#    xdias<-seq(1,10950)
    vv<-xtasmax[xx,yy,1:length(tt)] # Asi se queda con la serie temporal de cada punto
    circm<-climdexInput.raw(vv,vv,vv,tt,tt,tt, base.range=c(2071, 2100))
    wsdi<-climdex.wsdi(circm)
    wsdiavg[xx,yy]<-sum(wsdi)/30.
# De momento, guardamos el promedio a 30 años del valor anual que calcula por defecto climdex.wsdi
  }
}
#image(wsdiavg), esto es para representar graficamente
# Estas instrucciones son para generar un nc con el promedio anual
setwd("/home/kike/Escritorio/Dropbox/investigacion/tesis/tesismariamolina/olasdecaloreurocordex/calculos/")
ncdout<-"wsdiavgp1.nc"
londim <- ncdim_def("lon","degrees_east",as.double(xlon)) 
latdim <- ncdim_def("lat","degrees_north",as.double(xlat))
vname<-"wsdi"
fillValue <- 1e32
wsdidef<-ncvar_def("wsdi","days",list(londim,latdim),fillValue,vname,prec="single")
ncsfin<-nc_create(ncdout,wsdidef,force_v4=T)
ncvar_put(ncsfin,wsdidef,wsdiavg)
ncatt_put(ncsfin,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
ncatt_put(ncsfin,"lat","axis","Y")
nc_close(ncsfin)
