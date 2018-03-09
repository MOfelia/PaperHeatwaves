setwd("/home/UCLM/e.sanchez/olasdecalor/datos/")
library(ncdf4)
library(climdex.pcic)
library(Matrix) # necesario para definir la matrix para asignar wsdiavg en cada punto

args <- commandArgs(trailingOnly = TRUE)
#rnorm(n=as.numeric(args[1]), mean=as.numeric(args[2]))
ncfich<-nc_open(args[1])

xtasmax<-ncvar_get(ncfich,"tasmax")
ncfich$dim$x$vals->xlon
ncfich$dim$y$vals->xlat
#xv1<-xtasmax[,,1] estas dos instrucciones son solo para hacer pruebas para pintar campos 2d
#image(xlon,xlat,xv1)
tt<-ncvar_get(ncfich,"time")
xdias <- seq(as.Date(tt[1]+30, format = "%d/%m/%Y",origin="01/12/1949"),by = "days", length = length(tt)+7)
xdias365<-xdias[format(xdias,"%m-%d")!="02-29"]
tmax.dates.rcm <- as.character(xdias365)
cal<-"365_day"
tt<-as.PCICt(tmax.dates.rcm,cal)
# Estas instrucciones son necesarias para adaptar el eje de tiempos a lo que pide la funcion climdex
wsdiavg<-Matrix(0,nrow=length(xlon),ncol=length(xlat))
wsdimax<-Matrix(0,nrow=length(xlon),ncol=length(xlat))
# Para cada celdilla, se va calculando wsdi
for (xx in 1:length(xlon)){
  for (yy in 1:length(xlat)){
#    xdias<-seq(1,10950)
    vv<-xtasmax[xx,yy,1:length(tt)] # Asi se queda con la serie temporal de cada punto
    circm<-climdexInput.raw(vv,vv,vv,tt,tt,tt, base.range=c(2071,2100))
    wsdi<-climdex.wsdi(circm)
# Algunos años tienen NA, habrá que mirarlo despacio, veo en primera vista que es primer y ultimo año, asi que hago las cuentas con cuidado para evitarlos
    wsdiavg[xx,yy]<-mean(wsdi,na.rm=TRUE)
    wsdimax[xx,yy]<-max(wsdi,na.rm=TRUE) # guardo tambien el año con el más largo
#    wsdiavg[xx,yy]<-sum(wsdi)/30.
# De momento, guardamos el promedio a 30 años del valor anual que calcula por defecto climdex.wsdi
  }
}
#image(wsdiavg), esto es para representar graficamente
# Estas instrucciones son para generar un nc con el promedio anual
#setwd("/home/kike/Escritorio/Dropbox/investigacion/tesis/tesismariamolina/olasdecaloreurocordex/calculos/")
setwd("/home/UCLM/e.sanchez/olasdecalor/calculos/resultados/")
ncdout<-args[2]
londim <- ncdim_def("lon","degrees_east",as.double(xlon)) 
latdim <- ncdim_def("lat","degrees_north",as.double(xlat))
vname1<-"wsdiavg"
vname2<-"wsdimax"
fillValue <- 1e32
wsdidef<-ncvar_def("wsdiavg","days",list(londim,latdim),fillValue,vname1,prec="single")
wsdidef2<-ncvar_def("wsdimax","days",list(londim,latdim),fillValue,vname2,prec="single")
ncsfin<-nc_create(ncdout,list(wsdidef,wsdidef2),force_v4=T)
ncvar_put(ncsfin,wsdidef,wsdiavg)
ncvar_put(ncsfin,wsdidef2,wsdimax)
ncatt_put(ncsfin,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
ncatt_put(ncsfin,"lat","axis","Y")
nc_close(ncsfin)
