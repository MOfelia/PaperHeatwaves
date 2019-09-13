library("raster")
library("rasterVis")
library("graticule")
library("rgdal")
library("maps")
library("mapdata")
library("maptools")

##WSDI

# 11 combinaciones de modelos (solo falta 8.5 EC-EARTH, por eso este modelo lo ponemos el último)

cmod <-c("CanESM","CNRM","HadGEM","IPSL","MPI","EC-EARTH")
crcp <-c("45","85")

#Creo un entero igual al número de archivos de entrada, que será el tamaño del vector donde ubico cada salida, 
#así no lo tengo que cambiar si cambio el número de archivos

ngcmstot <-length(cmod)*length(crcp)

cfich<-vector("character",ngcmstot)
for (imod in 1:6){
  for (ircp in 1:2){
    print(((imod-1)*2+ircp))
    cfich[((imod-1)*2+ircp)]<-paste(cmod[imod],crcp[ircp],sep="")
  }
}

cfich <-paste("wsdi",cfich,"_box.nc",sep="")


hg_indexes<-vector("list",(ngcmstot))
for(igcm in 1:(ngcmstot)){
  hg_indexes[igcm]<-lapply(cfich[igcm], stack,varname="wsdi")
}

## Función para recortar el mar

data('worldMapEnv')

fooborder <- function(data) {
  mycrs <- projection(data)
  
  ext <- as.vector(extent(projectExtent(data, CRS(mycrs))))
  
  boundaries <- map('world', fill=TRUE, exact=FALSE,
                    xlim=ext[1:2], ylim=ext[3:4],
                    plot=FALSE)
  IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
  boundaries <- map2SpatialPolygons(boundaries, IDs=IDs,
                                    proj4string=CRS(mycrs))
  border <- boundaries}

borders<-vector("list",(ngcmstot))
for(igcm in 1:(ngcmstot)){borders[[igcm]] <- fooborder(hg_indexes[[igcm]])  }

##Creo un indice temporal para hacer SetZ que me pide la función xyplot:

t <- seq(as.Date("1971-01-01"), as.Date("2100-01-01"), "year")

for(igcm in 1:(ngcmstot)){hg_indexes[[igcm]] <- lapply(hg_indexes[igcm], FUN=function(x) setZ(x, t))}


#1. Mediterráneo
# Recorto el dominio a la extensión de interés

med <- extent(-12, 40, 28, 50)

hg_indexesmed<-vector("list",(ngcmstot))
for(igcm in 1:(ngcmstot)){
  hg_indexesmed[[igcm]] <-lapply(hg_indexes[[igcm]], FUN=function(x) mask(x, borders[[igcm]]))}


hg_indexesmed2<-vector("list",(ngcmstot))
for(igcm in 1:(ngcmstot)){
  hg_indexesmed2[[igcm]] <-lapply(hg_indexesmed[[igcm]], FUN=function(x) crop(x, med))
}


##Media de los últimos 30 años MED:

hg_indexesmedLast30<-vector("list",(ngcmstot))
for(igcm in 1:(ngcmstot)){
  hg_indexesmedLast30[[igcm]] <-lapply(hg_indexesmed2[[igcm]], FUN=function(x) x[[101:130]])
}

hg_indexesmedLast30mean<-vector("list",(ngcmstot))
for(igcm in 1:(ngcmstot)){
  hg_indexesmedLast30mean[[igcm]] <-lapply(hg_indexesmedLast30[[igcm]], FUN=function(x) mean(x, na.rm=TRUE))
}

## Agrupo los ficheros para representar

WSDIGCM <-vector("list",(ngcmstot))
for(igcm in 1:(ngcmstot)){
  WSDIGCM[[igcm]] <-lapply(hg_indexesmedLast30mean[[igcm]], FUN=function(x) stack(unlist(x)))
}

WSDIGCMsinmar <-vector("list",(ngcmstot))
for(igcm in 1:(ngcmstot)){
  WSDIGCMsinmar[[igcm]] <-lapply(WSDIGCM[[igcm]], FUN=function(x) mask(x, borders[[igcm]]))
}


## Figura 3

#Junto los gcms 8.5, cambiándolos antes todos a la misma extesión para poder hacer el stack

w <- vector("list", 6)
w[[1]] <- stack(WSDIGCMsinmar[[2]]) #CanESM
w[[2]] <- stack(WSDIGCMsinmar[[4]]) #CNRM
w[[3]] <- stack(WSDIGCMsinmar[[6]]) #MPI
w[[4]] <- stack(WSDIGCMsinmar[[8]]) #HadGEM
w[[5]] <- stack(WSDIGCMsinmar[[10]]) #IPSL
w[[6]] <- stack(WSDIGCMsinmar[[12]]) #EC-EARTH

a <- resample(w[[2]], w[[1]])
b <- resample(w[[3]], w[[1]])
c <- resample(w[[4]], w[[1]])
d <- resample(w[[5]], w[[1]])
e <- resample(w[[6]], w[[1]])

wgcm <- stack(e, c, a, b, d)

## Establezco la paleta de colores

my.at <- seq(0,370, 30)

myColorkey <- list(at=my.at, ## where the colors change
                   labels=list(
                     at=my.at),
                   space="bottom",
                   width= .8)

levelplot(wgcm, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("WSDI"), cex=1), xlab=NULL, ylab=list(c( ""), cex=1), names.attr=c("EC-EARTH", "HadGEM2-ES", "CNRM-CM5", "MPI-ESM-LR",  "IPSL-CM5A-MR"), layout=c(5,4), colorkey=myColorkey) + layer(sp.lines(borders, lwd=0.5))
