### Figuras GCMs
#No están hechas las gráficas para NE, añadir en caso que sea necesario.
library("raster")
library("rasterVis")
library("graticule")
library("rgdal")
library("maps")
library("mapdata")
library("maptools")

##HWMId

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

cfich <-paste("hwmid",cfich,"box.nc",sep="")


hg_indexes<-vector("list",(ngcmstot))
for(igcm in 1:(ngcmstot)){
  hg_indexes[igcm]<-lapply(cfich[igcm], stack,varname="hwmid")
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

HWMIGCM <-vector("list",(ngcmstot))
for(igcm in 1:(ngcmstot)){
  HWMIGCM[[igcm]] <-lapply(hg_indexesmedLast30mean[[igcm]], FUN=function(x) stack(unlist(x)))
}

HWMIGCMsinmar <-vector("list",(ngcmstot))
for(igcm in 1:(ngcmstot)){
  HWMIGCMsinmar[[igcm]] <-lapply(HWMIGCM[[igcm]], FUN=function(x) mask(x, borders[[igcm]]))
}


##Figura 1

#Hago la media de todo el dominio

h_imean <-vector("list",(ngcmstot))
for(igcm in 1:(ngcmstot)){
  h_imean[[igcm]] <- lapply(hg_indexesmedLast30mean[[igcm]], FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
}

#Hago un data frame de los datos

h_df <-vector("list",(ngcmstot))
for(igcm in 1:(ngcmstot)){
  h_df[[igcm]] <- lapply(h_imean[[igcm]], as.data.frame)
}  

# Hago una matriz de los datos, de una sola columna en la que estén intercalados rcp45 y rcp85 de cada modelo

h_mdf<-merge.data.frame(h_df[[1]],h_df[[2]],by="row.names")
for(igcm in 3:(ngcmstot)){
  h_mdf <- merge.data.frame(h_mdf, h_df[[igcm]], by="row.names")
}

h_mdf <- as.matrix(h_mdf[-1]) 
for(igcm in 1:9){
  h_mdf <- as.matrix(h_mdf[-1])}

  
## Coloco los datos en el orden que los quiero representar en el barplot

modg<-rep(1:1,11)

h_M <- matrix(c(h_mdf, modg), 11, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))


hg_M1_45 <- matrix(c(h_M[[1]], h_M[[3]], h_M[[5]], h_M[[7]], h_M[[9]],h_M[[11]], modg), 6, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))
hg_M1_85 <- matrix(c(h_M[[2]], h_M[4], h_M[[6]], h_M[[8]], h_M[[10]], modg), 5, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))

hgM <- as.data.frame(h_M)
hgM45 <- as.data.frame(hg_M1_45)
hgM85 <- as.data.frame(hg_M1_85)

h_df$color <- palette[as.factor(hgM45$resolution)]


nn_44_45 <- c("CanESM", "CNRM-CM5",  "HadGEM2-ES", "IPSL-CM5A-MR", "MPI-CSC",  "EC-EARTH")

nn_44_85 <- c("CanESM", "CNRM-CM5",  "HadGEM2-ES", "IPSL-CM5A-MR", "MPI-CSC")

c45 <- c(13.07081, 8.307701,40.51319,21.62164,10.72077,7.003455 )

c85 <- c(54.43756,18.30253,146.2253,108.0441,44.29039)

sd45 <- c()
sd85 <- c()


mycols <- c("orange", "gray")
b45 <- barplot(c45, col = mycols[hgM45$resolution], names.arg = nn_44_45, las=2, ylim=c(0,300), cex.names=0.7, main="RCP4.5", ylab="HWMId", legend=(hgM85$resolution[1:2]), args.legend = list(x = 'topright'))
arrows(b45, c45-sd45, b45, c45+sd45, angle = 90, code = 3, length=0.05)

b85 <- barplot(c85, col = mycols[hgM45$resolution], names.arg = nn_44_85, las=2, ylim=c(0,300), cex.names=0.7, main="RCP8.5", ylab="HWMId")
arrows(b85, c85-sd85, b85, c85+sd85, angle = 90, code = 3, length=0.05)


## Figura 3

#Junto los gcms 8.5, cambiándolos antes todos a la misma extesión para poder hacer el stack

w <- vector("list", 6)
w[[1]] <- stack(HWMIGCMsinmar[[2]]) #CanESM
w[[2]] <- stack(HWMIGCMsinmar[[4]]) #CNRM
w[[3]] <- stack(HWMIGCMsinmar[[6]]) #MPI
w[[4]] <- stack(HWMIGCMsinmar[[8]]) #HadGEM
w[[5]] <- stack(HWMIGCMsinmar[[10]]) #IPSL
w[[6]] <- stack(HWMIGCMsinmar[[12]]) #EC-EARTH

a <- resample(w[[2]], w[[1]])
b <- resample(w[[3]], w[[1]])
c <- resample(w[[4]], w[[1]])
d <- resample(w[[5]], w[[1]])
e <- resample(w[[6]], w[[1]])

wgcm <- stack(e, c, a, b, d)

## Establezco la paleta de colores

my.at <- seq(0,20,1)
my.at <- c(my.at, 30, 40, 50, 75, 100, 125)
wgcm[wgcm[]>100] <- 100
length(wgcm[wgcm[] > 150]) <- HWMIGCMsinmar[[3]]
HWMIGCMsinmar[HWMIGCMsinmar[]>100] <- 100
HWMIGCMsinmar[HWMIGCMsinmar[]>100] <- 100
myColorkey <- list(at=my.at, ## where the colors change
                   space="bottom",
                   width= .8)


div.pal <- brewer.pal(n=9, 'Set3')

div.pal <- brewer.pal(n=9, 'RdYlGn')

levelplot(wgcm, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("HWMId"), cex=1), xlab=NULL, ylab=list(c( ""), cex=1), names.attr=c("EC-EARTH", "HadGEM2-ES", "CNRM-CM5", "MPI-ESM-LR",  "IPSL-CM5A-MR"), layout=c(5,3), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at))) + layer(sp.lines(border1, lwd=0.5))

#ww <-vector("list",6)
#ww[[1]] <- levelplot(w1, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c(""), cex=1), xlab=list(c("RCP85")), ylab=list(c(""), cex=1), names.attr=c(""), layout=c(5,3), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at)))  +     layer(sp.lines(border1, lwd=0.5))
#ww[[2]] <- levelplot(w2, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c(""), cex=1), xlab=NULL, ylab=list(c(""), cex=1), names.attr=c(""), layout=c(5,3), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at)))  +     layer(sp.lines(border1, lwd=0.5))
#ww[[3]] <- levelplot(w3, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("HWMId"), cex=1), xlab=NULL, ylab=list(c(""), cex=1), names.attr=c(""), layout=c(5,3), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at)))  +     layer(sp.lines(border1, lwd=0.5))
#ww[[4]] <- levelplot(w3, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c(""), cex=1), xlab=NULL, ylab=list(c(""), cex=1), names.attr=c(""), layout=c(5,3), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at)))  +     layer(sp.lines(border1, lwd=0.5))
#ww[[5]] <- levelplot(w5, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c(""), cex=1), xlab=NULL, ylab=list(c(""), cex=1), names.attr=c(""), layout=c(5,3), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at)))  +     layer(sp.lines(border1, lwd=0.5))
#ww[[6]] <- levelplot(w6, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c(""), cex=1), xlab=NULL, ylab=list(c(""), cex=1), names.attr=c(""), layout=c(5,3), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at)))  +     layer(sp.lines(border1, lwd=0.5))


#pdf("hwmidGCMprint2.pdf", width=15, height=7)
#print(ww[[1]], split=c(1, 1, 3, 2), more=TRUE)
#print(ww[[2]], split=c(2, 1, 3, 2), more=TRUE)
#print(ww[[3]], split=c(3, 1, 3, 2), more=TRUE)
#print(ww[[4]], split=c(1, 2, 3, 2), more=TRUE)
#print(ww[[5]], split=c(2, 2, 3, 2),more=TRUE)
#print(ww[[6]], split=c(3, 2, 3, 2))
#dev.off()
