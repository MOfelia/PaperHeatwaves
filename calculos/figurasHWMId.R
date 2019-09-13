###Figuras de  HWMId 

library("raster")
library("rasterVis")
library("graticule")
library("rgdal")
library("maps")
library("mapdata")
library("maptools")
library("zoo")

setwd("/lustre/UCLM/mofelia.molina/calculos/HWMId/")

lista <- dir(path="../HWMId/", pattern="^hwmid.*_adapta\\.nc$")

lista <- lapply(lista, FUN=function(x) paste("../HWMId/", x, sep=''))

ngcmstot <-length(lista)


## Hago un vector con la lista de combinaciones de GCM-RCM

#cmod <- c("CNRM-ALADIN", "CNRM-CCLM", "CNRM-RCA", "ICHEC-CCLM", "ICHEC-HIRHAM","ICHEC-RACMO", "ICHEC-RCA", "IPSL-RCA", "IPSL-WRF", "MPI-CCLM", "MPI-RCA", "MPI-REMO", "MOHC-CCLM","MOHC-HIRHAM", "MOHC-RACMO", "MOHC-RCA")
#crcp <-c("45","85")

#ngcmstot <-length(cmod)*length(crcp)

#cfich<-vector("character",ngcmstot)
#for (imod in 1:16){
#  for (ircp in 1:2){
 #   print(((imod-1)*2+ircp))
  #  cfich[((imod-1)*2+ircp)]<-paste(cmod[imod],crcp[ircp],sep="")
  #}
#}

#cfich11 <-paste("hwmid11",cfich,"_adapta.nc",sep="")
#cfich44 <-paste("hwmid44",cfich,"_adapta.nc",sep="")
#cfichtot <- list(cfich11, cfich44)

h_indexes<-vector("list",(ngcmstot))
for(ihwmi in 1:(ngcmstot)){
  h_indexes[ihwmi]<-lapply(lista[ihwmi], stack,varname="hwmid")
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
for(ihwmi in 1:(ngcmstot)){borders[[ihwmi]] <- fooborder(h_indexes[[ihwmi]])  }


##Creo un indice temporal para hacer SetZ que me pide la función xyplot (termina en 2098 por los HadGEM:

t <- seq(as.Date("1971-01-01"), as.Date("2098-12-01"), "year")

for(ihwmi in 1:(ngcmstot)){h_indexes[[ihwmi]] <- lapply(h_indexes[ihwmi], FUN=function(x) setZ(x, t))}


#1. Mediterráneo
# Recorto el dominio a la extensión de interés

med <- extent(-12, 40, 28, 50)

h_indexesmed<-vector("list",(ngcmstot))
for(ihwmi in 1:(ngcmstot)){
  h_indexesmed[[ihwmi]] <-lapply(h_indexes[[ihwmi]], FUN=function(x) mask(x, borders[[ihwmi]]))}


h_indexesmed2<-vector("list",(ngcmstot))
for(ihwmi in 1:(ngcmstot)){
  h_indexesmed2[[ihwmi]] <-lapply(h_indexesmed[[ihwmi]], FUN=function(x) crop(x, med))
}


##Media de los últimos 30 años MED:

h_indexesmedLast30<-vector("list",(ngcmstot))
for(ihwmi in 1:(ngcmstot)){
  h_indexesmedLast30[[ihwmi]] <-lapply(h_indexesmed2[[ihwmi]], FUN=function(x) x[[101:130]])
}

h_indexesmedLast30mean<-vector("list",(ngcmstot))
for(ihwmi in 1:(ngcmstot)){
  h_indexesmedLast30mean[[ihwmi]] <-lapply(h_indexesmedLast30[[ihwmi]], FUN=function(x) mean(x, na.rm=TRUE))
}

## Agrupo los ficheros para representar

HWMI <-vector("list",(ngcmstot))
for(ihwmi in 1:(ngcmstot)){
  HWMI[[ihwmi]] <-lapply(h_indexesmedLast30mean[[ihwmi]], FUN=function(x) stack(unlist(x)))
}

HWMIsinmar <-vector("list",(ngcmstot))
for(ihwmi in 1:(ngcmstot)){
  HWMIsinmar[[ihwmi]] <-lapply(HWMI[[ihwmi]], FUN=function(x) mask(x, borders[[ihwmi]]))
}



##Figura Barplot

#Hago la media de todo el dominio

h_imean <-vector("list",(ngcmstot))
for(ihwmi in 1:(ngcmstot)){
  h_imean[[ihwmi]] <- lapply(h_indexesmedLast30mean[[ihwmi]], FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
}

#Hago la desviación estándar de todo el dominio

h_sd <-vector("list",(ngcmstot))
for(ihwmi in 1:(ngcmstot)){
  h_sd[[ihwmi]] <- lapply(h_indexesmedLast30mean[[ihwmi]], FUN=function(x) cellStats(x, stat='sd', na.rm=TRUE))
}

#Hago un data frame de los datos

h_df <-vector("list",(ngcmstot))
for(ihwmi in 1:(ngcmstot)){
  h_df[[ihwmi]] <- lapply(h_imean[[ihwmi]], as.data.frame)
}  

h_sdmatrix <-vector("list",(ngcmstot))
for(ihwmi in 1:(ngcmstot)){
  h_sdmatrix[[ihwmi]] <- lapply(h_sd[[ihwmi]], as.data.frame)
} 

# Hago una matriz de los datos, de una sola columna, en la que estén intercalados rcp45 y rcp85 de cada modelo
#Media
h_mdf<-merge.data.frame(h_df[[1]],h_df[[2]],by="row.names")
for(ihwmi in 3:(ngcmstot)){
  h_mdf <- merge.data.frame(h_mdf, h_df[[ihwmi]], by="row.names")
}

h_mdf <- as.matrix(h_mdf[-1]) 
for(ihwmi in 1:55){
  h_mdf <- as.matrix(h_mdf[-1])}

#Desviación estándar
h_sdf<-merge.data.frame(h_sdmatrix[[1]],h_sdmatrix[[2]],by="row.names")
for(ihwmi in 3:(ngcmstot)){
  h_sdf <- merge.data.frame(h_sdf, h_sdmatrix[[ihwmi]], by="row.names")
}

h_sdf <- as.matrix(h_sdf[-1]) 
for(ihwmi in 1:55){
  h_sdf <- as.matrix(h_sdf[-1])}

## Coloco los datos en el orden que los quiero representar en el barplot

modg<-rep(1:1,11)
mod <- c(44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 11, 11, 11,11,11,11,11,11,11, 11,11,11,11,11,11,11,11,11,11, 11,11,11,11,11, 44, 44, 44, 44, 11, 11, 11, 11, 11, 11, 11)
mod1 <- c(44, 11,44, 11, 44, 11,44, 11, 11,44, 11,44, 11,44, 11,44, 11,44, 11,44, 11,44, 11, 44, 11, 44,11,11)
mod2 <- c(44, 11,44, 11, 44, 11,44, 11, 11,44, 11,44, 11,44, 11,44, 11,44, 11,44, 11,44, 11, 44, 11, 44,11,11,11)


h_M <- matrix(c(h_mdf, mod), 57, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))

M1_45 <- matrix(c(h_M[[32]], h_M[[3]], h_M[[34]], h_M[[1]], h_M[[36]],h_M[[5]], h_M[[38]], h_M[[9]], h_M[[7]], h_M[[40]],h_M[[11]], h_M[[42]], h_M[[13]],h_M[[44]],h_M[[15]],h_M[[46]],h_M[[17]],h_M[[52]],h_M[[26]], h_M[[50]], h_M[[28]], h_M[[56]],h_M[[30]], h_M[[48]], h_M[[22]], h_M[[50]], h_M[[24]], h_M[[19]],mod1), 28, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))

M1_85 <- matrix(c(h_M[[33]], h_M[[4]], h_M[[35]], h_M[[2]], h_M[[37]], h_M[[6]],h_M[[39]],h_M[[10]],h_M[[8]],h_M[[41]], h_M[[12]],h_M[[43]],
                  h_M[[14]],h_M[45],h_M[16],h_M[[47]],h_M[[18]],h_M[[53]], h_M[[27]],h_M[[51]], h_M[[29]], h_M[[57]],h_M[[31]],h_M[[49]], h_M[[23]], h_M[[51]],h_M[[25]],h_M[[20]], h_M[[21]], mod2), 29, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))


M <- as.data.frame(h_M)
M45 <- as.data.frame(M1_45)
M85 <- as.data.frame(M1_85)

## Matriz de la desviación estándar

Msd <- matrix(c(h_sdf, mod), 57, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))


Msd1_45 <- matrix(c(Msd[[32]], Msd[[3]], Msd[[34]], Msd[[1]], Msd[[36]],Msd[[5]], Msd[[38]], Msd[[9]], Msd[[7]], Msd[[40]],Msd[[11]], Msd[[42]], Msd[[13]],Msd[[44]],Msd[[15]],Msd[[46]],Msd[[17]],Msd[[52]],Msd[[26]], Msd[[50]], Msd[[28]], Msd[[56]],Msd[[30]], Msd[[48]], Msd[[22]], Msd[[50]], Msd[[24]], Msd[[19]],mod1), 28, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))

Msd1_85 <- matrix(c(Msd[[33]], Msd[[4]], Msd[[35]], Msd[[2]], Msd[[37]], Msd[[6]],Msd[[39]],Msd[[10]],Msd[[8]],Msd[[41]], Msd[[12]],Msd[[43]],
                    Msd[[14]],Msd[45],Msd[16],Msd[[47]],Msd[[18]],Msd[[53]], Msd[[27]],Msd[[51]], Msd[[29]], Msd[[57]],Msd[[31]],Msd[[49]], Msd[[23]], Msd[[51]],Msd[[25]],Msd[[20]], Msd[[21]], mod2), 29, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))

Msd <- as.data.frame(Msd)
Msd45 <- as.data.frame(Msd1_45)
Msd85 <- as.data.frame(Msd1_85)


nn_44_45 <- c("CanESM-RCA", "CNRM-CCLM","CNRM-ALADIN", "CNRM-ALADIN", "CNRM-RCA", "CNRM-RCA", "ECEARTH-HIRHAM", "ECEARTH-HIRHAM", "ECEARTH-CCLM", "ECEARTH-RAC", "ECEARTH-RAC", "ECEARTH-RCA", "ECEARTH-RCA", "IPSL-RCA", "IPSL-RCA", "IPSL-WRF",  "IPSL-WRF", "MPI-CCLM","MPI-CCLM", "MPI-RCA", "MPI-RCA", "MPI-REMO", "MPI-REMO", "HadGEM-RAC", "HadGEM-RAC","HadGEM-RCA", "HadGEM-RCA", "HadGEM-CCLM")

nn_44_85 <- c("CanESM-RCA", "CNRM-CCLM","CNRM-ALADIN", "CNRM-ALADIN", "CNRM-RCA", "CNRM-RCA", "ECEARTH-HIRHAM", "ECEARTH-HIRHAM", "ECEARTH-CCLM", "ECEARTH-RAC", "ECEARTH-RAC", "ECEARTH-RCA", "ECEARTH-RCA", "IPSL-RCA", "IPSL-RCA", "IPSL-WRF",  "IPSL-WRF", "MPI-CCLM","MPI-CCLM", "MPI-RCA", "MPI-RCA", "MPI-REMO", "MPI-REMO", "HadGEM-RAC", "HadGEM-RAC","HadGEM-RCA", "HadGEM-RCA", "HadGEM-CCLM", "HadGEM-HIRHAM")

##Hago un vector con la media de cada modelo

c45 <- c(16.06598,6.254013,7.444083,8.019021,6.045284,6.980263,6.526587,5.984371
         ,        6.492412,6.017233,5.630969,8.167413,8.690041,16.00991,17.65458,14.12973
         ,        15.61324,9.168062,7.879629,9.665896,10.50521,9.218978,8.483061,19.73375
         ,        18.12334,22.68577,25.44299,18.81233)
c85 <- c(71.91478,13.88244,18.28386,20.31296,16.4474,18.80206,22.10038,21.54084
         ,        22.4485,20.85873,18.18016,29.18743,31.76524,71.94312,78.52964,61.68495
         ,        66.23563,34.47505,27.17715,37.88862,41.77478,27.17715,33.21821,77.74431
         ,        70.04025,93.91026,97.57647,66.5776, 59.84622)

sd45 <- c(12.70001,4.247496,4.948201,9.66553, 4.73963,4.892208,3.846831,5.399725
          ,         3.971265,4.05336,5.024716,5.63489,5.668432,15.06901,19.04927,8.302005
          ,         9.015321,5.949546,4.927282,6.071989,6.570455,5.050078,4.426279,19.801, 
          20.09724,24.73572,29.32821,17.97)
sd85 <- c(61.77582,10.82161,12.98633,21.52821,14.89199,15.10539,18.97251,19.65532
          ,         19.16631,18.9133,18.22466,25.37124,25.53737,71.41264,83.72191,55.96959
          ,         59.54299, 36.68057,29.97107,38.12777,41.82772,29.97107,26.98937,79.81722
          ,         80.88489,110.4208,116.3024,67.2004, 78.81022)


mycols <- c("lightgreen", "gray")
b45 <- barplot(c45, col = mycols[M45$resolution], names.arg = nn_44_45, las=2, ylim=c(0,250), cex.names=0.7, main="RCP4.5", ylab="HWMId", legend=(M85$resolution[1:2]), args.legend = list(x = 'topright'))
arrows(b45, c45-sd45, b45, c45+sd45, angle = 90, code = 3, length=0.05)

b85 <- barplot(c85, col = mycols[M85$resolution], names.arg = nn_44_85, las=2, ylim=c(0,250), cex.names=0.7, main="RCP8.5", ylab="HWMId")
arrows(b85, c85-sd85, b85, c85+sd85, angle = 90, code = 3, length=0.05)


## Figura GCM vs RCM

w <- vector("list", 16) 
w[[1]] <- stack(HWMIsinmar[[2]]) #CanESM
w[[2]] <- stack(HWMIsinmar[[4]]) #CNRM
w[[3]] <- stack(HWMIsinmar[[6]]) #MPI
w[[4]] <- stack(HWMIsinmar[[8]]) #HadGEM
w[[5]] <- stack(HWMIsinmar[[10]]) #IPSL
w[[6]] <- stack(HWMIsinmar[[12]]) #EC-EARTH

w

### Series temporales HWMId 0.44. Están hechas con el archivo global, no MED ni NE


c_11_85 <- list("hwmid11CNRM-ALADIN85_adapta_runmean.nc", "hwmid11CNRM-CCLM85_adapta_runmean.nc", "hwmid11CNRM-RCA85_adapta_runmean.nc", "hwmid11ICHEC-CCLM85_adapta_runmean.nc", "hwmid11ICHEC-HIRHAM85_adapta_runmean.nc", "hwmid11ICHEC-RACMO85_adapta_runmean.nc", "hwmid11ICHEC-RCA85_adapta_runmean.nc", "hwmid11IPSL-RCA85_adapta_runmean.nc", "hwmid11IPSL-WRF85_adapta_runmean.nc",  "hwmid11MPI-CCLM85_adapta_runmean.nc",  "hwmid11MPI-RCA85_adapta_runmean.nc",  "hwmid11MPI-REMO85_adapta_runmean.nc")
cm_11_85 <- list("hwmid11MOHC-RACMO85_adapta_runmean.nc", "hwmid11MOHC-RCA85_adapta_runmean.nc", "hwmid11MOHC-CCLM85_adapta_runmean.nc", "hwmid11MOHC-HIRHAM85_adapta_runmean.nc")


##Abro los archivos con stack:

h_indexes_11_85 <- lapply(c_11_85, stack, varname="hwmid")
hm_indexes_11_85 <- lapply(cm_11_85, stack)

## creo un vector con los nombres de las capas.

nn_11_85 <- c("CNRM-CM5-ALADIN", "CNRM-CM5-CCLM", "CNRM-CM5-RCA", "ICHEC-CCLM", "ICHEC-HIRHAM", "ICHEC-RACMO", "ICHEC-RCA", "IPSL-RCA", "IPSL-WRF", "MPI-CCLM", "MPI-RCA", "MPI-REMO")
nn_11_85m <- c("HadGEM-RACMO", "HadGEM-RCA", "HadGEM-CCLM","HadGEM-HIRHAM")

##Añadimos las líneas de costa:
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

border_11_85 <- fooborder(h_indexes_11_85[[1]])
border_11_85m <- fooborder(hm_indexes_11_85[[1]])

##Creo un indice temporal para hacer SetZ que me pide la función xyplot: es más pequeño porque son las medias móviles, 126 años

idx <- seq(as.Date("1973-01-01"), as.Date("2098-01-01"), "year")
idxm <- seq(as.Date("1973-01-01"), as.Date("2097-12-01"), "year")

h_indexes_11_85 <- lapply(h_indexes_11_85, FUN=function(x) setZ(x, idx))
hm_indexes_11_85 <- lapply(hm_indexes_11_85, FUN=function(x) setZ(x, idxm))

#Selecciono el periodo de interés para que coincidan los dos ficheros:

h_indexes_11_85 <- lapply(h_indexes_11_85, FUN=function(x) x[[1:125]])


#Selecciono zonas

e1 <- extent(-12, 0, 36, 38)
e2 <- extent(-12, 5, 38, 44)
e3 <- extent(-12, 7, 44, 50)
e4 <- extent(7, 19, 44, 50)
e5 <- extent(-12, 0, 28, 36)
e6 <- extent(12, 40, 28, 36)
e7 <- extent(19, 40, 44, 50)
e8 <- extent(5, 12, 38, 44)
e9 <- extent(12, 19, 36, 44)
e10 <- extent(0, 12, 28, 38)
e11 <- extent(19, 40, 36, 44)

#Aprovechamos las regiones para hacer el dibujo de cada una de ellas en un mapa
##Spatialpolygons

sp1 <- as(e1, "SpatialPolygons")
sp2 <- as(e2, "SpatialPolygons")
sp3 <- as(e3, "SpatialPolygons")
sp4 <- as(e4, "SpatialPolygons")
sp5 <- as(e5, "SpatialPolygons")
sp6 <- as(e6, "SpatialPolygons")
sp7 <- as(e7, "SpatialPolygons")
sp8 <- as(e8, "SpatialPolygons")
sp9 <- as(e9, "SpatialPolygons")
sp10 <- as(e10, "SpatialPolygons")
sp11 <- as(e11, "SpatialPolygons")
spT <- sp1+sp2+sp3+sp4+sp5+sp6+sp7+sp8+sp9+sp10+sp11

library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-12, 40), ylim = c(28, 50), asp = 1, lwd= 0.4)
spatial.polys = SpatialPolygons(list(polys))

plot(sp1, col="transparent", bor="transparent", cex=2.5, add=TRUE)
plot(sp2, col="transparent", bor="transparent",cex=2.5,add=TRUE)
plot(sp3, col="transparent", cex=2.5, add=TRUE)
plot(sp4, col="transparent", cex=2.5, add=TRUE)
plot(sp5, col="transparent", cex=2.5,add=TRUE)
plot(sp6, col="transparent", cex=2.5,add=TRUE)
plot(sp7, col="transparent", cex=2.5,add=TRUE)
plot(sp8, col="transparent", bor="transparent", cex=2.5,add=TRUE)
plot(sp9, col="transparent", bor="transparent", cex=2.5,add=TRUE)
plot(sp10, col="transparent", cex=2.5, add=TRUE)
plot(sp11, col="transparent", cex=2.5, add=TRUE)
line<- as(extent(5,5,38,44), "SpatialLines")
plot(line, cex=2.5, add=TRUE)
line<- as(extent(-12,-12,36,44), "SpatialLines")
plot(line, cex=2.5, add=TRUE)

##Volvemos a las series temporales


## 1. PI
e1 <- e1 + e2
#ext1<- c(-12, 0, 36, 38)
#ext2<- c(-12, 5, 38, 44)
#ext <- cbind(ext1, ext2)
ext1 <- sp1 + sp2

piseries <- lapply(h_indexes_11_85, FUN=function(x) mask(x, border_11_85))
piseriesm <- lapply(hm_indexes_11_85, FUN=function(x) mask(x, border_11_85m))

piseries <- lapply(piseries, FUN=function(x) crop(x, ext1))
piseriesm <- lapply(piseriesm, FUN=function(x) crop(x, ext1))

piseries <- lapply(piseries, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
piseriesm <- lapply(piseriesm, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

pimatrix <- do.call(cbind, piseries)
pimatrixm <- do.call(cbind, piseriesm)


library(zoo)
z <- as.zoo(pimatrix)
zm <- as.zoo(pimatrixm)

library(reshape2)
pimatrix <- as.data.frame(pimatrix)
pimatrixm <- as.data.frame(pimatrixm)

rows<- c(1973:2097)
colnames(pimatrix) <- nn_11_85
colnames(pimatrixm) <- nn_11_85m
rownames(pimatrix) <- rows
rownames(pimatrixm) <- rows

pimatrixt <- merge(pimatrix, pimatrixm, by="row.names", sort=FALSE)
pimatrixt <- as.data.frame(pimatrixt[-1])

pimatrix <- as.zoo(pimatrixt)



myTheme <- custom.theme(pch = 20, cex = 0.7, alpha=0.7, symbol = brewer.pal(12, "Paired"), fill = brewer.pal(12, "Paired"))
myTheme$strip.background$col <- 'transparent'
myTheme$strip.shingle$col <- 'transparent'

pi <- xyplot(pimatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,150),main=list("PI",cex=0.8), xlab=NULL,
             auto.key = FALSE, scales=list(
               x=list(labels=NULL)))

## para colocar leyenda:
auto.key = list(corner=c(0.95, 1), x = 0.8, y = 0.93)

## 2. FR

frseries <- lapply(h_indexes_11_85, FUN=function(x) mask(x, border_11_85))
frseriesm <- lapply(hm_indexes_11_85, FUN=function(x) mask(x, border_11_85m))

frseries <- lapply(frseries, FUN=function(x) crop(x, e3))
frseriesm <- lapply(frseriesm, FUN=function(x) crop(x, e3))

frseries <- lapply(frseries, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
frseriesm <- lapply(frseriesm, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

frmatrix <- do.call(cbind, frseries)
frmatrixm <- do.call(cbind, frseriesm)


library(zoo)
z <- as.zoo(frmatrix)
zm <- as.zoo(frmatrixm)

library(reshape2)
frmatrix <- as.data.frame(frmatrix)
frmatrixm <- as.data.frame(frmatrixm)

rows<- c(1, 2, 3, 4,5, 6,7, 8, 9,10, 11,12, 13,14, 15,  16, 17,18,19,20, 21,22,23,24,25,26, 27, 28, 29,30, 31,32,33, 34,35,36,37,38, 39,  40,41,42,43, 44,45, 46, 47, 48,49, 50,51, 52,  53, 54,55, 56, 57,58,59, 60, 61,62, 63, 64, 65, 66,  67, 68,69,70,71, 72,73,74, 75, 76,77,78,79,  80, 81, 82, 83, 84,  85, 86, 87, 88,  89,  90, 91, 92, 93,  94,  95,  96, 97, 98, 99, 100,101,102,103,104,105,106,107, 108, 109,110,111,112,113,114, 115,116, 117,118,119,120,121,122,123,124,125)
colnames(frmatrix) <- nn_11_85
colnames(frmatrixm) <- nn_11_85m
rownames(frmatrix) <- rows
rownames(frmatrixm) <- rows

frmatrixt <- merge(frmatrix, frmatrixm, by="row.names", sort=FALSE)
frmatrixt <- as.data.frame(frmatrixt[-1])

frmatrix <- as.zoo(frmatrixt)

fr <- xyplot(frmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,150), main=list("FR",cex=0.8), xlab=NULL,
             auto.key =list(corner=c(1, 1), x = 0.6, y = 0.95, cex=0.4), scales=list(
               x=list(labels=NULL)), lattice.options = list(panel.error = "warning"))

## 3. ALPS

alpsseries <- lapply(h_indexes_11_85, FUN=function(x) mask(x, border_11_85))
alpsseriesm <- lapply(hm_indexes_11_85, FUN=function(x) mask(x, border_11_85m))

alpsseries <- lapply(alpsseries, FUN=function(x) crop(x, e4))
alpsseriesm <- lapply(alpsseriesm, FUN=function(x) crop(x, e4))

alpsseries <- lapply(alpsseries, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
alpsseriesm <- lapply(alpsseriesm, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

alpsmatrix <- do.call(cbind, alpsseries)
alpsmatrixm <- do.call(cbind, alpsseriesm)


library(zoo)
z <- as.zoo(alpsmatrix)
zm <- as.zoo(alpsmatrixm)

library(reshape2)
alpsmatrix <- as.data.frame(alpsmatrix)
alpsmatrixm <- as.data.frame(alpsmatrixm)

rows<- c(1, 2, 3, 4,5, 6,7, 8, 9,10, 11,12, 13,14, 15,  16, 17,18,19,20, 21,22,23,24,25,26, 27, 28, 29,30, 31,32,33, 34,35,36,37,38, 39,  40,41,42,43, 44,45, 46, 47, 48,49, 50,51, 52,  53, 54,55, 56, 57,58,59, 60, 61,62, 63, 64, 65, 66,  67, 68,69,70,71, 72,73,74, 75, 76,77,78,79,  80, 81, 82, 83, 84,  85, 86, 87, 88,  89,  90, 91, 92, 93,  94,  95,  96, 97, 98, 99, 100,101,102,103,104,105,106,107, 108, 109,110,111,112,113,114, 115,116, 117,118,119,120,121,122,123,124,125)
colnames(alpsmatrix) <- nn_11_85
colnames(alpsmatrixm) <- nn_11_85m
rownames(alpsmatrix) <- rows
rownames(alpsmatrixm) <- rows

alpsmatrixt <- merge(alpsmatrix, alpsmatrixm, by="row.names", sort=FALSE)
alpsmatrixt <- as.data.frame(alpsmatrixt[-1])

alpsmatrix <- as.zoo(alpsmatrixt)

alps <- xyplot(alpsmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme,ylim=c(0,150),main=list("ALPS",cex=0.8), xlab=NULL, ylab=NULL,
               auto.key = FALSE, scales=list(
                 x=list(labels=NULL), y=list(labels=NULL)), lattice.options = list(panel.error = "warning"))



## 4.WAFR

wafrseries <- lapply(h_indexes_11_85, FUN=function(x) mask(x, border_11_85))
wafrseriesm <- lapply(hm_indexes_11_85, FUN=function(x) mask(x, border_11_85m))

wafrseries <- lapply(wafrseries, FUN=function(x) crop(x, e5))
wafrseriesm <- lapply(wafrseriesm, FUN=function(x) crop(x, e5))

wafrseries <- lapply(wafrseries, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
wafrseriesm <- lapply(wafrseriesm, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

wafrmatrix <- do.call(cbind, wafrseries)
wafrmatrixm <- do.call(cbind, wafrseriesm)


library(zoo)
z <- as.zoo(wafrmatrix)
zm <- as.zoo(wafrmatrixm)

library(reshape2)
wafrmatrix <- as.data.frame(wafrmatrix)
wafrmatrixm <- as.data.frame(wafrmatrixm)

rows<- c(1, 2, 3, 4,5, 6,7, 8, 9,10, 11,12, 13,14, 15,  16, 17,18,19,20, 21,22,23,24,25,26, 27, 28, 29,30, 31,32,33, 34,35,36,37,38, 39,  40,41,42,43, 44,45, 46, 47, 48,49, 50,51, 52,  53, 54,55, 56, 57,58,59, 60, 61,62, 63, 64, 65, 66,  67, 68,69,70,71, 72,73,74, 75, 76,77,78,79,  80, 81, 82, 83, 84,  85, 86, 87, 88,  89,  90, 91, 92, 93,  94,  95,  96, 97, 98, 99, 100,101,102,103,104,105,106,107, 108, 109,110,111,112,113,114, 115,116, 117,118,119,120,121,122,123,124,125)
colnames(wafrmatrix) <- nn_11_85
colnames(wafrmatrixm) <- nn_11_85m
rownames(wafrmatrix) <- rows
rownames(wafrmatrixm) <- rows

wafrmatrixt <- merge(wafrmatrix, wafrmatrixm, by="row.names", sort=FALSE)
wafrmatrixt <- as.data.frame(wafrmatrixt[-1])

wafrmatrix <- as.zoo(wafrmatrixt)

wafr <- xyplot(wafrmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme,ylim=c(0,450),main=list("WAFR",cex=0.8),
               auto.key = FALSE, scales=list(
                 x=list(
                   at=seq(1,125,25),
                   labels=c("1973","1998","2023", "2048", "2073") )), lattice.options = list(panel.error = "warning"))


## 5. EAFR

eafrseries <- lapply(h_indexes_11_85, FUN=function(x) mask(x, border_11_85))
eafrseriesm <- lapply(hm_indexes_11_85, FUN=function(x) mask(x, border_11_85m))

eafrseries <- lapply(eafrseries, FUN=function(x) crop(x, e6))
eafrseriesm <- lapply(eafrseriesm, FUN=function(x) crop(x, e6))

eafrseries <- lapply(eafrseries, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
eafrseriesm <- lapply(eafrseriesm, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

eafrmatrix <- do.call(cbind, eafrseries)
eafrmatrixm <- do.call(cbind, eafrseriesm)


library(zoo)
z <- as.zoo(eafrmatrix)
zm <- as.zoo(eafrmatrixm)

library(reshape2)
eafrmatrix <- as.data.frame(eafrmatrix)
eafrmatrixm <- as.data.frame(eafrmatrixm)

rows<- c(1, 2, 3, 4,5, 6,7, 8, 9,10, 11,12, 13,14, 15,  16, 17,18,19,20, 21,22,23,24,25,26, 27, 28, 29,30, 31,32,33, 34,35,36,37,38, 39,  40,41,42,43, 44,45, 46, 47, 48,49, 50,51, 52,  53, 54,55, 56, 57,58,59, 60, 61,62, 63, 64, 65, 66,  67, 68,69,70,71, 72,73,74, 75, 76,77,78,79,  80, 81, 82, 83, 84,  85, 86, 87, 88,  89,  90, 91, 92, 93,  94,  95,  96, 97, 98, 99, 100,101,102,103,104,105,106,107, 108, 109,110,111,112,113,114, 115,116, 117,118,119,120,121,122,123,124,125)
colnames(eafrmatrix) <- nn_11_85
colnames(eafrmatrixm) <- nn_11_85m
rownames(eafrmatrix) <- rows
rownames(eafrmatrixm) <- rows

eafrmatrixt <- merge(eafrmatrix, eafrmatrixm, by="row.names", sort=FALSE)
eafrmatrixt <- as.data.frame(eafrmatrixt[-1])

eafrmatrix <- as.zoo(eafrmatrixt)


eafr <- xyplot(eafrmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,450),main=list("EAFR",cex=0.8),
               auto.key = FALSE, scales=list(
                 x=list(at=seq(1,125,25),
                        labels=c("1973","1998","2023", "2048", "2073") ), y=list(labels=NULL)), lattice.options = list(panel.error = "warning"))

##NEAST

neastseries <- lapply(h_indexes_11_85, FUN=function(x) mask(x, border_11_85))
neastseriesm <- lapply(hm_indexes_11_85, FUN=function(x) mask(x, border_11_85m))

neastseries <- lapply(neastseries, FUN=function(x) crop(x, e7))
neastseriesm <- lapply(neastseriesm, FUN=function(x) crop(x, e7))

neastseries <- lapply(neastseries, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
neastseriesm <- lapply(neastseriesm, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

neastmatrix <- do.call(cbind, neastseries)
neastmatrixm <- do.call(cbind, neastseriesm)


library(zoo)
z <- as.zoo(neastmatrix)
zm <- as.zoo(neastmatrixm)

library(reshape2)
neastmatrix <- as.data.frame(neastmatrix)
neastmatrixm <- as.data.frame(neastmatrixm)

rows<- c(1, 2, 3, 4,5, 6,7, 8, 9,10, 11,12, 13,14, 15,  16, 17,18,19,20, 21,22,23,24,25,26, 27, 28, 29,30, 31,32,33, 34,35,36,37,38, 39,  40,41,42,43, 44,45, 46, 47, 48,49, 50,51, 52,  53, 54,55, 56, 57,58,59, 60, 61,62, 63, 64, 65, 66,  67, 68,69,70,71, 72,73,74, 75, 76,77,78,79,  80, 81, 82, 83, 84,  85, 86, 87, 88,  89,  90, 91, 92, 93,  94,  95,  96, 97, 98, 99, 100,101,102,103,104,105,106,107, 108, 109,110,111,112,113,114, 115,116, 117,118,119,120,121,122,123,124,125)
colnames(neastmatrix) <- nn_11_85
colnames(neastmatrixm) <- nn_11_85m
rownames(neastmatrix) <- rows
rownames(neastmatrixm) <- rows

neastmatrixt <- merge(neastmatrix, neastmatrixm, by="row.names", sort=FALSE)
neastmatrixt <- as.data.frame(neastmatrixt[-1])

neastmatrix <- as.zoo(neastmatrixt)

neast <- xyplot(neastmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,150),main=list("NEAST", cex=0.8), xlab=NULL,
                auto.key = FALSE, scales=list(
                  x=list(labels=NULL), y=list(labels=NULL)), lattice.options = list(panel.error = "warning"))


##CMED

e8 <- e8 + e9
ext8 <- sp8 + sp9
ext8

cmedseries <- lapply(h_indexes_11_85, FUN=function(x) mask(x, border_11_85))
cmedseriesm <- lapply(hm_indexes_11_85, FUN=function(x) mask(x, border_11_85m))

cmedseries <- lapply(cmedseries, FUN=function(x) crop(x, ext8))
cmedseriesm <- lapply(cmedseriesm, FUN=function(x) crop(x, ext8))

cmedseries <- lapply(cmedseries, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
cmedseriesm <- lapply(cmedseriesm, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

cmedmatrix <- do.call(cbind, cmedseries)
cmedmatrixm <- do.call(cbind, cmedseriesm)


library(zoo)
z <- as.zoo(cmedmatrix)
zm <- as.zoo(cmedmatrixm)

library(reshape2)
cmedmatrix <- as.data.frame(cmedmatrix)
cmedmatrixm <- as.data.frame(cmedmatrixm)

rows<- c(1, 2, 3, 4,5, 6,7, 8, 9,10, 11,12, 13,14, 15,  16, 17,18,19,20, 21,22,23,24,25,26, 27, 28, 29,30, 31,32,33, 34,35,36,37,38, 39,  40,41,42,43, 44,45, 46, 47, 48,49, 50,51, 52,  53, 54,55, 56, 57,58,59, 60, 61,62, 63, 64, 65, 66,  67, 68,69,70,71, 72,73,74, 75, 76,77,78,79,  80, 81, 82, 83, 84,  85, 86, 87, 88,  89,  90, 91, 92, 93,  94,  95,  96, 97, 98, 99, 100,101,102,103,104,105,106,107, 108, 109,110,111,112,113,114, 115,116, 117,118,119,120,121,122,123,124,125)
colnames(cmedmatrix) <- nn_11_85
colnames(cmedmatrixm) <- nn_11_85m
rownames(cmedmatrix) <- rows
rownames(cmedmatrixm) <- rows

cmedmatrixt <- merge(cmedmatrix, cmedmatrixm, by="row.names", sort=FALSE)
cmedmatrixt <- as.data.frame(cmedmatrixt[-1])

cmedmatrix <- as.zoo(cmedmatrixt)

cmed <- xyplot(cmedmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,150),main=list("CMED",cex=0.8), xlab=NULL,
               auto.key = FALSE, scales=list(
                 x=list(labels=NULL), y=list(labels=NULL)), lattice.options = list(panel.error = "warning"))


##CAFR

cafrseries <- lapply(h_indexes_11_85, FUN=function(x) mask(x, border_11_85))
cafrseriesm <- lapply(hm_indexes_11_85, FUN=function(x) mask(x, border_11_85m))

cafrseries <- lapply(cafrseries, FUN=function(x) crop(x, e10))
cafrseriesm <- lapply(cafrseriesm, FUN=function(x) crop(x, e10))

cafrseries <- lapply(cafrseries, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
cafrseriesm <- lapply(cafrseriesm, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

cafrmatrix <- do.call(cbind, cafrseries)
cafrmatrixm <- do.call(cbind, cafrseriesm)


library(zoo)
z <- as.zoo(cafrmatrix)
zm <- as.zoo(cafrmatrixm)

library(reshape2)
cafrmatrix <- as.data.frame(cafrmatrix)
cafrmatrixm <- as.data.frame(cafrmatrixm)

rows<- c(1, 2, 3, 4,5, 6,7, 8, 9,10, 11,12, 13,14, 15,  16, 17,18,19,20, 21,22,23,24,25,26, 27, 28, 29,30, 31,32,33, 34,35,36,37,38, 39,  40,41,42,43, 44,45, 46, 47, 48,49, 50,51, 52,  53, 54,55, 56, 57,58,59, 60, 61,62, 63, 64, 65, 66,  67, 68,69,70,71, 72,73,74, 75, 76,77,78,79,  80, 81, 82, 83, 84,  85, 86, 87, 88,  89,  90, 91, 92, 93,  94,  95,  96, 97, 98, 99, 100,101,102,103,104,105,106,107, 108, 109,110,111,112,113,114, 115,116, 117,118,119,120,121,122,123,124,125)
colnames(cafrmatrix) <- nn_11_85
colnames(cafrmatrixm) <- nn_11_85m
rownames(cafrmatrix) <- rows
rownames(cafrmatrixm) <- rows

cafrmatrixt <- merge(cafrmatrix, cafrmatrixm, by="row.names", sort=FALSE)
cafrmatrixt <- as.data.frame(cafrmatrixt[-1])

cafrmatrix <- as.zoo(cafrmatrixt)

cafr <- xyplot(cafrmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,450),main=list("CAFR",cex=0.8),
               auto.key = FALSE, scales=list(
                 x=list(
                   at=seq(1,125,25),
                   labels=c("1973","1998","2023", "2048", "2073") )), lattice.options = list(panel.error = "warning"))


##EAST

eastseries <- lapply(h_indexes_11_85, FUN=function(x) mask(x, border_11_85))
eastseriesm <- lapply(hm_indexes_11_85, FUN=function(x) mask(x, border_11_85m))

eastseries <- lapply(eastseries, FUN=function(x) crop(x, e11))
eastseriesm <- lapply(eastseriesm, FUN=function(x) crop(x, e11))

eastseries <- lapply(eastseries, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
eastseriesm <- lapply(eastseriesm, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

eastmatrix <- do.call(cbind, eastseries)
eastmatrixm <- do.call(cbind, eastseriesm)


library(zoo)
z <- as.zoo(eastmatrix)
zm <- as.zoo(eastmatrixm)

library(reshape2)
eastmatrix <- as.data.frame(eastmatrix)
eastmatrixm <- as.data.frame(eastmatrixm)

rows<- c(1, 2, 3, 4,5, 6,7, 8, 9,10, 11,12, 13,14, 15,  16, 17,18,19,20, 21,22,23,24,25,26, 27, 28, 29,30, 31,32,33, 34,35,36,37,38, 39,  40,41,42,43, 44,45, 46, 47, 48,49, 50,51, 52,  53, 54,55, 56, 57,58,59, 60, 61,62, 63, 64, 65, 66,  67, 68,69,70,71, 72,73,74, 75, 76,77,78,79,  80, 81, 82, 83, 84,  85, 86, 87, 88,  89,  90, 91, 92, 93,  94,  95,  96, 97, 98, 99, 100,101,102,103,104,105,106,107, 108, 109,110,111,112,113,114, 115,116, 117,118,119,120,121,122,123,124,125)
colnames(eastmatrix) <- nn_11_85
colnames(eastmatrixm) <- nn_11_85m
rownames(eastmatrix) <- rows
rownames(eastmatrixm) <- rows

eastmatrixt <- merge(eastmatrix, eastmatrixm, by="row.names", sort=FALSE)
eastmatrixt <- as.data.frame(eastmatrixt[-1])

eastmatrix <- as.zoo(eastmatrixt)

east <- xyplot(eastmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,150),main=list("EAST",cex=0.8), xlab=NULL,
               auto.key = FALSE, scales=list(
                 x=list(labels=NULL), y=list(labels=NULL)), lattice.options = list(panel.error = "warning"))


## Represento todas juntas

pdf("printseriesHWMId.pdf", width=10, height=10)
print(fr, split=c(1, 1, 3, 3), more=TRUE)
print(alps, split=c(2, 1, 3, 3), more=TRUE)
print(neast, split=c(3, 1, 3, 3), more=TRUE)
print(pi, split=c(1, 2, 3, 3), more=TRUE)
print(cmed, split=c(2, 2, 3, 3), more=TRUE)
print(east, split=c(3, 2, 3, 3), more=TRUE)
print(wafr, split=c(1, 3, 3, 3), more=TRUE)
print(cafr, split=c(2, 3, 3, 3), more=TRUE)
print(eafr, split=c(3, 3, 3, 3))
dev.off()




## Series temporales Optimización del código 

# Selecciono sólo los ficheros EUR11, rcp85
setwd("/lustre/UCLM/mofelia.molina/calculos/HWMId")

lista1 <- dir(path="../HWMId/", pattern="^hwmid11.*85_adapta_runmean\\.nc$")

lista <- lapply(lista1, FUN=function(x) paste("../taller/", x, sep=''))

ngcmstot <-length(lista)

h_indexes<-vector("list",(ngcmstot))
for(ihwmi in 1:(ngcmstot)){
  h_indexes[ihwmi]<-lapply(lista[ihwmi], stack,varname="hwmid")
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
for(ihwmi in 1:(ngcmstot)){borders[[ihwmi]] <- fooborder(h_indexes[[ihwmi]])  }

##Creo un indice temporal para hacer SetZ que me pide la función xyplot: es más pequeño porque son las medias móviles, 126 años

idx <- seq(as.Date("1973-01-01"), as.Date("2098-01-01"), "year")
idxm <- seq(as.Date("1973-01-01"), as.Date("2097-12-01"), "year") #10,11,12,13

for(ihwmi in 1:9){h_indexes[ihwmi] <- lapply(h_indexes[ihwmi], FUN=function(x) setZ(x, idx))}
for(ihwmi in 10:13){h_indexes[ihwmi] <- lapply(h_indexes[ihwmi], FUN=function(x) setZ(x, idxm))}
for(ihwmi in 14:16){h_indexes[ihwmi] <- lapply(h_indexes[ihwmi], FUN=function(x) setZ(x, idx))}



#Selecciono el periodo de interés para que coincidan los dos ficheros:

for(ihwmi in 1:(ngcmstot)){h_indexes[[ihwmi]] <- lapply(h_indexes[ihwmi], FUN=function(x) x[[1:125]])}


#Selecciono zonas

e <- c("-12, 0, 36, 38", "-12, 5, 38, 44", "-12, 7, 44, 50", "7, 19, 44, 50", "-12, 0, 28, 36", "12, 40, 28, 36",
       "19, 40, 44, 50", "5, 12, 38, 44", "12, 19, 36, 44", "0, 12, 28, 38", "19, 40, 36, 44")

ne <-length(e)

e <- as.vector(paste("(", e , ")", sep=))


e1 <- extent(-12, 0, 36, 38)
e2 <- extent(-12, 5, 38, 44)
e3 <- extent(-12, 7, 44, 50)
e4 <- extent(7, 19, 44, 50)
e5 <- extent(-12, 0, 28, 36)
e6 <- extent(12, 40, 28, 36)
e7 <- extent(19, 40, 44, 50)
e8 <- extent(5, 12, 38, 44)
e9 <- extent(12, 19, 36, 44)
e10 <- extent(0, 12, 28, 38)
e11 <- extent(19, 40, 36, 44)

##Spatialpolygons

sp1 <- as(e1, "SpatialPolygons")
sp2 <- as(e2, "SpatialPolygons")
sp3 <- as(e3, "SpatialPolygons")
sp4 <- as(e4, "SpatialPolygons")
sp5 <- as(e5, "SpatialPolygons")
sp6 <- as(e6, "SpatialPolygons")
sp7 <- as(e7, "SpatialPolygons")
sp8 <- as(e8, "SpatialPolygons")
sp9 <- as(e9, "SpatialPolygons")
sp10 <- as(e10, "SpatialPolygons")
sp11 <- as(e11, "SpatialPolygons")
spT <- sp1+sp2+sp3+sp4+sp5+sp6+sp7+sp8+sp9+sp10+sp11


## 1. PI

ext1 <- sp1 + sp2

piseries <- vector("list",(ngcmstot))
for(ihwmi in 1:(ngcmstot)){
  piseries[[ihwmi]]<-lapply(h_indexes[[ihwmi]], FUN=function(x) mask(x, borders[[ihwmi]]))
}


piseries <- vector("list",(ngcmstot))
for(ihwmi in 1:(ngcmstot)){
  piseries[[ihwmi]]<-lapply(piseries[[ihwmi]], FUN=function(x) crop(x, ext1))
}

piseries <- vector("list",(ngcmstot))
for(ihwmi in 1:(ngcmstot)){
  piseries[[ihwmi]]<-lapply(piseries[[ihwmi]], FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
}

pimatrix <- do.call(cbind, piseries)


z <- as.zoo(pimatrix)


library(reshape2)
pimatrix <- as.data.frame(pimatrix)

nn_11_85 <- c("CNRM-ALADIN", "CNRM-CCLM", "CNRM-RCA", "ICHEC-CCLM", "ICHEC-HIRHAM", "ICHEC-RACMO", "ICHEC-RCA", "IPSL-RCA", "IPSL-WRF", "MPI-CCLM", "MPI-RCA", "MPI-REMO", "HadGEM-RACMO", "HadGEM-RCA", "HadGEM-CCLM", "HadGEM-HIRHAM")
rows<- seq(1:125)
colnames(pimatrix) <- nn_11_85
rownames(pimatrix) <- rows

pimatrixt <- as.data.frame(pimatrix)

pimatrix <- as.zoo(pimatrixt)


# Establezco la paleta de colores

myTheme <- custom.theme(pch = 20, cex = 0.7, alpha=0.7, symbol = brewer.pal(12, "Paired"), fill = brewer.pal(12, "Paired"))
myTheme$strip.background$col <- 'transparent'
myTheme$strip.shingle$col <- 'transparent'

# Hago la serie temporal

pi <- xyplot(pimatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,150),main=list("PI",cex=0.8), xlab=NULL,
             auto.key = FALSE, scales=list(
               x=list(labels=NULL)))
