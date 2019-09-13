###Figuras de  WSDI

library("raster")
library("rasterVis")
library("graticule")
library("rgdal")
library("maps")
library("mapdata")
library("maptools")
library("zoo")

setwd("/lustre/UCLM/mofelia.molina/calculos/WSDI")
## Hago un vector con la lista de combinaciones de GCM-RCM


lista <- dir(path="../WSDI/", pattern="^wsdi.*_adapta\\.nc$")

lista <- lapply(lista, FUN=function(x) paste("../WSDI/", x, sep=''))

ngcmstot <-length(lista)

w_indexes<-vector("list",(ngcmstot)) 
for(iwsdi in 1:(ngcmstot)){
  w_indexes[iwsdi]<-lapply(lista[iwsdi], stack,varname="wsdi")
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
for(iwsdi in 1:(ngcmstot)){borders[[iwsdi]] <- fooborder(w_indexes[[iwsdi]])  }


##Creo un indice temporal para hacer SetZ que me pide la función xyplot:

t <- seq(as.Date("1971-01-01"), as.Date("2100-01-01"), "year")

for(iwsdi in 1:(ngcmstot)){w_indexes[[iwsdi]] <- lapply(w_indexes[iwsdi], FUN=function(x) setZ(x, t))}


#1. Mediterráneo
# Recorto el dominio a la extensión de interés

med <- extent(-12, 40, 28, 50)

w_indexesmed<-vector("list",(ngcmstot))
for(iwsdi in 1:(ngcmstot)){
  w_indexesmed[[iwsdi]] <-lapply(w_indexes[[iwsdi]], FUN=function(x) mask(x, borders[[iwsdi]]))}


w_indexesmed2<-vector("list",(ngcmstot))
for(iwsdi in 1:(ngcmstot)){
  w_indexesmed2[[iwsdi]] <-lapply(w_indexesmed[[iwsdi]], FUN=function(x) crop(x, med))
}


##Media de los últimos 30 años MED:

w_indexesmedLast30<-vector("list",(ngcmstot))
for(iwsdi in 1:(ngcmstot)){
  w_indexesmedLast30[[iwsdi]] <-lapply(w_indexesmed2[[iwsdi]], FUN=function(x) x[[101:130]])
}

w_indexesmedLast30mean<-vector("list",(ngcmstot))
for(iwsdi in 1:(ngcmstot)){
  w_indexesmedLast30mean[[iwsdi]] <-lapply(w_indexesmedLast30[[iwsdi]], FUN=function(x) mean(x, na.rm=TRUE))
}

## Agrupo los ficheros para representar

WSDI <-vector("list",(ngcmstot))
for(iwsdi in 1:(ngcmstot)){
  WSDI[[iwsdi]] <-lapply(w_indexesmedLast30mean[[iwsdi]], FUN=function(x) stack(unlist(x)))
}

WSDIsinmar <-vector("list",(ngcmstot))
for(iwsdi in 1:(ngcmstot)){
  WSDIsinmar[[iwsdi]] <-lapply(WSDI[[iwsdi]], FUN=function(x) mask(x, borders[[iwsdi]]))
}


##Figura Barplot

#Hago la media de todo el dominio

w_imean <-vector("list",(ngcmstot))
for(iwsdi in 1:(ngcmstot)){
  w_imean[[iwsdi]] <- lapply(w_indexesmedLast30mean[[iwsdi]], FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
}

#Hago la desviación estándar de todo el dominio

w_sd <-vector("list",(ngcmstot))
for(iwsdi in 1:(ngcmstot)){
  w_sd[[iwsdi]] <- lapply(w_indexesmedLast30mean[[iwsdi]], FUN=function(x) cellStats(x, stat='sd', na.rm=TRUE))
}

#Hago un data frame de los datos

w_df <-vector("list",(ngcmstot))
for(iwsdi in 1:(ngcmstot)){
  w_df[[iwsdi]] <- lapply(w_imean[[iwsdi]], as.data.frame)
}  

w_sdmatrix <-vector("list",(ngcmstot))
for(iwsdi in 1:(ngcmstot)){
  w_sdmatrix[[iwsdi]] <- lapply(w_sd[[iwsdi]], as.data.frame)
} 

# Hago una matriz de los datos, de una sola columna, en la que estén intercalados rcp45 y rcp85 de cada modelo
#Media
w_mdf<-merge.data.frame(w_df[[1]],w_df[[2]],by="row.names")
for(iwsdi in 3:(ngcmstot)){
  w_mdf <- merge.data.frame(w_mdf, w_df[[iwsdi]], by="row.names")
}

w_mdf <- as.matrix(w_mdf[-1]) 
for(iwsdi in 1:55){
  w_mdf <- as.matrix(w_mdf[-1])}

#Desviación estándar
w_sdf<-merge.data.frame(w_sdmatrix[[1]],w_sdmatrix[[2]],by="row.names")
for(iwsdi in 3:(ngcmstot)){
  w_sdf <- merge.data.frame(w_sdf, w_sdmatrix[[iwsdi]], by="row.names")
}

w_sdf <- as.matrix(w_sdf[-1]) 
for(iwsdi in 1:55){
  w_sdf <- as.matrix(w_sdf[-1])}

## Coloco los datos en el orden que los quiero representar en el barplot

modg<-rep(1:1,11)
mod <- c(44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 11, 11, 11,11,11,11,11,11,11, 11,11,11,11,11,11,11,11,11,11, 11,11,11,11,11, 44, 44, 44, 44, 11, 11, 11, 11, 11, 11, 11)
mod1 <- c(44, 11,44, 11, 44, 11,44, 11, 11,44, 11,44, 11,44, 11,44, 11,44, 11,44, 11,44, 11, 44, 11, 44,11,11)
mod2 <- c(44, 11,44, 11, 44, 11,44, 11, 11,44, 11,44, 11,44, 11,44, 11,44, 11,44, 11,44, 11, 44, 11, 44,11,11,11)


w_M <- matrix(c(w_mdf, mod), 57, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))

M1_45 <- matrix(c(w_M[[32]], w_M[[3]], w_M[[34]], w_M[[1]], w_M[[36]],w_M[[5]], w_M[[38]], w_M[[9]], w_M[[7]], w_M[[40]],w_M[[11]], w_M[[42]], w_M[[13]],w_M[[44]],w_M[[15]],w_M[[46]],w_M[[17]],w_M[[52]],w_M[[26]], w_M[[50]], w_M[[28]], w_M[[56]],w_M[[30]], w_M[[48]], w_M[[22]], w_M[[50]], w_M[[24]], w_M[[19]],mod1), 28, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))

M1_85 <- matrix(c(w_M[[33]], w_M[[4]], w_M[[35]], w_M[[2]], w_M[[37]], w_M[[6]],w_M[[39]],w_M[[10]],w_M[[8]],w_M[[41]], w_M[[12]],w_M[[43]],
                    w_M[[14]],w_M[45],w_M[16],w_M[[47]],w_M[[18]],w_M[[53]], w_M[[27]],w_M[[51]], w_M[[29]], w_M[[57]],w_M[[31]],w_M[[49]], w_M[[23]], w_M[[51]],w_M[[25]],w_M[[20]], w_M[[21]], mod2), 29, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))


M <- as.data.frame(w_M)
M45 <- as.data.frame(M1_45)
M85 <- as.data.frame(M1_85)

## Matriz de la desviación estándar

Msd <- matrix(c(w_sdf, mod), 57, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))

#Hay que cambiar estos números según el orden en que se quieran representar los valores

Msd1_45 <- matrix(c(Msd[[32]], Msd[[3]], Msd[[34]], Msd[[1]], Msd[[36]],Msd[[5]], Msd[[38]], Msd[[9]], Msd[[7]], Msd[[40]],Msd[[11]], Msd[[42]], Msd[[13]],Msd[[44]],Msd[[15]],Msd[[46]],Msd[[17]],Msd[[52]],Msd[[26]], Msd[[50]], Msd[[28]], Msd[[56]],Msd[[30]], Msd[[48]], Msd[[22]], Msd[[50]], Msd[[24]], Msd[[19]],mod1), 28, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))

Msd1_85 <- matrix(c(Msd[[33]], Msd[[4]], Msd[[35]], Msd[[2]], Msd[[37]], Msd[[6]],Msd[[39]],Msd[[10]],Msd[[8]],Msd[[41]], Msd[[12]],Msd[[43]],
                    Msd[[14]],Msd[45],Msd[16],Msd[[47]],Msd[[18]],Msd[[53]], Msd[[27]],Msd[[51]], Msd[[29]], Msd[[57]],Msd[[31]],Msd[[49]], Msd[[23]], Msd[[51]],Msd[[25]],Msd[[20]], Msd[[21]], mod2), 29, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))

Msd <- as.data.frame(Msd)
Msd45 <- as.data.frame(Msd1_45)
Msd85 <- as.data.frame(Msd1_85)


nn_44_45 <- c("CanESM-RCA", "CNRM-CCLM","CNRM-ALADIN", "CNRM-ALADIN", "CNRM-RCA", "CNRM-RCA", "ECEARTH-HIRHAM", "ECEARTH-HIRHAM", "ECEARTH-CCLM", "ECEARTH-RAC", "ECEARTH-RAC", "ECEARTH-RCA", "ECEARTH-RCA", "IPSL-RCA", "IPSL-RCA", "IPSL-WRF",  "IPSL-WRF", "MPI-CCLM","MPI-CCLM", "MPI-RCA", "MPI-RCA", "MPI-REMO", "MPI-REMO", "HadGEM-RAC", "HadGEM-RAC","HadGEM-RCA", "HadGEM-RCA", "HadGEM-CCLM")

nn_44_85 <- c("CanESM-RCA", "CNRM-CCLM","CNRM-ALADIN", "CNRM-ALADIN", "CNRM-RCA", "CNRM-RCA", "ECEARTH-HIRHAM", "ECEARTH-HIRHAM", "ECEARTH-CCLM", "ECEARTH-RAC", "ECEARTH-RAC", "ECEARTH-RCA", "ECEARTH-RCA", "IPSL-RCA", "IPSL-RCA", "IPSL-WRF",  "IPSL-WRF", "MPI-CCLM","MPI-CCLM", "MPI-RCA", "MPI-RCA", "MPI-REMO", "MPI-REMO", "HadGEM-RAC", "HadGEM-RAC","HadGEM-RCA", "HadGEM-RCA", "HadGEM-CCLM", "HadGEM-HIRHAM")

##Hago un vector con la media de cada modelo

w_c45 <- c(71.53166,35.11117,42.6609,43.0777, 36.4578,42.16603,40.86466,40.12196,39.66241,38.965, 38.80963,
           47.39583,53.22136,70.44985,74.0578, 57.05056,63.01405,38.81164,34.18446,42.23178,45.59371,40.50615,
           39.06117,83.38123,79.03875,89.36995,94.95744,81.70299)
w_c85 <- c(71.53166,71.7412,91.84607,91.89399,80.94725,91.68588,103.1416,106.0862,94.16466,101.4033,99.64966,
           115.12,  124.4086,154.1524,158.3986,122.2417,132.8255,100.8527,92.72784,117.2293,121.84,  92.72784,
           112.9261,157.1757,153.8848,163.2633,168.1514,155.2168,151.9421)

sd45 <- c(30.10707,10.81209,13.79346,13.9987,13.51102,15.67444,15.44317,15.26382,15.93697,15.85772,14.54955,
          19.24695,20.61229,34.8791,37.9791,29.99389,32.34764,17.67041,15.41118,17.96394,19.73994,16.33787,
          17.25146,37.68398,36.87194,35.65436,39.22984,32.51638)
sd85 <- c(30.10707,25.08067,25.39648,23.88027,27.99699,30.68059,40.35976,40.71279,37.13936,38.06383,35.7374,
          37.49551,37.83424,65.46625,68.01816,60.60791,64.17277,43.4742,40.30838,45.85555,48.73069,40.30838,
          54.60797,49.3587,50.16468,50.10604,52.12964,44.6902,57.61907)


mycols <- c("pink", "gray")

w_b45 <- barplot(w_c45, col = mycols[M45$resolution], names.arg = nn_44_45, las=2, ylim=c(0,250), cex.names=0.7, main="RCP4.5", ylab="WSDI", legend=(M85$resolution[1:2]), args.legend = list(x = 'topright'))
arrows(w_b45, w_c45-sd45, w_b45, w_c45+sd45, angle = 90, code = 3, length=0.05)

w_b85 <- barplot(w_c85, col = mycols[M85$resolution], names.arg = nn_44_85, las=2, ylim=c(0,250), cex.names=0.7, main="RCP8.5", ylab="WSDI")
arrows(w_b85, w_c85-sd85, w_b85, w_c85+sd85, angle = 90, code = 3, length=0.05)



## Figura GCM vs RCM

w <- vector("list", 6)
w[[1]] <- stack(WSDIsinmar[[2]]) #CanESM
w[[2]] <- stack(WSDIsinmar[[4]]) #CNRM
w[[3]] <- stack(WSDIsinmar[[6]]) #MPI
w[[4]] <- stack(WSDIsinmar[[8]]) #HadGEM
w[[5]] <- stack(WSDIsinmar[[10]]) #IPSL
w[[6]] <- stack(WSDIsinmar[[12]]) #EC-EARTH



### Series temporales WSDI

c_11_85 =list("wsdi11CNRM-ALADIN85_adapta_runmean.nc", "wsdi11CNRM-CCLM85_C_adapta_runmean.nc", "wsdi11CNRM-RCA85_adapta_runmean.nc", "wsdi11ICHEC-CCLM85_adapta_runmean.nc", "wsdi11ICHEC-HIRHAM85_adapta_runmean.nc", "wsdi11ICHEC-RACMO85_adapta_runmean.nc", "wsdi11ICHEC-RCA85_adapta_runmean.nc","wsdi11IPSL-RCA85_adapta_runmean.nc", "wsdi11IPSL-WRF85_adapta_runmean.nc","wsdi11MPI-CCLM85_adapta_runmean.nc", "wsdi11MPI-RCA85_adapta_runmean.nc", "wsdi11MPI-REMO85_adapta_runmean.nc", "wsdi11MOHC-RACMO85_adapta_runmean.nc", "wsdi11MOHC-RCA85_adapta_runmean.nc", "wsdi11MOHC-CCLM85_adapta_runmean.nc", "wsdi11MOHC-HIRHAM85_adapta_runmean.nc")

##Abro los archivos con stack:

w_indexes_11_85 <- lapply(c_11_85, stack)

## creo un vector con los nombres de las capas.

nn_11_85 <- c("CNRM-ALADIN", "CNRM-CCLM", "CNRM-RCA", "ICHEC-CCLM", "ICHEC-HIRHAM", "ICHEC-RACMO", "ICHEC-RCA", "IPSL-RCA", "IPSL-WRF", "MPI-CCLM", "MPI-RCA", "MPI-REMO", "HadGEM-RACMO", "HadGEM-RCA", "HadGEM-CCLM", "HadGEM-HIRHAM")

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

border_11_85 <- fooborder(w_indexes_11_85[[1]])

##Creo un indice temporal para hacer SetZ que me pide la función xyplot: es más pequeño porque son las medias móviles, 126 años

idx <- seq(as.Date("1973-01-01"), as.Date("2098-01-01"), "year")

w_indexes_11_85 <- lapply(w_indexes_11_85, FUN=function(x) setZ(x, idx))

#Selecciono el periodo de interés para que coincidan los dos ficheros:

w_indexes_11_85 <- lapply(w_indexes_11_85, FUN=function(x) x[[1:125]])


#Selecciono zonas

e1 <- extent(-12, 0, 36, 38)
e2 <- extent(-12, 5, 38, 44)
e3 <- extent(-12, 7, 44, 50)
e4 <- extent(7, 19, 44, 50)
e5 <- extent(-12, 0, 28, 36)
e6 <- extent(12, 40, 28, 36)
e7 <- extent(19, 40, 44, 50)
e8 <- extent(5, 12, 38, 44)
e9 <- extent(12, 19, 40, 44)
e10 <- extent(0, 12, 28, 38)
e11 <- extent(19, 44, 36, 44)



## 1. PI
e1 <- sp1 + sp2

piseries <- lapply(w_indexes_11_85, FUN=function(x) mask(x, border_11_85))

piseries <- lapply(piseries, FUN=function(x) crop(x, e1))

piseries <- lapply(piseries, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

pimatrix <- do.call(cbind, piseries)


z <- as.zoo(pimatrix)

library(reshape2)
pimatrix <- as.data.frame(pimatrix)

rows<- c(1, 2, 3, 4,5, 6,7, 8, 9,10, 11,12, 13,14, 15,  16, 17,18,19,20, 21,22,23,24,25,26, 27, 28, 29,30, 31,32,33, 34,35,36,37,38, 39,  40,41,42,43, 44,45, 46, 47, 48,49, 50,51, 52,  53, 54,55, 56, 57,58,59, 60, 61,62, 63, 64, 65, 66,  67, 68,69,70,71, 72,73,74, 75, 76,77,78,79,  80, 81, 82, 83, 84,  85, 86, 87, 88,  89,  90, 91, 92, 93,  94,  95,  96, 97, 98, 99, 100,101,102,103,104,105,106,107, 108, 109,110,111,112,113,114, 115,116, 117,118,119,120,121,122,123,124,125)
colnames(pimatrix) <- nn_11_85
rownames(pimatrix) <- rows

pimatrix <- as.zoo(pimatrix)


myTheme <- custom.theme.2(pch = 20, cex = 0.7, alpha=0.7)
myTheme$strip.background$col <- 'transparent'
myTheme$strip.shingle$col <- 'transparent'

pi <- xyplot(pimatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,250),main=list("PI",cex=0.8), xlab=NULL,
             auto.key = FALSE, scales=list(
               x=list(labels=NULL)))

## para colocar leyenda:
auto.key = list(corner=c(0.95, 1), x = 0.8, y = 0.93)

## 2. FR

frseries <- lapply(w_indexes_11_85, FUN=function(x) mask(x, border_11_85))

frseries <- lapply(frseries, FUN=function(x) crop(x, e3))

frseries <- lapply(frseries, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

frmatrix <- do.call(cbind, frseries)

library(zoo)
z <- as.zoo(frmatrix)

library(reshape2)
frmatrix <- as.data.frame(frmatrix)

colnames(frmatrix) <- nn_11_85
rownames(frmatrix) <- rows

frmatrix <- as.zoo(frmatrix)

fr <- xyplot(frmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,250), main=list("FR",cex=0.8), xlab=NULL,
             auto.key =list(corner=c(1, 1), x = 0.6, y = 0.95, cex=0.4), scales=list(
               x=list(labels=NULL)))


## 3. ALPS

alpsseries <- lapply(w_indexes_11_85, FUN=function(x) mask(x, border_11_85))

alpsseries <- lapply(alpsseries, FUN=function(x) crop(x, e4))

alpsseries <- lapply(alpsseries, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

alpsmatrix <- do.call(cbind, alpsseries)


z <- as.zoo(alpsmatrix)

library(reshape2)
alpsmatrix <- as.data.frame(alpsmatrix)

colnames(alpsmatrix) <- nn_11_85
rownames(alpsmatrix) <- rows

alpsmatrix <- as.zoo(alpsmatrix)

alps <- xyplot(alpsmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme,ylim=c(0,250),main=list("ALPS",cex=0.8), xlab=NULL, ylab=NULL,
               auto.key = FALSE, scales=list(
                 x=list(labels=NULL), y=list(labels=NULL)))



## 4.WAFR

wafrseries <- lapply(w_indexes_11_85, FUN=function(x) mask(x, border_11_85))

wafrseries <- lapply(wafrseries, FUN=function(x) crop(x, e5))

wafrseries <- lapply(wafrseries, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

wafrmatrix <- do.call(cbind, wafrseries)

z <- as.zoo(wafrmatrix)

library(reshape2)
wafrmatrix <- as.data.frame(wafrmatrix)

colnames(wafrmatrix) <- nn_11_85
rownames(wafrmatrix) <- rows

wafrmatrix <- as.zoo(wafrmatrix)

wafr <- xyplot(wafrmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme,ylim=c(0,250),main=list("WAFR",cex=0.8),
               auto.key = FALSE, scales=list(
                 x=list(
                   at=seq(1,125,25),
                   labels=c("1973","1998","2023", "2048", "2073") )))


## 5. EAFR

eafrseries <- lapply(w_indexes_11_85, FUN=function(x) mask(x, border_11_85))

eafrseries <- lapply(eafrseries, FUN=function(x) crop(x, e6))

eafrseries <- lapply(eafrseries, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

eafrmatrix <- do.call(cbind, eafrseries)


z <- as.zoo(eafrmatrix)

eafrmatrix <- as.data.frame(eafrmatrix)

colnames(eafrmatrix) <- nn_11_85
rownames(eafrmatrix) <- rows

eafrmatrix <- as.zoo(eafrmatrix)


eafr <- xyplot(eafrmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,250),main=list("EAFR",cex=0.8),
               auto.key = FALSE, scales=list(
                 x=list(at=seq(1,125,25),
                        labels=c("1973","1998","2023", "2048", "2073") ), y=list(labels=NULL)))

##NEAST

neastseries <- lapply(w_indexes_11_85, FUN=function(x) mask(x, border_11_85))

neastseries <- lapply(neastseries, FUN=function(x) crop(x, e7))

neastseries <- lapply(neastseries, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

neastmatrix <- do.call(cbind, neastseries)


library(zoo)
z <- as.zoo(neastmatrix)

library(reshape2)
neastmatrix <- as.data.frame(neastmatrix)

colnames(neastmatrix) <- nn_11_85
rownames(neastmatrix) <- rows

neastmatrix <- as.zoo(neastmatrix)

neast <- xyplot(neastmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,250),main=list("NEAST", cex=0.8), xlab=NULL,
                auto.key = FALSE, scales=list(
                  x=list(labels=NULL), y=list(labels=NULL)))


##CMED

e8 <- sp8 + sp9

cmedseries <- lapply(w_indexes_11_85, FUN=function(x) mask(x, border_11_85))

cmedseries <- lapply(cmedseries, FUN=function(x) crop(x, e8))

cmedseries <- lapply(cmedseries, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

cmedmatrix <- do.call(cbind, cmedseries)


z <- as.zoo(cmedmatrix)

library(reshape2)
cmedmatrix <- as.data.frame(cmedmatrix)

colnames(cmedmatrix) <- nn_11_85
rownames(cmedmatrix) <- rows

cmedmatrix <- as.zoo(cmedmatrix)

cmed <- xyplot(cmedmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,250),main=list("CMED",cex=0.8), xlab=NULL,
               auto.key = FALSE, scales=list(
                 x=list(labels=NULL), y=list(labels=NULL)))


##CAFR

cafrseries <- lapply(w_indexes_11_85, FUN=function(x) mask(x, border_11_85))

cafrseries <- lapply(cafrseries, FUN=function(x) crop(x, e10))

cafrseries <- lapply(cafrseries, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

cafrmatrix <- do.call(cbind, cafrseries)


z <- as.zoo(cafrmatrix)

cafrmatrix <- as.data.frame(cafrmatrix)

colnames(cafrmatrix) <- nn_11_85
rownames(cafrmatrix) <- rows


cafrmatrix <- as.zoo(cafrmatrix)

cafr <- xyplot(cafrmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,250),main=list("CAFR",cex=0.8),
               auto.key = FALSE, scales=list(
                 x=list(
                   at=seq(1,125,25),
                   labels=c("1973","1998","2023", "2048", "2073") )))


##EAST

eastseries <- lapply(w_indexes_11_85, FUN=function(x) mask(x, border_11_85))

eastseries <- lapply(eastseries, FUN=function(x) crop(x, e11))

eastseries <- lapply(eastseries, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

eastmatrix <- do.call(cbind, eastseries)


z <- as.zoo(eastmatrix)

eastmatrix <- as.data.frame(eastmatrix)

colnames(eastmatrix) <- nn_11_85
rownames(eastmatrix) <- rows

eastmatrix <- as.zoo(eastmatrix)

east <- xyplot(eastmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,250),main=list("EAST",cex=0.8), xlab=NULL,
               auto.key = FALSE, scales=list(
                 x=list(labels=NULL), y=list(labels=NULL)))


## Represento todas juntas

pdf("printseriesWSDI.pdf", width=10, height=10)
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

