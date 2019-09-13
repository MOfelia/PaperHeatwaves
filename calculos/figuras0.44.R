###Figuras de HWMId 

library("raster")
library("rasterVis")
library("graticule")
library("rgdal")
library("maps")
library("mapdata")
library("maptools")

c <- list("hwmid44CCC-RCA45_adapta.nc", "hwmid44CCC-RCA85_adapta.nc", "hwmid44CNRM-ALADIN45K_adapta.nc", "hwmid44CNRM-ALADIN85K_adapta.nc", "hwmid44CNRM-RCA45_adapta.nc",  "hwmid44CNRM-RCA85_adapta.nc", "hwmid44ICHEC-HIRHAM45_adapta.nc", "hwmid44ICHEC-HIRHAM85_adapta.nc", "hwmid44ICHEC-RACMO45_adapta.nc", "hwmid44CHEC-RACMO85_adapta.nc", "hwmid44ICHEC-RCA45_adapta.nc", "hwmid44ICHEC-RCA85_adapta.nc", "hwmid44IPSL-RCA45_adapta.nc",  "hwmid44IPSL-RCA85_adapta.nc", "hwmid44IPSL-WRF45_adapta.nc", "hwmid44IPSL-WRF85_adapta.nc",  "hwmid44MPI-CCLM45_adapta.nc", "hwmid44MPI-CCLM85_adapta.nc",  "hwmid44MPI-RCA45_adapta.nc",  "hwmid44MPI-RCA85_adapta.nc",    "hwmid44MPI-REMO45_adapta.nc",   "hwmid44MPI-REMO85_adapta.nc")
c_11 <- list("hwmid11CNRM-ALADIN45_adapta.nc", "hwmid11CNRM-ALADIN85_adapta.nc", "hwmid11CNRM-CCLM45_adapta.nc","hwmid11CNRM-CCLM85_adapta.nc","hwmid11CNRM-RCA45_adapta.nc",  "hwmid11CNRM-RCA85_adapta.nc", "hwmid11ICHEC-CCLM45_adapta.nc", "hwmid11ICHEC-CCLM85_adapta.nc", "hwmid11ICHEC-HIRHAM45_adapta.nc", "hwmid11ICHEC-HIRHAM85_adapta.nc", "hwmid11ICHEC-RACMO45_adapta.nc", "hwmid11ICHEC-RACMO85_adapta.nc", "hwmid11ICHEC-RCA45_adapta.nc", "hwmid11ICHEC-RCA85_adapta.nc", "hwmid11IPSL-RCA45_adapta.nc",  "hwmid11IPSL-RCA85_adapta.nc", "hwmid11IPSL-WRF45_adapta.nc", "hwmid11IPSL-WRF85_adapta.nc",  "hwmid11MPI-CCLM45_adapta.nc", "hwmid11MPI-CCLM85_adapta.nc",  "hwmid11MPI-RCA45_adapta.nc",  "hwmid11MPI-RCA85_adapta.nc",    "hwmid11MPI-REMO45_adapta.nc",   "hwmid11MPI-REMO85_adapta.nc")
cm <- list("hwmid44MOHC-RACMO45_adapta.nc", "hwmid44MOHC-RACMO85_adapta.nc", "hwmid44MOHC-RCA45_adapta.nc", "hwmid44MOHC-RCA85_adapta.nc")
cm_11 <- list("hwmid11MOHC-RACMO45_adapta.nc", "hwmid11MOHC-RACMO85_adapta.nc", "hwmid11MOHC-RCA45_adapta.nc", "hwmid11MOHC-RCA85_adapta.nc", "hwmid11MOHC-CCLM45_adapta128.nc", "hwmid11MOHC-CCLM85_adapta128.nc", "hwmid11MOHC-HIRHAM85_adapta128.nc")

##Abro los archivos con stack:
h_indexesm <- lapply(cm, stack) 
h_indexesm_11 <- lapply(cm_11, stack)
h_indexes <- lapply(c, stack)
h_indexes_11 <- lapply(c_11, stack)

## creo un vector con los nombres de las capas.
nnm <- c("44MOHC-RACMO45", "44MOHC-RACMO85", "44MOHC-RCA45",  "44MOHC-RCA85")
nnm_11 <- c("11HadGEM-RACMO45", "11HadGEM-RACMO85", "11HadGEM-RCA45",  "11HadGEM-RCA85")
nn <- c("44CCC-RCA45", "44CCC-RCA85", "44CNRM-ALADIN45", "44CNRM-ALADIN85", "44CNRM-RCA45", "44CNRM-RCA85", "44ECEARTH-RACMO45", "44ICHEC-RACMO85",  "44ICHEC-RCA45", "44ICHEC-RCA85", "44IPSL-RCA45", "44IPSL-RCA85",  "44IPSL-WRF45", "44IPSL-WRF85", "44MPI-CCLM45", "44MPI-CCLM85",  "44MPI-RCA45", "44MPI-RCA85",  "44MPI-REMO45", "44MPI-REMO85", "44MOHC-RACMO45", "44MOHC-RACMO85", "44MOHC-RCA45",  "44MOHC-RCA85")
#nn_44_45 <- c("44CCC-RCA45", "44CNRM-ALADIN45", "44CNRM-RCA45",  "44ICHEC-RACMO45", "44ICHEC-RCA45", "44IPSL-RCA45", "44IPSL-WRF45", "44MPI-CCLM45", "44MPI-RCA45", "44MPI-REMO45")
#nn_44_85 <- c("44CCC-RCA85", "44CNRM-ALADIN85", "44CNRM-RCA85",  "44ICHEC-RACMO85", "44ICHEC-RCA85", "44IPSL-RCA85", "44IPSL-WRF85", "44MPI-CCLM85", "44MPI-RCA85", "44MPI-REMO85")
nn_11 <- c("11CNRM-ALADIN45", "11CNRM-ALADIN85", "11CNRM-CCLM45", "11CNRM-CCLM85", "11CNRM-RCA45", "11CNRM-RCA85", "11ICHEC-RACMO45", "11ICHEC-RACMO85", "11ICHEC-RCA45", "11ICHEC-RCA85", "11IPSL-RCA45", "11IPSL-RCA85",  "11IPSL-WRF45", "11IPSL-WRF85", "11MPI-CCLM45", "11MPI-CCLM85",  "11MPI-RCA45", "11MPI-RCA85",  "11MPI-REMO45", "11MPI-REMO85")

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

borderm <- fooborder(h_indexesm[[1]])
borderm_11 <- fooborder(h_indexesm_11[[1]])
border <- fooborder(h_indexes[[1]])
border_11 <- fooborder(h_indexes_11[[1]])

##Creo un indice temporal para hacer SetZ que me pide la función xyplot:

idx <- seq(as.Date("1971-01-01"), as.Date("2100-01-01"), "year")
idxm <- seq(as.Date("1971-01-01"), as.Date("2098-12-01"), "year")

h_indexesm <- lapply(h_indexesm, FUN=function(x) setZ(x, idxm))
h_indexesm_11 <- lapply(h_indexesm_11, FUN=function(x) setZ(x, idxm))
h_indexes <- lapply(h_indexes, FUN=function(x) setZ(x, idx))
h_indexes_11 <- lapply(h_indexes_11, FUN=function(x) setZ(x, idx))

##Seleccionamos el dominio que nos interesa:

#1. Mediterráneo

#m <- extent(-25.63, 50.65, 26.73, 52.34)
med <- extent(-12, 40, 28, 50)

h_indexesmedM <- lapply(h_indexesm, FUN=function(x) mask(x, borderm))
h_indexesmedM_11 <- lapply(h_indexesm_11, FUN=function(x) mask(x, borderm_11))
h_indexesmed <- lapply(h_indexes, FUN=function(x) mask(x, border))
h_indexesmed_11 <- lapply(h_indexes_11, FUN=function(x) mask(x, border_11))

h_indexesmedM <- lapply(h_indexesmedM, FUN=function(x) crop(x, med))
h_indexesmedM_11 <- lapply(h_indexesmedM_11, FUN=function(x) crop(x, med))
h_indexesmed <- lapply(h_indexesmed, FUN=function(x) crop(x, med))
h_indexesmed_11 <- lapply(h_indexesmed_11, FUN=function(x) crop(x, med))

#1. Norte Europa

ne <- extent(-8, 40, 50, 73) 

h_indexesne <- lapply(h_indexes, FUN=function(x) mask(x, border))
h_indexesne_44_45 <- lapply(h_indexes_44_45, FUN=function(x) mask(x, border_44_45))
h_indexesne_44_85 <- lapply(h_indexes_44_85, FUN=function(x) mask(x, border_44_85))
h_indexesne_11 <- lapply(h_indexes_11, FUN=function(x) mask(x, border))

h_indexesne <- lapply(h_indexesne, FUN=function(x) crop(x, ne))
h_indexesne_44_45 <- lapply(h_indexesne_44_45, FUN=function(x) crop(x, ne))
h_indexesne_44_85 <- lapply(h_indexesne_44_85, FUN=function(x) crop(x, ne))
h_indexesne_11 <- lapply(h_indexesne_11, FUN=function(x) crop(x, ne))


##Media de los últimos 30 años MED:

h_indexesmedLast30M <- lapply(h_indexesmedM, FUN=function(x) x[[099:128]])
h_indexesmedLast30M_11 <- lapply(h_indexesmedM_11, FUN=function(x) x[[099:128]])
h_indexesmedLast30 <- lapply(h_indexesmed, FUN=function(x) x[[101:130]])
h_indexesmedLast30_11 <- lapply(h_indexesmed_11, FUN=function(x) x[[101:130]])


h_indexesmedLast30meanM <- lapply(h_indexesmedLast30M, FUN=function(x) mean(x, na.rm=TRUE))
h_indexesmedLast30meanM_11 <- lapply(h_indexesmedLast30M_11, FUN=function(x) mean(x, na.rm=TRUE))
h_indexesmedLast30mean <- lapply(h_indexesmedLast30, FUN=function(x) mean(x, na.rm=TRUE))
h_indexesmedLast30mean_11 <- lapply(h_indexesmedLast30_11, FUN=function(x) mean(x, na.rm=TRUE))


##Media de los últimos 30 años NE:

h_indexesneLast30M <- lapply(h_indexesmedM, FUN=function(x) x[[099:128]])
h_indexesneLast30 <- lapply(h_indexesne, FUN=function(x) x[[101:130]])
h_indexesneLast30_44_45 <- lapply(h_indexesne_44_45, FUN=function(x) x[[101:130]])
h_indexesneLast30_44_85 <- lapply(h_indexesne_44_85, FUN=function(x) x[[101:130]])
h_indexesneLast30_11 <- lapply(h_indexesne_11, FUN=function(x) x[[101:130]])

h_indexesRef <- lapply(h_indexesne, FUN=function(x) x[[1:30]])

h_indexesneLast30meanM <- lapply(h_indexesmedLast30M, FUN=function(x) mean(x, na.rm=TRUE))
h_indexesneLast30mean <- lapply(h_indexesneLast30, FUN=function(x) mean(x, na.rm=TRUE))
h_indexesneLast30mean_44_45 <- lapply(h_indexesneLast30_44_45, FUN=function(x) mean(x, na.rm=TRUE))
h_indexesneLast30mean_44_85 <- lapply(h_indexesneLast30_44_85, FUN=function(x) mean(x, na.rm=TRUE))
h_indexesneLast30mean_11 <- lapply(h_indexesneLast30_11, FUN=function(x) mean(x, na.rm=TRUE))

h_indexesRefmean <- lapply(h_indexesRef, FUN=function(x) mean(x, na.rm=TRUE))

# MED:
HWMImedM <- stack(unlist(h_indexesmedLast30meanM))
HWMImedM_11 <- stack(unlist(h_indexesmedLast30meanM_11))
HWMImed <- stack(unlist(h_indexesmedLast30mean))
HWMImed_11 <- stack(unlist(h_indexesmedLast30mean_11))

HWMImedsinmarM <- mask(HWMImedM, borderm)
HWMImedsinmarM_11 <- mask(HWMImedM_11, borderm_11)
HWMImedsinmar <- mask(HWMImed, border)
HWMImedsinmar_11 <- mask(HWMImed_11, border_11)

#Unir los MOHC-HadGEM a los otros modelos en un mismo stack:

w <- stack(c(HWMImedsinmar,HWMImedsinmarM))
w_11 <- stack(c(HWMImedsinmar_11,HWMImedsinmarM_11))

names(HWMImedsinmar) <- nn
names(HWMImedsinmar_44_45) <- nn_44_45
names(HWMImedsinmar_44_85) <- nn_44_85
names(HWMImedsinmar_11) <- nn_11

#NE:

HWMIne <- stack(unlist(h_indexesneLast30mean))
HWMIne_44_45 <- stack(unlist(h_indexesneLast30mean_44_45))
HWMIne_44_85 <- stack(unlist(h_indexesneLast30mean_44_85))
HWMIne_11 <- stack(unlist(h_indexesneLast30mean_11))

HWMInesinmar <- mask(HWMIne, border)
HWMInesinmar_44_45 <- mask(HWMIne_44_45, border_44_45)
HWMInesinmar_44_85 <- mask(HWMIne_44_85, border_44_85)
HWMInesinmar_11 <- mask(HWMIne_11, border_11)

names(HWMInesinmar) <- nn
names(HWMInesinmar_44_45) <- nn_44_45
names(HWMInesinmar_44_85) <- nn_44_85
names(HWMInesinmar_11) <- nn_11


## Figura 1: gráfico de barras
## Calculo la media de todo el dominio para cada modelo

h_meanM <- lapply(h_indexesmedLast30meanM, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
h_meanM_11 <- lapply(h_indexesmedLast30meanM_11, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
h_mean44 <- lapply(h_indexesmedLast30mean, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
h_mean11 <- lapply(h_indexesmedLast30mean_11, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

## Calculo la desviación estándar de todo el dominio para cada modelo

h_sdM <- lapply(h_indexesmedLast30meanM, FUN=function(x) cellStats(x, stat='sd', na.rm=TRUE))
h_sdM_11 <- lapply(h_indexesmedLast30meanM_11, FUN=function(x) cellStats(x, stat='sd', na.rm=TRUE))
h_sd44 <- lapply(h_indexesmedLast30mean, FUN=function(x) cellStats(x, stat='sd', na.rm=TRUE))
h_sd11 <- lapply(h_indexesmedLast30mean_11, FUN=function(x) cellStats(x, stat='sd', na.rm=TRUE))

## Hago una matriz con la media y otra matriz con la desviación estándar

dfM <- lapply(h_meanM, as.data.frame)
dfM_11 <- lapply(h_meanM_11, as.data.frame)
df44 <- lapply(h_mean44, as.data.frame)
df11 <- lapply(h_mean11, as.data.frame)

sdM <- lapply(h_sdM, as.data.frame)
sdM_11 <- lapply(h_sdM_11, as.data.frame)
sd44 <- lapply(h_sd44, as.data.frame)
sd11 <- lapply(h_sd11, as.data.frame)

m <- merge.data.frame(df44, df11, by="row.names")
m <- merge.data.frame(m, dfM, by="row.names")
m <- merge.data.frame(m, dfM_11, by="row.names")
m <- as.matrix(m[-3])
m <- as.matrix(m[-1])
m <- as.matrix(m[-1])


mod1 <- c(44, 11,44, 11, 44, 11,44, 11, 11,44, 11,44, 11,44, 11,44, 11,44, 11,44, 11,44, 11, 44, 11, 44,11,11)
mod <- c(44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 11, 11, 11,11,11,11,11,11,11, 11,11,11,11,11,11,11,11,11,11, 11,11,11,11,11, 44, 44, 44, 44, 11, 11, 11, 11, 11, 11, 11)
mod2 <- c(44, 11,44, 11, 44, 11,44, 11, 11,44, 11,44, 11,44, 11,44, 11,44, 11,44, 11,44, 11, 44, 11, 44,11,11,11)


M<- matrix(c(m, mod), 57, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))


M1_45 <- matrix(c(M[[1]], M[[25]], M[[3]], M[[23]], M[[5]],M[[27]], M[[7]], M[[31]], M[[29]], M[[9]],M[[33]], M[[11]], M[[35]],M[[13]],M[[37]],M[[15]],M[[39]],M[[17]],M[[41]], M[[19]], M[[43]], M[[21]],M[[45]], M[[47]], M[[51]], M[[49]], M[[53]], M[[55]],mod1), 28, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))
M1_85 <- matrix(c(M[[2]], M[[26]], M[[4]], M[[24]], M[[6]], M[[28]],M[[8]],M[[32]],M[[30]],M[[10]], M[[34]],M[[12]],
                    M[[36]],M[14],M[38],M[[16]],M[[40]],M[[18]], M[[42]],M[[20]], M[[44]], M[[42]],M[[46]],M[[48]], M[[52]], M[[50]],M[[54]],M[[56]], M[[57]], mod2), 29, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))



M <- as.data.frame(M)
M45 <- as.data.frame(M1_45)
M85 <- as.data.frame(M1_85)

## Matriz de la desviación estándar


msd <- merge.data.frame(sd44, sd11, by="row.names")
msd <- merge.data.frame(msd, sdM, by="row.names")
msd <- merge.data.frame(msd, sdM_11, by="row.names")
msd <- as.matrix(msd[-3])
msd <- as.matrix(msd[-1])
msd <- as.matrix(msd[-1])

Msd <- matrix(c(msd, mod), 57, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))


Msd1_45 <- matrix(c(Msd[[1]], Msd[[25]], Msd[[3]], Msd[[23]], Msd[[5]],Msd[[27]], Msd[[7]], Msd[[31]], Msd[[29]], Msd[[9]],Msd[[33]], Msd[[11]], Msd[[35]],Msd[[13]],Msd[[37]],Msd[[15]],Msd[[39]],Msd[[17]],Msd[[41]], Msd[[19]], Msd[[43]], Msd[[21]],Msd[[45]], Msd[[47]], Msd[[51]], Msd[[49]], Msd[[53]], Msd[[55]],mod1), 28, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))

Msd1_85 <- matrix(c(Msd[[2]], Msd[[26]], Msd[[4]], Msd[[24]], Msd[[6]], Msd[[28]],Msd[[8]],Msd[[32]],Msd[[30]],Msd[[10]], Msd[[34]],Msd[[12]],
                    Msd[[36]],Msd[14],Msd[38],Msd[[16]],Msd[[40]],Msd[[18]], Msd[[42]],Msd[[20]], Msd[[44]], Msd[[42]],Msd[[46]],Msd[[48]], Msd[[52]], Msd[[50]],Msd[[54]],Msd[[56]], Msd[[57]], mod2), 29, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))

Msd <- as.data.frame(Msd)
Msd45 <- as.data.frame(Msd1_45)
Msd85 <- as.data.frame(Msd1_85)


nn_44_45 <- c("CanESM-RCA", "CNRM-CCLM","CNRM-ALADIN", "CNRM-ALADIN", "CNRM-RCA", "CNRM-RCA", "ECEARTH-HIRHAM", "ECEARTH-HIRHAM", "ECEARTH-CCLM", "ECEARTH-RAC", "ECEARTH-RAC", "ECEARTH-RCA", "ECEARTH-RCA", "IPSL-RCA", "IPSL-RCA", "IPSL-WRF",  "IPSL-WRF", "MPI-CCLM","MPI-CCLM", "MPI-RCA", "MPI-RCA", "MPI-REMO", "MPI-REMO", "HadGEM-RAC", "HadGEM-RAC","HadGEM-RCA", "HadGEM-RCA", "HadGEM-CCLM")

nn_44_85 <- c("CanESM-RCA", "CNRM-CCLM","CNRM-ALADIN", "CNRM-ALADIN", "CNRM-RCA", "CNRM-RCA", "ECEARTH-HIRHAM", "ECEARTH-HIRHAM", "ECEARTH-CCLM", "ECEARTH-RAC", "ECEARTH-RAC", "ECEARTH-RCA", "ECEARTH-RCA", "IPSL-RCA", "IPSL-RCA", "IPSL-WRF",  "IPSL-WRF", "MPI-CCLM","MPI-CCLM", "MPI-RCA", "MPI-RCA", "MPI-REMO", "MPI-REMO", "HadGEM-RAC", "HadGEM-RAC","HadGEM-RCA", "HadGEM-RCA", "HadGEM-CCLM", "HadGEM-HIRHAM")

##Hago un vector con la media de cada modelo


df$color <- palette[as.factor(M45$resolution)]


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
  
## ordeno para representar
#MED:
wmed <- stack(HWMImedsinmar[[1:20]])
wmed_44_45 <- stack(HWMImedsinmar_44_45[[1:10]])
wmed_44_85 <- stack(HWMImedsinmar_44_85[[1:10]])
wmed_11 <- stack(HWMImedsinmar_11[[1:20]])
wmed_11_85 <- stack(w_11[[14]],w_11[[28]], w_11[[6]], w_11[[22]],w_11[[16]],w_11[[8]],w_11[[30]],w_11[[4]],w_11[[20]],w_11[[18]],w_11[[10]],w_11[[31]], w_11[[2]],w_11[[24]],w_11[[26]],w_11[[12]])



#NE:
wne <- stack(HWMInesinmar[[1:20]])
wne_44_45 <- stack(HWMInesinmar_44_45[[1:10]])
wne_44_85 <- stack(HWMInesinmar_44_85[[1:10]])
wne_11 <- stack(HWMInesinmar_11[[1:20]])

my.at <- seq(0,20,1)
my.at <- c(my.at, 30, 40, 50, 75, 100, 125)
wmed_11_85[wmed_11_85[]>100] <- 100
length(wmed_11_85[wmed_11_85[] > 150]) <- HWMIsinmar
HWMIsinmar[HWMIsinmar[]>100] <- 100
HWMIsinmar[HWMIsinmar[]>100] <- 100
myColorkey <- list(at=my.at, ## where the colors change
                   space="bottom",
                   width= .8)


div.pal <- brewer.pal(n=9, 'Set3')

div.pal <- brewer.pal(n=9, 'RdYlGn')

##Figura 3: Mapas 0.11, RCP8.5

levelplot(wmed_11_85, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("HWMId"), cex=1), xlab=NULL, ylab=list(c( ""), cex=1),  names.attr=c("ECEARTH-RCA", "HadGEM-RCA", "CNRM-RCA", "MPI-RCA", "IPSL-RCA", "ECEARTH-CCLM", "HadGEM-CCLM","CNRM-CCLM", "MPI-CCLM", "IPSL-WRF", "ECEARTH-HIRHAM", "HadGEM-HIRHAM","CNRM-ALADIN", "MPI-REMO", "HadGEM-RACMO", "ECEARTH-RACMO"), layout=c(5,4), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at))) + layer(sp.lines(border, lwd=0.5))



##Otras figuras

pdf("hwmidmed44.pdf")
levelplot(wmed, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85", "RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c("EUR44"), cex=1), names.attr=c("CanESM-RCA", "CanESM-RCA", "CNRM-ALADIN", "CNRM-ALADIN", "CNRM-RCA", "CNRM-RCA","ECEARTH-RACMO", "EC-EARTH-RACMO","EC-EARTH-RCA","EC-EARTH-RCA", "IPSL-RCA", "IPSL-RCA", "IPSL-WRF", "IPSL-WRF", "MPI-CCLM","MPI-CCLM", "MPI-RCA", "MPI-RCA", "MPI-REMO", "MPI-REMO"), layout=c(4,5), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at)))+
  layer(sp.lines(border, lwd=0.5))
dev.off()

pdf("hwmidmed44_45.pdf")
levelplot(wmed_44_45, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("", ""), cex=1), xlab=NULL, ylab=list(c("EUR44.RCP45"), cex=1), names.attr=c( "CanESM-RCA", "CNRM-ALADIN", "CNRM-RCA","ECEARTH-RACMO", "EC-EARTH-RCA", "IPSL-RCA", "IPSL-WRF", "MPI-CCLM", "MPI-RCA", "MPI-REMO"), layout=c(2,5), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at)))+
   layer(sp.lines(border, lwd=0.5))
dev.off()

pdf("hwmidmed44_85.pdf")
levelplot(wmed_44_85, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("", ""), cex=1), xlab=NULL, ylab=list(c("EUR44-RCP85"), cex=1), names.attr=c("CanESM-RCA", "CNRM-ALADIN", "CNRM-RCA","ECEARTH-RACMO", "EC-EARTH-RCA", "IPSL-RCA", "IPSL-WRF", "MPI-CCLM", "MPI-RCA", "MPI-REMO"), layout=c(2,5), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at)))+
  layer(sp.lines(border, lwd=0.5))
dev.off()

pdf("hwmidmed_11.pdf")
levelplot(wmed_11, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85", "RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c("EUR11"), cex=1), names.attr=c("CNRM-ALADIN", "CNRM-ALADIN", "CNRM-CCLM", "CNRM-CCLM", "CNRM-RCA", "CNRM-RCA","ECEARTH-RACMO", "EC-EARTH-RACMO","EC-EARTH-RCA","EC-EARTH-RCA", "IPSL-RCA", "IPSL-RCA", "IPSL-WRF", "IPSL-WRF", "MPI-CCLM","MPI-CCLM", "MPI-RCA", "MPI-RCA", "MPI-REMO", "MPI-REMO"), layout=c(4,5), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at)))+
 layer(sp.lines(border, lwd=0.5))
dev.off()

pdf("hwmidne44.pdf")
levelplot(wne, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85", "RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c("EUR44"), cex=1), names.attr=c("CanESM-RCA", "CanESM-RCA", "CNRM-ALADIN", "CNRM-ALADIN", "CNRM-RCA", "CNRM-RCA","EC-EARTH-RACMO", "ECEARTH-RACMO","EC-EARTH-RCA","EC-EARTH-RCA", "IPSL-RCA", "IPSL-RCA", "IPSL-WRF", "IPSL-WRF", "MPI-CCLM","MPI-CCLM", "MPI-RCA", "MPI-RCA", "MPI-REMO", "MPI-REMO"), layout=c(4,5), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at)))+
  layer(sp.lines(border, lwd=0.5))
dev.off()

pdf("hwmidne44_45.pdf")
levelplot(wne_44_45, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("", ""), cex=1), xlab=NULL, ylab=list(c("EUR44-RCP45"), cex=1), names.attr=c( "CanESM-RCA", "CNRM-ALADIN", "CNRM-RCA","EC-EARTH-RACMO", "ECEARTH-RCA", "IPSL-RCA", "IPSL-WRF", "MPI-CCLM", "MPI-RCA", "MPI-REMO"), layout=c(2,5), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at)))+
  layer(sp.lines(border, lwd=0.5))
dev.off()

pdf("hwmidne44_85.pdf")
levelplot(wne_44_85, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("", ""), cex=1), xlab=NULL, ylab=list(c("EUR44-RCP85"), cex=1), names.attr=c("CanESM-RCA", "CNRM-ALADIN", "CNRM-RCA","EC-EARTH-RACMO", "ECEARTH-RCA", "IPSL-RCA", "IPSL-WRF", "MPI-CCLM", "MPI-RCA", "MPI-REMO"), layout=c(2,5), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at)))+
  layer(sp.lines(border, lwd=0.5))
dev.off()

pdf("hwmidne_11.pdf")
levelplot(wne_11, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85", "RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c("EUR11"), cex=1), names.attr=c("CNRM-ALADIN", "CNRM-ALADIN", "CNRM-CCLM", "CNRM-CCLM", "CNRM-RCA", "CNRM-RCA","ECEARTH-RACMO", "ECEARTH-RACMO","EC-EARTH-RCA","EC-EARTH-RCA", "IPSL-RCA", "IPSL-RCA", "IPSL-WRF", "IPSL-WRF", "MPI-CCLM","MPI-CCLM", "MPI-RCA", "MPI-RCA", "MPI-REMO", "MPI-REMO"), layout=c(2,2), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at)))+
  layer(sp.lines(border, lwd=0.5))
dev.off()

## RCA44

wrca44 <- stack(HWMImedsinmar[[1]], HWMImedsinmar[[2]], HWMImedsinmar[[5]], HWMImedsinmar[[6]], HWMImedsinmar[[9]], HWMImedsinmar[[10]], HWMImedsinmar[[11]], HWMImedsinmar[[12]], HWMImedsinmar[[17]], HWMImedsinmar[[18]])

pdf("hwmidmedRCA44.pdf")
levelplot(wrca44, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c("44RCA"), cex=1), names.attr=c("CanESM-RCA","CanESM-RCA","CNRM-RCA","CNRM-RCA","ECEARTH-RCA", "ECEARTH-RCA", "IPSL-RCA","IPSL-RCA","MPI-RCA","MPI-RCA"), layout=c(2,5), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at)))+
  layer(sp.lines(border, lwd=0.5))
dev.off()

## RCA11

wrca11 <- stack(HWMImedsinmar_11[[5]], HWMImedsinmar_11[[6]], HWMImedsinmar_11[[9]], HWMImedsinmar_11[[10]], HWMImedsinmar_11[[11]], HWMImedsinmar_11[[12]], HWMImedsinmar_11[[17]], HWMImedsinmar_11[[18]])

pdf("hwmidmedRCA11.pdf")
levelplot(wrca11, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c("11RCA"), cex=1), names.attr=c("CNRM-RCA","CNRM-RCA","ECEARTH-RCA", "ECEARTH-RCA", "IPSL-RCA","IPSL-RCA","MPI-RCA","MPI-RCA"), layout=c(2,4), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at)))+
  layer(sp.lines(border, lwd=0.5))
dev.off()

##CNRM44

wmedcnrm44 <- stack(HWMImedsinmar[[3]],HWMImedsinmar[[4]], HWMImedsinmar[[5]], HWMImedsinmar[[6]])

pdf("hwmidmedCNRM44.pdf")
levelplot(wmedcnrm44, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c("CNRM 0.44"), cex=1), names.attr=c("ALADIN", "ALADIN", "RCA", "RCA"), layout=c(2,2), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at)))+
  layer(sp.lines(border, lwd=0.5))
dev.off()

##CNRM11

wmedcnrm11 <- stack(HWMImedsinmar_11[[1]],HWMImedsinmar_11[[2]],HWMImedsinmar_11[[3]],HWMImedsinmar_11[[4]], HWMImedsinmar_11[[5]], HWMImedsinmar_11[[6]])

pdf("hwmidmedCNRM11.pdf")
levelplot(wmedcnrm11, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c("CNRM 0.11"), cex=1), names.attr=c("ALADIN", "ALADIN", "CCLM","CCLM","RCA", "RCA"), layout=c(2,3), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at)))+
  layer(sp.lines(border, lwd=0.5))
dev.off()




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

#levelplot(w_11[[1]], margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c(""), cex=1), xlab=NULL, ylab=list(c( ""), cex=1), names.attr=c(""), layout=c(), colorkey=FALSE)+ layer(sp.lines(border, lwd=0.5)) + layer(sp.lines(spT,lwd=1.5, col='darkgray'))
#levelplot(w_11[[1]], margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("HWMId"), cex=1), xlab=NULL, ylab=list(c( ""), cex=1), names.attr=c("CNRM-RCA"), layout=c(), colorkey=myColorkey)+ layer(sp.lines(border, lwd=0.5)) + layer(sp.lines(sp1))+ layer(sp.lines(sp3))+ layer(sp.lines(sp4))+ layer(sp.lines(sp5))+ layer(sp.lines(sp6))+ layer(sp.lines(sp7))+ layer(sp.lines(sp8))+ layer(sp.lines(sp9,lwd=1.5, col='darkgray'))
#levelplot(w_11[[1]], margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("HWMId"), cex=1), xlab=NULL, ylab=list(c( ""), cex=1), names.attr=c("CNRM-RCA"), layout=c(), colorkey=myColorkey)+ layer(sp.lines(border, lwd=0.5)) + layer(sp.polygons(sp1))+ layer(sp.polygons(sp3))+ layer(sp.polygons(sp4))+ layer(sp.polygons(sp5))+ layer(sp.polygons(sp6))+ layer(sp.polygons(sp7))+ layer(sp.polygons(sp8))+ layer(sp.polygons(sp9,lwd=1.5, col='darkgray'))
#PI <- lapply(h_indexes, FUN=function(x) mask(x, border))
library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-12, 40), ylim = c(28, 50), asp = 1, lwd= 0.4)
spatial.polys = SpatialPolygons(list(polys))

proj4string(newmap) = CRS("+proj=longlat +datum=WGS84 +no_defs  +ellps=WGS84 +towgs84=0,0,0")

proj4string(spT) = CRS("+proj=longlat +datum=WGS84 +no_defs  +ellps=WGS84 +towgs84=0,0,0")

proj4string(sp1) = proj4string(newmap)
proj4string(sp2) = CRS("+proj=longlat +datum=WGS84 +no_defs  +ellps=WGS84 +towgs84=0,0,0")
proj4string(sp3) = CRS("+proj=longlat +datum=WGS84 +no_defs  +ellps=WGS84 +towgs84=0,0,0")
proj4string(sp4) = CRS("+proj=longlat +datum=WGS84 +no_defs  +ellps=WGS84 +towgs84=0,0,0")
proj4string(sp5) = CRS("+proj=longlat +datum=WGS84 +no_defs  +ellps=WGS84 +towgs84=0,0,0")
proj4string(sp6) = CRS("+proj=longlat +datum=WGS84 +no_defs  +ellps=WGS84 +towgs84=0,0,0")
proj4string(sp7) = CRS("+proj=longlat +datum=WGS84 +no_defs  +ellps=WGS84 +towgs84=0,0,0")
proj4string(sp8) = CRS("+proj=longlat +datum=WGS84 +no_defs  +ellps=WGS84 +towgs84=0,0,0")
proj4string(sp9) = CRS("+proj=longlat +datum=WGS84 +no_defs  +ellps=WGS84 +towgs84=0,0,0")
proj4string(sp10) = CRS("+proj=longlat +datum=WGS84 +no_defs  +ellps=WGS84 +towgs84=0,0,0")
proj4string(sp11) = CRS("+proj=longlat +datum=WGS84 +no_defs  +ellps=WGS84 +towgs84=0,0,0")

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





### WSDI

c <- list("wsdi44CCC-RCA45_adapta.nc", "wsdi44CCC-RCA85_adapta.nc", "wsdi44CNRM-ALADIN45_adapta.nc", "wsdi44CNRM-ALADIN85_adapta.nc", "wsdi44CNRM-RCA45_adapta.nc",  "wsdi44CNRM-RCA85_adapta.nc", "wsdi44ICHEC-HIRHAM45_adapta.nc", "wsdi44ICHEC-HIRHAM85_adapta.nc", "wsdi44ICHEC-RACMO45_adapta.nc", "wsdi44ICHEC-RACMO85_adapta.nc", "wsdi44ICHEC-RCA45_adapta.nc", "wsdi44ICHEC-RCA85_adapta.nc", "wsdi44IPSL-RCA45_adapta.nc",  "wsdi44IPSL-RCA85_adapta.nc", "wsdi44IPSL-WRF45_adapta.nc", "wsdi44IPSL-WRF85_adapta.nc",  "wsdi44MPI-CCLM45_adapta.nc", "wsdi44MPI-CCLM85_adapta.nc",  "wsdi44MPI-RCA45_adapta.nc",  "wsdi44MPI-RCA85_adapta.nc",    "wsdi44MPI-REMO45_adapta.nc",   "wsdi44MPI-REMO85_adapta.nc")
c1 <- list("wsdi11CNRM-ALADIN45_adapta.nc", "wsdi11CNRM-ALADIN85_adapta.nc", "wsdi11CNRM-CCLM45_C_adapta.nc","wsdi11CNRM-CCLM85_C_adapta.nc","wsdi11CNRM-RCA45_adapta.nc",  "wsdi11CNRM-RCA85_adapta.nc", "wsdi11ICHEC-CCLM45_adapta.nc", "wsdi11ICHEC-CCLM85_adapta.nc", "wsdi11ICHEC-HIRHAM45_adapta.nc", "wsdi11ICHEC-HIRHAM85_adapta.nc", "wsdi11ICHEC-RACMO45_adapta.nc", "wsdi11ICHEC-RACMO85_adapta.nc", "wsdi11ICHEC-RCA45_adapta.nc", "wsdi11ICHEC-RCA85_adapta.nc", "wsdi11IPSL-RCA45_adapta.nc",  "wsdi11IPSL-RCA85_adapta.nc", "wsdi11IPSL-WRF45_adapta.nc", "wsdi11IPSL-WRF85_adapta.nc",  "wsdi11MPI-CCLM45_adapta.nc", "wsdi11MPI-CCLM85_adapta.nc",  "wsdi11MPI-RCA45_adapta.nc",  "wsdi11MPI-RCA85_adapta.nc",    "wsdi11MPI-REMO45_adapta.nc",   "wsdi11MPI-REMO85_adapta.nc")
cm <- list("wsdi44MOHC-RACMO45_adapta.nc", "wsdi44MOHC-RACMO85_adapta.nc", "wsdi44MOHC-RCA45_adapta.nc", "wsdi44MOHC-RCA85_adapta.nc")
cm_11 <- list("wsdi11MOHC-RACMO45_adapta.nc", "wsdi11MOHC-RACMO85_adapta.nc", "wsdi11MOHC-RCA45_adapta.nc", "wsdi11MOHC-RCA85_adapta.nc", "wsdi11MOHC-CCLM45_adapta.nc", "wsdi11MOHC-CCLM85_adapta.nc", "wsdi11MOHC-HIRHAM85_adapta.nc")

nn<- c("44CCC-RCA45", "44CCC-RCA85", "44CNRM-ALADIN45", "44CNRM-ALADIN85", "44CNRM-RCA45",  "44CNRM-RCA85", "44ICHEC-RACMO45", "44ICHEC-RACMO85", "44ICHEC-RCA45", "44ICHEC-RCA85", "44IPSL-RCA45",  "44IPSL-RCA85", "44IPSL-WRF45", "44IPSL-WRF85",   "44MPI-CCLM45", "44MPI-CCLM85",  "44MPI-RCA45",  "44MPI-RCA85",    "44MPI-REMO45",   "44MPI-REMO85" )
nn1 <- c("11CNRM-ALADIN45", "11CNRM-ALADIN85", "11CNRM-CCLM45", "11CNRM-CCLM85", "11CNRM-RCA45", "11CNRM-RCA85", "11ICHEC-RACMO45", "11ICHEC-RACMO85", "11ICHEC-RCA45", "11ICHEC-RCA85", "11IPSL-RCA45",  "11IPSL-RCA85", "11IPSL-WRF45", "11IPSL-WRF85", "11MPI-CCLM45", "11MPI-CCLM85",  "11MPI-RCA45",  "11MPI-RCA85",    "11MPI-REMO45",   "11MPI-REMO85")

w_indexes <- lapply(c, stack)
w_indexes1 <- lapply(c1, stack)
w_indexesm <- lapply(cm, stack)
w_indexes1m <- lapply(cm_11, stack)

data('worldMapEnv')

fooborder <- function(data) {
  mycrs <- projection(data)
  
  ext <- as.vector(extent(projectExtent(data, CRS(mycrs))))
  boundaries <- map('world',
                    fill=TRUE, exact=FALSE,
                    xlim=ext[1:2], ylim= ext[3:4],
                    plot=FALSE)
  IDs <- sapply(strsplit(boundaries$names, ":"),
                function(x) x[1])
  boundaries <- map2SpatialPolygons(boundaries,
                                    IDs=IDs,
                                    proj4string=CRS(mycrs))
  border <- boundaries}
#border <- as(boundaries, 'SpatialLines')}


border <- fooborder(w_indexes[[1]])
border1 <- fooborder(w_indexes1[[1]])
borderm <- fooborder(w_indexesm[[1]])
border1m <- fooborder(w_indexes1m[[1]])

## 6. Añado índice temporal a los rasters:

idx <- seq(as.Date("1971-01-01"), as.Date("2100-01-01"), "year")
idxm <- seq(as.Date("1969-01-01"), as.Date("2098-01-01"), "year")

w_indexes <- lapply(w_indexes, FUN=function(x) setZ(x, idx))
w_indexes1 <- lapply(w_indexes1, FUN=function(x) setZ(x, idx))
w_indexesm <- lapply(w_indexesm, FUN=function(x) setZ(x, idx))
w_indexes1m <- lapply(w_indexes1m, FUN=function(x) setZ(x, idx))

##Seleccionamos el dominio que nos interesa:

#1. Mediterráneo

#m <- extent(-25.63, 50.65, 26.73, 52.34)
med <- extent(-12, 40, 28, 50)

w_indexesmed <- lapply(w_indexes, FUN=function(x) mask(x, border))
w_indexesmed1 <- lapply(w_indexes1, FUN=function(x) mask(x, border1))
w_indexesmedm <- lapply(w_indexesm, FUN=function(x) mask(x, border))
w_indexesmed1m <- lapply(w_indexes1m, FUN=function(x) mask(x, border1))

w_indexesmed <- lapply(w_indexesmed, FUN=function(x) crop(x, med))
w_indexesmed1 <- lapply(w_indexesmed1, FUN=function(x) crop(x, med))
w_indexesmedm <- lapply(w_indexesmedm, FUN=function(x) crop(x, med))
w_indexesmed1m <- lapply(w_indexesmed1m, FUN=function(x) crop(x, med))

#1. Norte Europa

ne <- extent(-12, 40, 50, 73) 

w_indexesne <- lapply(w_indexes, FUN=function(x) mask(x, border))
w_indexesne1 <- lapply(w_indexes1, FUN=function(x) mask(x, border1))

w_indexesne <- lapply(w_indexesne, FUN=function(x) crop(x, ne))
w_indexesne1 <- lapply(w_indexesne1, FUN=function(x) crop(x, ne))


##Media de los últimos 30 años MED:

w_indexesmedLast30 <- lapply(w_indexesmed, FUN=function(x) x[[101:130]])
w_indexesmedLast301 <- lapply(w_indexesmed1, FUN=function(x) x[[101:130]])
w_indexesmedLast30m <- lapply(w_indexesmedm, FUN=function(x) x[[101:130]])
w_indexesmedLast301m <- lapply(w_indexesmed1m, FUN=function(x) x[[101:130]])

w_indexesRef <- lapply(w_indexesmed, FUN=function(x) x[[1:30]])

w_indexesmedLast30mean <- lapply(w_indexesmedLast30, FUN=function(x) mean(x, na.rm=TRUE))
w_indexesmedLast30mean1 <- lapply(w_indexesmedLast301, FUN=function(x) mean(x, na.rm=TRUE))
w_indexesmedLast30meanm <- lapply(w_indexesmedLast30m, FUN=function(x) mean(x, na.rm=TRUE))
w_indexesmedLast30mean1m <- lapply(w_indexesmedLast301m, FUN=function(x) mean(x, na.rm=TRUE))

w_indexesRefmean <- lapply(w_indexesRef, FUN=function(x) mean(x, na.rm=TRUE))

##Media de los últimos 30 años NE:

w_indexesneLast30 <- lapply(w_indexesne, FUN=function(x) x[[101:130]])
w_indexesneLast301 <- lapply(w_indexesne1, FUN=function(x) x[[101:130]])

w_indexesRef <- lapply(w_indexesne, FUN=function(x) x[[1:30]])

w_indexesneLast30mean <- lapply(w_indexesneLast30, FUN=function(x) mean(x, na.rm=TRUE))
w_indexesneLast30mean1 <- lapply(w_indexesneLast301, FUN=function(x) mean(x, na.rm=TRUE))

w_indexesRefmean <- lapply(w_indexesRef, FUN=function(x) mean(x, na.rm=TRUE))


meanWSDImed <- stack(unlist(w_indexesmedLast30mean))
meanWSDI1med <- stack(unlist(w_indexesmedLast30mean1))
meanWSDImedm <- stack(unlist(w_indexesmedLast30meanm))
meanWSDI1medm <- stack(unlist(w_indexesmedLast30mean1m))

meanWSDIsinmarmed <- mask(meanWSDImed, border)
meanWSDIsinmar1med <- mask(meanWSDI1med, border1)
meanWSDIsinmarmedm <- mask(meanWSDImedm, borderm)
meanWSDIsinmar1medm <- mask(meanWSDI1medm, border1m)


names(meanWSDIsinmarmed) <- nn
names(meanWSDIsinmar1med) <- nn1

##Uno los stacks con los MOHC
w <- stack(c(meanWSDIsinmarmed,meanWSDIsinmarmedm))
w_11 <- stack(c(meanWSDIsinmar1med,meanWSDIsinmar1medm))

#NE
meanWSDIne <- stack(unlist(w_indexesneLast30mean))
meanWSDI1ne <- stack(unlist(w_indexesneLast30mean1))

meanWSDIsinmarne <- mask(meanWSDIne, border)
meanWSDIsinmar1ne <- mask(meanWSDI1ne, border1)
names(meanWSDIsinmarne) <- nn
names(meanWSDIsinmar1ne) <- nn1

my.at <- seq(0,370, 30)

myColorkey <- list(at=my.at, ## where the colors change
                   labels=list(
                     at=my.at),
                   space="bottom",
                   width= .8)

names(w) <- c("RCP4.5", "RCP8.5", "RCP4.5", "RCP8.5", "RCP4.5", "RCP8.5")

##Figura 1: gráfico de barras
##Calculo la media de todo el dominio para cada modelo 
w_meanM <- lapply(w_indexesmedLast30meanm, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
w_meanM_11 <- lapply(w_indexesmedLast30mean1m, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
w_mean44 <- lapply(w_indexesmedLast30mean, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
w_mean11 <- lapply(w_indexesmedLast30mean1, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

##Calculo la desviación estándar de todo el dominio para cada modelo 
w_sdM <- lapply(w_indexesmedLast30meanm, FUN=function(x) cellStats(x, stat='sd', na.rm=TRUE))
w_sdM_11 <- lapply(w_indexesmedLast30mean1m, FUN=function(x) cellStats(x, stat='sd', na.rm=TRUE))
w_sd44 <- lapply(w_indexesmedLast30mean, FUN=function(x) cellStats(x, stat='sd', na.rm=TRUE))
w_sd11 <- lapply(w_indexesmedLast30mean1, FUN=function(x) cellStats(x, stat='sd', na.rm=TRUE))

#Hago una matriz con los datos de la media obtenidos para poder representar

dfM <- lapply(w_meanM, as.data.frame)
dfM_11 <- lapply(w_meanM_11, as.data.frame)
df44 <- lapply(w_mean44, as.data.frame)
df11 <- lapply(w_mean11, as.data.frame)
w_m<- merge.data.frame(df44, df11, by="row.names")
w_m<- merge.data.frame(w_m, dfM, by="row.names")
w_m<- merge.data.frame(w_m, dfM_11, by="row.names")
w_m<- as.matrix(w_m[-3])
w_m<- as.matrix(w_m[-1])
w_m<- as.matrix(w_m[-1])

mod1 <- c(44, 11,44, 11, 44, 11,44, 11, 11,44, 11,44, 11,44, 11,44, 11,44, 11,44, 11,44, 11, 44, 11, 44,11,11)
mod <- c(44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 11, 11, 11,11,11,11,11,11,11, 11,11,11,11,11,11,11,11,11,11, 11,11,11,11,11, 44, 44, 44, 44, 11, 11, 11, 11, 11, 11, 11)
mod2 <- c(44, 11,44, 11, 44, 11,44, 11, 11,44, 11,44, 11,44, 11,44, 11,44, 11,44, 11,44, 11, 44, 11, 44,11,11,11)


w_M<- matrix(c(w_m, mod), 57, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))


w_M1_45 <- matrix(c(w_M[[1]], w_M[[25]], w_M[[3]], w_M[[23]], w_M[[5]],w_M[[27]], w_M[[7]], w_M[[31]], w_M[[29]], w_M[[9]],w_M[[33]], w_M[[11]], w_M[[35]],w_M[[13]],w_M[[37]],w_M[[15]],w_M[[39]],w_M[[17]],w_M[[41]], w_M[[19]], w_M[[43]], w_M[[21]],w_M[[45]], w_M[[47]], w_M[[51]], w_M[[49]], w_M[[53]], w_M[[55]],mod1), 28, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))
w_M1_85 <- matrix(c(w_M[[2]], w_M[[26]], w_M[[4]], w_M[[24]], w_M[[6]], w_M[[28]],w_M[[8]],w_M[[32]],w_M[[30]],w_M[[10]], w_M[[34]],w_M[[12]],
                    w_M[[36]],w_M[14],w_M[38],w_M[[16]],w_M[[40]],w_M[[18]], w_M[[42]],w_M[[20]], w_M[[44]], w_M[[42]],w_M[[46]],w_M[[48]], w_M[[52]], w_M[[50]],w_M[[54]],w_M[[56]], w_M[[57]], mod2), 29, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))

w_m<- as.data.frame(w_M)
w_M45 <- as.data.frame(w_M1_45)
w_M85 <- as.data.frame(w_M1_85)

#Hago la matriz de la desviación estándar

sdM <- lapply(w_sdM, as.data.frame)
sdM_11 <- lapply(w_sdM_11, as.data.frame)
sd44 <- lapply(w_sd44, as.data.frame)
sd11 <- lapply(w_sd11, as.data.frame)
msd <- merge.data.frame(sd44, sd11, by="row.names")
msd <- merge.data.frame(msd, sdM, by="row.names")
msd <- merge.data.frame(msd, sdM_11, by="row.names")
msd <- as.matrix(msd[-3])
msd <- as.matrix(msd[-1])
msd <- as.matrix(msd[-1])

Msd <- matrix(c(msd, mod), 57, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))


Msd1_45 <- matrix(c(Msd[[1]], Msd[[25]], Msd[[3]], Msd[[23]], Msd[[5]],Msd[[27]], Msd[[7]], Msd[[31]], Msd[[29]], Msd[[9]],Msd[[33]], Msd[[11]], Msd[[35]],Msd[[13]],Msd[[37]],Msd[[15]],Msd[[39]],Msd[[17]],Msd[[41]], Msd[[19]], Msd[[43]], Msd[[21]],Msd[[45]], Msd[[47]], Msd[[51]], Msd[[49]], Msd[[53]], Msd[[55]],mod1), 28, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))

Msd1_85 <- matrix(c(Msd[[2]], Msd[[26]], Msd[[4]], Msd[[24]], Msd[[6]], Msd[[28]],Msd[[8]],Msd[[32]],Msd[[30]],Msd[[10]], Msd[[34]],Msd[[12]],
                    Msd[[36]],Msd[14],Msd[38],Msd[[16]],Msd[[40]],Msd[[18]], Msd[[42]],Msd[[20]], Msd[[44]], Msd[[42]],Msd[[46]],Msd[[48]], Msd[[52]], Msd[[50]],Msd[[54]],Msd[[56]], Msd[[57]], mod2), 29, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))

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


mycols <- c("lightblue", "gray")

w_b45 <- barplot(w_c45, col = mycols[w_M45$resolution], names.arg = nn_44_45, las=2, ylim=c(0,300), cex.names=0.7, main="RCP4.5", ylab="WSDI", legend=(w_M85$resolution[1:2]), args.legend = list(x = 'topright'))
arrows(w_b45, w_c45-sd45, w_b45, w_c45+sd45, angle = 90, code = 3, length=0.05)

w_b85 <- barplot(w_c85, col = mycols[w_M85$resolution], names.arg = nn_44_85, las=2, ylim=c(0,300), cex.names=0.7, main="RCP8.5", ylab="WSDI")
arrows(w_b85, w_c85-sd85, w_b85, w_c85+sd85, angle = 90, code = 3, length=0.05)


##Figura3:

## ordeno para representar 

wmed <- stack(meanWSDIsinmarmed[[1:20]])
wne <- stack(meanWSDIsinmarne[[1:20]])
w1med <- stack(meanWSDIsinmar1med[[1:20]])
w1ne <- stack(meanWSDIsinmar1ne[[1:20]])
w_11_85 <- stack(w_11[[14]],w_11[[28]], w_11[[6]], w_11[[22]],w_11[[16]],w_11[[8]],w_11[[30]],w_11[[4]],w_11[[20]],w_11[[18]],w_11[[10]],w_11[[31]], w_11[[2]],w_11[[24]],w_11[[26]],w_11[[12]])


levelplot(w_11_85, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("WSDI"), cex=1), xlab=NULL, ylab=list(c( ""), cex=1), names.attr=c("ECEARTH-RCA", "HadGEM-RCA", "CNRM-RCA", "MPI-RCA", "IPSL-RCA", "ECEARTH-CCLM", "HadGEM-CCLM","CNRM-CCLM", "MPI-CCLM", "IPSL-WRF", "ECEARTH-HIRHAM", "HadGEM-HIRHAM","CNRM-ALADIN", "MPI-REMO", "HadGEM-RACMO", "ECEARTH-RACMO"), layout=c(5,4), colorkey=myColorkey)+
  layer(sp.lines(border, lwd=0.5))


## RCA44
wmedrca <- stack(meanWSDIsinmarmed[[1]], meanWSDIsinmarmed[[2]], meanWSDIsinmarmed[[5]], meanWSDIsinmarmed[[6]], meanWSDIsinmarmed[[9]], meanWSDIsinmarmed[[10]], meanWSDIsinmarmed[[11]], meanWSDIsinmarmed[[12]], meanWSDIsinmarmed[[17]], meanWSDIsinmarmed[[18]])
wnerca <- stack(meanWSDIsinmarne[[1]], meanWSDIsinmarne[[2]], meanWSDIsinmarne[[5]], meanWSDIsinmarne[[6]], meanWSDIsinmarne[[9]], meanWSDIsinmarne[[10]], meanWSDIsinmarne[[11]], meanWSDIsinmarne[[12]], meanWSDIsinmarne[[17]], meanWSDIsinmarne[[18]])

pdf("wsdimed44rca.pdf")
levelplot(wmedrca, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c( "RCA"), cex=1), names.attr=c("CanESM-RCA", "CanESM-RCA", "CNRM-RCA", "CNRM-RCA","ECEARTH-RCA","ECEARTH-RCA", "IPSL-RCA", "IPSL-RCA", "MPI-RCA", "MPI-RCA"), layout=c(2,5), colorkey=myColorkey)+
  layer(sp.lines(border, lwd=0.5))
dev.off()

pdf("wsdine44.pdf")
levelplot(wnerca, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c("RCA"), cex=1), names.attr=c("CanESM-RCA", "CanESM-RCA", "CNRM-RCA", "CNRM-RCA","ECEARTH-RCA","ECEARTH-RCA", "IPSL-RCA", "IPSL-RCA", "MPI-RCA", "MPI-RCA"), layout=c(2,5), colorkey=myColorkey)+
  layer(sp.lines(border, lwd=0.5))
dev.off()

## RCA 11
w1medrca <- stack(meanWSDIsinmar1med[[5]], meanWSDIsinmar1med[[6]], meanWSDIsinmar1med[[9]], meanWSDIsinmar1med[[10]], meanWSDIsinmar1med[[11]], meanWSDIsinmar1med[[12]],  meanWSDIsinmar1med[[17]], meanWSDIsinmar1med[[18]])
w1nerca <- stack(meanWSDIsinmar1ne[[5]], meanWSDIsinmar1ne[[6]], meanWSDIsinmar1ne[[9]], meanWSDIsinmar1ne[[10]], meanWSDIsinmar1ne[[11]], meanWSDIsinmar1ne[[12]], meanWSDIsinmar1ne[[17]], meanWSDIsinmar1ne[[18]])

pdf("wsdimed11rca.pdf")
levelplot(w1medrca, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c("RCA"), cex=1), names.attr=c("CNRM-RCA", "CNRM-RCA","ECEARTH-RCA","EC-EARTH-RCA", "IPSL-RCA", "IPSL-RCA", "MPI-RCA", "MPI-RCA"), layout=c(2,4), colorkey=myColorkey)+
  layer(sp.lines(border, lwd=0.5))
dev.off()

pdf("wsdine11.pdf")
levelplot(w1nerca, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c("RCA"), cex=1), names.attr=c("CNRM-RCA", "CNRM-RCA","ECEARTH-RCA","EC-EARTH-RCA", "IPSL-RCA", "IPSL-RCA", "MPI-RCA", "MPI-RCA"), layout=c(2,4), colorkey=myColorkey)+
  layer(sp.lines(border, lwd=0.5))
dev.off()

## CNRM 44
wmedcnrm <- stack(meanWSDIsinmarmed[[3:6]])
wnecnrm <- stack(meanWSDIsinmarne[[3:6]])

pdf("wsdimed44cnrm.pdf")
levelplot(wmedcnrm, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c( "", "" ), cex=1), names.attr=c("CNRM-ALADIN", "CNRM-ALADIN","CNRM-RCA", "CNRM-RCA" ), layout=c(2,2), colorkey=myColorkey)+
  layer(sp.lines(border, lwd=0.5))
dev.off()

pdf("wsdine44cnrm.pdf")
levelplot(wnecnrm, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c( "", ""), cex=1), names.attr=c("CNRM-ALADIN", "CNRM-ALADIN", "CNRM-RCA", "CNRM-RCA"), layout=c(2,2), colorkey=myColorkey)+
  layer(sp.lines(border, lwd=0.5))

## CNRM 11
w1medcnrm <- stack(meanWSDIsinmar1med[[1:6]]) 
w1necnrm <- stack(meanWSDIsinmar1ne[[1:6]]) 

pdf("wsdimed11cnrm.pdf")
levelplot(w1medcnrm, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c( "", "", "" ), cex=1), names.attr=c("CNRM-ALADIN", "CNRM-ALADIN","CNRM-CCLM", "CNRM-CCLM","CNRM-RCA", "CNRM-RCA" ), layout=c(2,3), colorkey=myColorkey)+
  layer(sp.lines(border, lwd=0.5))
dev.off()

pdf("wsdine11cnrm.pdf")
levelplot(w1necnrm, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c( "", "", ""), cex=1), names.attr=c("CNRM-ALADIN", "CNRM-ALADIN", "CNRM-CCLM", "CNRM-CCLM","CNRM-RCA", "CNRM-RCA"), layout=c(2,3), colorkey=myColorkey)+
  layer(sp.lines(border, lwd=0.5))

# Represento:

pdf("wsdimed44.pdf")
levelplot(wmed, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85", "RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c(), cex=1), names.attr=c("CanESM-RCA", "CanESM-RCA", "CNRM-ALADIN", "CNRM-ALADIN", "CNRM-RCA", "CNRM-RCA","ECEARTH-RACMO", "ECEARTH-RACMO","EC-EARTH-RCA","EC-EARTH-RCA", "IPSL-RCA", "IPSL-RCA", "IPSL-WRF", "IPSL-WRF", "MPI-CCLM","MPI-CCLM", "MPI-RCA", "MPI-RCA", "MPI-REMO", "MPI-REMO"), layout=c(4,5), colorkey=myColorkey)+
  layer(sp.lines(border, lwd=0.5))
dev.off()

pdf("wsdine44.pdf")
levelplot(wne, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85", "RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c(), cex=1), names.attr=c("CanESM-RCA", "CanESM-RCA", "CNRM-ALADIN", "CNRM-ALADIN", "CNRM-RCA", "CNRM-RCA","ECEARTH-RACMO", "ECEARTH-RACMO","EC-EARTH-RCA","EC-EARTH-RCA", "IPSL-RCA", "IPSL-RCA", "IPSL-WRF", "IPSL-WRF", "MPI-CCLM","MPI-CCLM", "MPI-RCA", "MPI-RCA", "MPI-REMO", "MPI-REMO"), layout=c(4,5), colorkey=myColorkey)+
  layer(sp.lines(border, lwd=0.5))
dev.off()

pdf("wsdimed11.pdf")
levelplot(w1med, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85", "RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c(), cex=1), names.attr=c("CNRM-ALADIN", "CNRM-ALADIN", "CNRM-CCLM","CNRM-CCLM","CNRM-RCA", "CNRM-RCA","ECEARTH-RACMO", "ECEARTH-RACMO","EC-EARTH-RCA","EC-EARTH-RCA", "IPSL-RCA", "IPSL-RCA", "IPSL-WRF", "IPSL-WRF", "MPI-CCLM","MPI-CCLM", "MPI-RCA", "MPI-RCA", "MPI-REMO", "MPI-REMO"), layout=c(4,5), colorkey=myColorkey)+
  layer(sp.lines(border, lwd=0.5))
dev.off()

pdf("wsdine11.pdf")
levelplot(w1ne, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85", "RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c(), cex=1), names.attr=c("CNRM-ALADIN", "CNRM-ALADIN","CNRM-CCLM","CNRM-CCLM", "CNRM-RCA", "CNRM-RCA","ECEARTH-RACMO", "ECEARTH-RACMO","EC-EARTH-RCA","EC-EARTH-RCA", "IPSL-RCA", "IPSL-RCA", "IPSL-WRF", "IPSL-WRF", "MPI-CCLM","MPI-CCLM", "MPI-RCA", "MPI-RCA", "MPI-REMO", "MPI-REMO"), layout=c(4,5), colorkey=myColorkey)+
  layer(sp.lines(border, lwd=0.5))
dev.off()

## Barplot WSDI

bmed <- brick(w_indexesmedLast30mean)
bmed <- cellStats(bmed, mean)
pdf("wsdimedbarplot44.pdf", width=7, height=4)
bmed <- barplot(bmed, col=c('grey','orange'), las=2, ylab="WSDI", main="MED 0.44", legend.text= c("RCP45", "RCP85"), names.arg= nn)
dev.off()

bne <- brick(w_indexesneLast30mean)
bne <- cellStats(bne, mean)
pdf("wsdibarplot44.pdf", width=7, height=4)
bne <- barplot(bne, col=c('grey','orange'), las=2, ylab="WSDI", main="NE 0.44", legend.text= c("RCP45", "RCP85"), names.arg= nn)
dev.off()

bmed1 <- brick(w_indexesmedLast30mean1)
bmed1 <- cellStats(bmed1, mean)
pdf("wsdimedbarplot11.pdf", width=7, height=4)
bmed1 <- barplot(bmed1, col=c('grey','orange'), las=2, ylab="WSDI", main="MED 0.11", legend.text= c("RCP45", "RCP85"), names.arg= nn1)
dev.off()

bne1 <- brick(w_indexesneLast30mean1)
bne1 <- cellStats(bne1, mean)
pdf("wsdinebarplot11.pdf", width=7, height=4)
bne1 <- barplot(bne1, col=c('grey','orange'), las=2, ylab="WSDI", main="NE 0.11", legend.text= c("RCP45", "RCP85"), names.arg= nn1)
dev.off()

pdf("wsdibarplotGCMprint.pdf")
print(bmed, split=c(1, 1, 2, 1), more=TRUE)
print(bmed1, split=c(1, 2, 2, 1))
dev.off()


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

#pdf("gridwsdi44.pdf")
#grid.arrange(pi,eurs, mede,afrw,afr, ncol = 3, nrow=2, main = "Main title")

#grid <- expand.grid(x=x, y=y)
#pdf("gridwsdi44.pdf", width=7, height=7)
#grid.arrange(pi,eurs,mede,afrw,afr, ncol=3, nrow=2)
#dev.off()

pi <- xyplot(pimatrix, superpose=TRUE, type='b', cex=0.8, lwd=1.5, par.settings=myTheme, ylim=c(0,250),main="PI",
             auto.key = list(corner=c(0.95, 1), x = 0.5, y = 0.93))
eurs <- xyplot(pimatrix, superpose=TRUE, type='b', cex=0.8, lwd=1.5, par.settings=myTheme, ylim=c(0,250), main="EURS",
               auto.key = FALSE)
mede <- xyplot(pimatrix, superpose=TRUE, type='b', cex=0.8, lwd=1.5, par.settings=myTheme,ylim=c(0,250),main="MEDE",
               auto.key = FALSE)
afrw <- xyplot(pimatrix, superpose=TRUE, type='b', cex=0.8, lwd=1.5, par.settings=myTheme,ylim=c(0,250),main="AFRW",
               auto.key = FALSE)
afr <- xyplot(pimatrix, superpose=TRUE, type='b', cex=0.8, lwd=1.5, par.settings=myTheme, ylim=c(0,250),main="AFR",
              auto.key = FALSE)
par()
dev.off()

