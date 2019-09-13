### Series temporales HWMId. Están hechas con el archivo global, no MED ni NE
## Cargo los paquetes necesarios

library("raster")
library("rasterVis")
library("graticule")
library("rgdal")
library("maps")
library("mapdata")
library("maptools")
library("zoo")
library("rworldmap")

## leer los datos

#lista <- dir(pattern="^hwmid11.*85_adapta_runmean\\.nc$")

#lista <- lapply(lista, FUN=function(x) paste("../mofelia/taller/", x, sep=''))

## creo un vector con los nombres de las capas en el orden en el que están cargados los datos para que coincidan

nn_11_85 <- c("CNRM-ALADIN", "CNRM-CCLM", "CNRM-RCA", "ICHEC-RACMO", "ICHEC-RCA", "IPSL-RCA", "IPSL-WRF", "MPI-CCLM", "MPI-RCA", "MPI-REMO")
nn_11_85m <- c("HadGEM-RACMO", "HadGEM-RCA")

lista <- paste("hwmid11",nn_11_85,"85_adapta_runmean.nc",sep="")
listam <- paste("hwmid11",nn_11_85m,"85_adapta_runmean.nc",sep="")

##Abro los archivos con stack:

h_indexes_11_85 <- lapply(lista, stack)
hm_indexes_11_85 <- lapply(listam, stack)


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


## 1. PI
#e1 <- e1 + e2
#ext1<- c(-12, 0, 36, 38)
#ext2<- c(-12, 5, 38, 44)
#ext <- cbind(ext1, ext2)
ext1 <- sp1 + sp2

piseries <- lapply(h_indexes_11_85, FUN=function(x) mask(x, border))
piseriesm <- lapply(hm_indexes_11_85, FUN=function(x) mask(x, border))

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



myTheme <- custom.theme.2(pch = 20, cex = 0.7, alpha=0.7)
myTheme$strip.background$col <- 'transparent'
myTheme$strip.shingle$col <- 'transparent'

pdf("piseriesHWMId11_85.pdf")
pi <- xyplot(pimatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,150),main=list("PI",cex=0.8), xlab=NULL,
             auto.key = FALSE, scales=list(
               x=list(labels=NULL)))
dev.off()

## para colocar leyenda:
auto.key = list(corner=c(0.95, 1), x = 0.8, y = 0.93)

## 2. FR

frseries <- lapply(h_indexes_11_85, FUN=function(x) mask(x, border))
frseriesm <- lapply(hm_indexes_11_85, FUN=function(x) mask(x, border))

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

