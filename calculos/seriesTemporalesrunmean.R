##Para hacer series temporales de la media móvil de hwmid
##Primero hay que calcular runmean,5 con cdo, y con los archivos de salida, corremos este script.

library(raster)
library(rasterVis)
library(graticule)
library(rgdal)
library(maps)
library(mapdata)
library(maptools)

crun <- list("hwmid44CNRM-ALADIN45_adapta_runmean.nc", "hwmid44CNRM-ALADIN85_adapta_runmean.nc", "hwmid11CNRM-ALADIN45_adapta_runmean.nc", "hwmid11CNRM-ALADIN85_adapta_runmean.nc")

nn <- list("44CNRM-ALADIN45", "44CNRM-ALADIN85", "11CNRM-ALADIN45", "11CNRM-ALADIN85")

runmean <- lapply(crun, stack)
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

border <- fooborder(runmean[[1]])

t <- seq(as.Date("1973-01-01"), as.Date("2098-01-01"), "year")

hwmids <- lapply(runmean, FUN=function(x) setZ(x, t))
e <- extent(-10, 6, 35,44)
piseries <- lapply(runmean, FUN=function(x) mask(x, border))

## 1. MEDW
e <- extent(-10, 6, 35,44)

#pi <- crop(meanWSDIsinmar, e)
piseries <- lapply(hwmids, FUN=function(x) mask(x, border))

piseries <- lapply(piseries, FUN=function(x) crop(x, e))

piseries <- lapply(piseries, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

pimatrix <- do.call(cbind, piseries)
library(zoo)
z <- as.zoo(pimatrix)

library(reshape2)
pimatrix <- as.data.frame(pimatrix)
colnames(pimatrix) <- nn
pimatrix <- as.zoo(pimatrix)

myTheme <- custom.theme.2(pch = 20, cex = 0.7, alpha=0.7)
myTheme$strip.background$col <- 'transparent'
myTheme$strip.shingle$col <- 'transparent'

pdf("piseries.pdf")
pi <- xyplot(pimatrix, superpose=TRUE, type='b', cex=0.8, lwd=1.5, par.settings=myTheme, ylim=c(0,50),main="PI",
             auto.key = list(corner=c(0.95, 1), x = 0.25, y = 0.95))