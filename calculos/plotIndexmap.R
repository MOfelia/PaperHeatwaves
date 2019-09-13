
## 1. cargo las librerías necesarias para leer raster y representar:

library(raster)
library(rasterVis)
library(graticule)
library(rgdal)
library(maps)
library(mapdata)
library(maptools)


## 2. leer los datos

lista <- dir(path="../kike/gcm/", pattern="^hwmid30avg.*lonlat.*\\.nc$")
  
lista <- lapply(lista, FUN=function(x) paste("../kike/gcm/", x, sep=''))

## creo un vector con los nombres de las capas.

nn <- c("RACMO-RCP45-EC-EARTH", "RACMO-RCP85EC-EARTH", "RCA4-RCP45-CCC", "RCA4-RCP85-CCC", "RCA4-RCP45-ECEARTH", "RCA4-RCP85-ECEARTH")

## 3. creo una lista de rasterStacks con cada .nc

indexes <- lapply(lista, stack)

## 4. Puedo acceder a cada uno de los componentes de esta lista por separado para representarlos o a la vez.

## 5. creo las líneas de países:

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


border <- fooborder(indexes[[1]])

## 6. Añado índice temporal a los rasters:
 
idx <- seq(as.Date("1971-01-01"), as.Date("2100-01-01"), "year")
indexes <- lapply(indexes, FUN=function(x) setZ(x, idx))

## 7.selecciono los 30 últimos años y hago la media:

which(getZ(indexes[[1]])== "2071-01-01") ## 101
which(getZ(indexes[[1]])== "1971-01-01") ## 1
which(getZ(indexes[[1]])== "2000-01-01") ## 30

indexesLast30 <- lapply(indexes, FUN=function(x) x[[101:130]])
indexesRef <- lapply(indexes, FUN=function(x) x[[1:30]])

## 7.1 media

indexesLast30mean <- lapply(indexesLast30, FUN=function(x) mean(x, na.rm=TRUE))
indexesRefmean <- lapply(indexesRef, FUN=function(x) mean(x, na.rm=TRUE))

## 8. REPRESENTO:

## todas

meanWSDI <- stack(unlist(indexesLast30mean))

meanWSDIsinmar <- mask(meanWSDI, border)
names(meanWSDIsinmar) <- nn

## ordeno para representar
w <- stack(meanWSDIsinmar[[5]],meanWSDIsinmar[[6]], meanWSDIsinmar[[3]], meanWSDIsinmar[[4]], meanWSDIsinmar[[1]], meanWSDIsinmar[[2]])

my.at <- seq(0,370, 30)

myColorkey <- list(at=my.at, ## where the colors change
                   labels=list(
                       at=my.at),
                   space="bottom",
                   width= .8)
 
names(w) <- c("RCP4.5", "RCP8.5", "RCP4.5", "RCP8.5", "RCP4.5", "RCP8.5")

 
pdf("wsdi20712100_4.pdf", width=7, height=4)
p <- levelplot(w, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("EC-EARTH", "CCC"), cex=1), xlab=NULL, ylab=list(c("RACMO", "RCA4"), cex=1), names.attr=c("RCP4.5", "RCP8.5", "RCP4.5", "RCP8.5", "RCP4.5", "RCP8.5"), layout=c(4,2), colorkey=myColorkey)+
    layer(sp.lines(border, lwd=0.5))
dev.off()
 
## periodo referencia:

meanWSDIref <- stack(unlist(indexesRefmean))

meanWSDIrefsinmar <- mask(meanWSDIref, border)
names(meanWSDIrefsinmar) <- nn

y <- stack(meanWSDIrefsinmar[[5]],meanWSDIrefsinmar[[6]], meanWSDIrefsinmar[[3]], meanWSDIrefsinmar[[4]], meanWSDIrefsinmar[[1]], meanWSDIrefsinmar[[2]])

my.at <- seq(0,16,2)

myColorkey <- list(at=my.at, ## where the colors change
                   space="bottom",
                   width= .8)



pdf("wsdiRef.pdf", width=7, height=4)
p2 <- levelplot(y, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)), xlab.top=list(c("EC-EARTH", "CCC"), cex=1), xlab=NULL, ylab=list(c("RACMO", "RCA4"), cex=1), names.attr=c("RCP4.5", "RCP8.5", "RCP4.5", "RCP8.5", "RCP4.5", "RCP8.5"), layout=c(4,2),  par.settings=viridisTheme, at=my.at, colorkey=list(space="bottom", width=0.8))+
    layer(sp.lines(border, lwd=0.5))
dev.off()

pdf("wsdiall3.pdf")
print(p2, split=c(1, 1, 1, 2), more=TRUE)
print(p, split=c(1, 2, 1, 2))
dev.off()


## ____________________________________________________________-

## HWMI

HWMI <- stack(unlist(indexes))

HWMIsinmar <- mask(HWMI, border)
names(HWMIsinmar) <- nn

## ordeno para representar
w <- stack(HWMIsinmar[[5]],HWMIsinmar[[6]], HWMIsinmar[[3]], HWMIsinmar[[4]], HWMIsinmar[[1]], HWMIsinmar[[2]])
 
my.at <- seq(0,40,5)
my.at <- c(my.at, 50, 75, 100, 125,150)
w[w[]>150] <- 150
length(w[w[] > 150]) <- HWMIsinmar
HWMIsinmar[HWMIsinmar[]>150] <- 150

myColorkey <- list(at=my.at, ## where the colors change
                   space="bottom",
                   width= .8)


div.pal <- brewer.pal(n=9, 'Set3')
 
div.pal <- brewer.pal(n=9, 'RdYlGn')


pdf("hwmid44rcm.pdf", width=7, height=4)
levelplot(w, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("EC-EARTH", "CCC"), cex=1), xlab=NULL, ylab=list(c("RACMO", "RCA4"), cex=1), names.attr=c("RCP4.5", "RCP8.5", "RCP4.5", "RCP8.5", "RCP4.5", "RCP8.5"), layout=c(4,2), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at)))+
    layer(sp.lines(border, lwd=0.5))
dev.off()

## 11

pdf("hwmidGCM.pdf", width=7, height=4)
levelplot(HWMIsinmar, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("CCC"), cex=1), xlab=NULL, ylab=NULL, names.attr=c("RCP4.5", "RCP8.5"),par.settings=rasterTheme(region=rev(div.pal)), layout=c(2,1), at=my.at,colorkey=list(space='bottom', labels=list(at=my.at)))+
    layer(sp.lines(border, lwd=0.5))
dev.off()
 
## __________________________________________________________

## selecciono zonas:

## 1. MEDW
e <- extent(-10, 6, 35,44)

pi <- crop(meanWSDIsinmar, e)
 
pdf("wsdiPI2.pdf")
levelplot(pi, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)), at=my.at, xlab=NULL, ylab=NULL, layout=c(3,2),colorkey=list(space="bottom"))+
    layer(sp.lines(border, lwd=0.5))
dev.off()

## series temporales

piseries <- lapply(indexes, FUN=function(x) mask(x, border))

piseries <- lapply(piseries, FUN=function(x) crop(x, e))

piseries <- lapply(piseries, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

pimatrix <- do.call(cbind, piseries)
z <- as.zoo(pimatrix)

library(reshape2)
pimatrix <- as.data.frame(pimatrix)
colnames(pimatrix) <- nn
pimatrix <- as.zoo(pimatrix)

myTheme <- custom.theme.2(pch = 20, cex = 0.7, alpha=0.7)
myTheme$strip.background$col <- 'transparent'
myTheme$strip.shingle$col <- 'transparent'
 
pdf("piseries.pdf")
pi <- xyplot(pimatrix, superpose=TRUE, type='b', cex=0.8, lwd=1.5, par.settings=myTheme, ylim=c(0,250),main="PI",
       auto.key = list(corner=c(0.95, 1), x = 0.65, y = 0.95))
dev.off() 

## para colocar leyenda:
auto.key = list(corner=c(0.95, 1), x = 0.8, y = 0.93)
## 2. EURS

e <- extent(6, 26, 35,44)

piseries <- lapply(indexes, FUN=function(x) mask(x, border))

piseries <- lapply(piseries, FUN=function(x) crop(x, e))

piseries <- lapply(piseries, FUN=function(x) cellStats(x, stat='mean'))

pimatrix <- do.call(cbind, piseries)
z <- as.zoo(pimatrix)
 
library(reshape2)
pimatrix <- as.data.frame(pimatrix)
colnames(pimatrix) <- nn
pimatrix <- as.zoo(pimatrix)


pdf("eursmatrix.pdf")
eurs <- xyplot(pimatrix, superpose=TRUE, type='b', cex=0.8, lwd=1.5, par.settings=myTheme, ylim=c(0,250), main="EURS",
       auto.key =FALSE)
dev.off() 
 
## 3. MEDE

e <- extent(26,41,25,44)

piseries <- lapply(indexes, FUN=function(x) mask(x, border))

piseries <- lapply(piseries, FUN=function(x) crop(x, e))

piseries <- lapply(piseries, FUN=function(x) cellStats(x, stat='mean'))

pimatrix <- do.call(cbind, piseries)
z <- as.zoo(pimatrix)

library(reshape2)
pimatrix <- as.data.frame(pimatrix)
colnames(pimatrix) <- nn
pimatrix <- as.zoo(pimatrix)

pdf("medematrix.pdf")
mede <- xyplot(pimatrix, superpose=TRUE, type='b', cex=0.8, lwd=1.5, par.settings=myTheme,ylim=c(0,250),main="MEDE",
       auto.key = FALSE)
dev.off() 

## 4.AFRW

e <- extent(-10,6,25,35)

piseries <- lapply(indexes, FUN=function(x) mask(x, border))

piseries <- lapply(piseries, FUN=function(x) crop(x, e))

piseries <- lapply(piseries, FUN=function(x) cellStats(x, stat='mean'))

pimatrix <- do.call(cbind, piseries)
z <- as.zoo(pimatrix)

library(reshape2)
pimatrix <- as.data.frame(pimatrix)
colnames(pimatrix) <- nn
pimatrix <- as.zoo(pimatrix)

pdf("afrwmatrix.pdf")
afrw <- xyplot(pimatrix, superpose=TRUE, type='b', cex=0.8, lwd=1.5, par.settings=myTheme,ylim=c(0,250),main="AFRW",
       auto.key = FALSE)
dev.off()

## 5. AFR

e <- extent(6,26,25,35)

piseries <- lapply(indexes, FUN=function(x) mask(x, border))

piseries <- lapply(piseries, FUN=function(x) crop(x, e))

piseries <- lapply(piseries, FUN=function(x) cellStats(x, stat='mean'))

pimatrix <- do.call(cbind, piseries)
z <- as.zoo(pimatrix)

library(reshape2)
pimatrix <- as.data.frame(pimatrix)
colnames(pimatrix) <- nn
pimatrix <- as.zoo(pimatrix)

pdf("afrmatrix.pdf")
afr <- xyplot(pimatrix, superpose=TRUE, type='b', cex=0.8, lwd=1.5, par.settings=myTheme, ylim=c(0,250),main="AFR",
       auto.key = FALSE)
dev.off()

idx <- seq(as.Date("1971-01-01"), as.Date("2100-01-01"), "year")

pdf("print.pdf", width=15, height=7)
print(pi, split=c(1, 1, 3, 2), more=TRUE)
print(eurs, split=c(2, 1, 3, 2), more=TRUE)
print(afrw, split=c(1, 2, 3, 2), more=TRUE)
print(afr, split=c(2, 2, 3, 2), more=TRUE)
print(mede, split=c(3, 2, 3, 2))
dev.off()



pdf("grid.pdf")
grid.arrange(pi,eurs, mede,afrw,afr, ncol = 3, nrow=2, main = "Main title")

grid <- expand.grid(x=x, y=y)
pdf("grid.pdf", width=7, height=7)
grid.arrange(pi,eurs,mede,afrw,afr, ncol=3, nrow=2)
dev.off()

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
 
