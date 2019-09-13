### Series temporales HWMId 0.44. Están hechas con el archivo global, no MED ni NE

c =list("hwmid44CCC-RCA45_adapta_runmean.nc", "hwmid44CCC-RCA85_adapta_runmean.nc", "hwmid44CNRM-ALADIN45K_adapta_runmean.nc", "hwmid44CNRM-ALADIN85K_adapta_runmean.nc", "hwmid44CNRM-RCA45_adapta_runmean.nc", "hwmid44CNRM-RCA85_adapta_runmean.nc","hwmid44ICHEC-RACMO45_adapta_runmean.nc", "hwmid44CHEC-RACMO85_adapta_runmean.nc", "hwmid44ICHEC-RCA45_adapta_runmean.nc", "hwmid44ICHEC-RCA85_adapta_runmean.nc", "hwmid44IPSL-RCA45_adapta_runmean.nc","hwmid44IPSL-RCA85_adapta_runmean.nc", "hwmid44IPSL-WRF45_adapta_runmean.nc","hwmid44IPSL-WRF85_adapta_runmean.nc","hwmid44MPI-CCLM45_adapta_runmean.nc","hwmid44MPI-CCLM85_adapta_runmean.nc", "hwmid44MPI-RCA45_adapta_runmean.nc", "hwmid44MPI-RCA85_adapta_runmean.nc", "hwmid44MPI-REMO45_adapta_runmean.nc", "hwmid44MPI-REMO85_adapta_runmean.nc")
c_44_45 =list("hwmid44CCC-RCA45_adapta_runmean.nc", "hwmid44CNRM-ALADIN45K_adapta_runmean.nc",  "hwmid44CNRM-RCA45_adapta_runmean.nc", "hwmid44ICHEC-RACMO45_adapta_runmean.nc", "hwmid44ICHEC-RCA45_adapta_runmean.nc","hwmid44IPSL-RCA45_adapta_runmean.nc", "hwmid44IPSL-WRF45_adapta_runmean.nc","hwmid44MPI-CCLM45_adapta_runmean.nc", "hwmid44MPI-RCA45_adapta_runmean.nc", "hwmid44MPI-REMO45_adapta_runmean.nc")
c_44_85 =list("hwmid44CCC-RCA85_adapta_runmean.nc", "hwmid44CNRM-ALADIN85K_adapta_runmean.nc",  "hwmid44CNRM-RCA85_adapta_runmean.nc", "hwmid44CHEC-RACMO85_adapta_runmean.nc", "hwmid44ICHEC-RCA85_adapta_runmean.nc","hwmid44IPSL-RCA85_adapta_runmean.nc", "hwmid44IPSL-WRF85_adapta_runmean.nc","hwmid44MPI-CCLM85_adapta_runmean.nc", "hwmid44MPI-RCA85_adapta_runmean.nc", "hwmid44MPI-REMO85_adapta_runmean.nc")
##Abro los archivos con stack:

indexes <- lapply(c, stack)
indexes_44_45 <- lapply(c_44_45, stack)
indexes_44_85 <- lapply(c_44_85, stack)

## creo un vector con los nombres de las capas.

nn <- c("44CCC-RCA45", "44CCC-RCA85", "44CNRM-ALADIN45", "44CNRM-ALADIN85", "44CNRM-RCA45", "44CNRM-RCA85", "44ICHEC-RACMO45", "44ICHEC-RACMO85",  "44ICHEC-RCA45", "44ICHEC-RCA85", "44IPSL-RCA45", "44IPSL-RCA85",  "44IPSL-WRF45", "44IPSL-WRF85","44MPI-CCLM45", "44MPI-CCLM85",  "44MPI-RCA45", "44MPI-RCA85",  "44MPI-REMO45", "44MPI-REMO85")
nn_44_45 <- c("44CCC-RCA45", "44CNRM-ALADIN45", "44CNRM-RCA45",  "44ICHEC-RACMO45", "44ICHEC-RCA45", "44IPSL-RCA45", "44IPSL-WRF45", "44MPI-CCLM45", "44MPI-RCA45", "44MPI-REMO45")
nn_44_85 <- c("44CCC-RCA85", "44CNRM-ALADIN85", "44CNRM-RCA85",  "44ICHEC-RACMO85", "44ICHEC-RCA85", "44IPSL-RCA85", "44IPSL-WRF85", "44MPI-CCLM85", "44MPI-RCA85", "44MPI-REMO85")

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

border <- fooborder(indexes[[1]])
border_44_45 <- fooborder(indexes_44_45[[1]])
border_44_85 <- fooborder(indexes_44_85[[1]])

##Creo un indice temporal para hacer SetZ que me pide la función xyplot:

idx <- seq(as.Date("1971-01-01"), as.Date("2100-01-01"), "year")

indexes <- lapply(indexes, FUN=function(x) setZ(x, idx))
indexes_44_45 <- lapply(indexes_44_45, FUN=function(x) setZ(x, idx))
indexes_44_85 <- lapply(indexes_44_85, FUN=function(x) setZ(x, idx))

## 1. MEDW
e <- extent(-10, 6, 35,44)

#pi <- crop(meanWSDIsinmar, e)
piseries <- lapply(indexes, FUN=function(x) mask(x, border))

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

pdf("piseriesmed44.pdf")
pi <- xyplot(pimatrix, superpose=TRUE, type='b', cex=0.8, lwd=1.5, par.settings=myTheme, ylim=c(0,250),main="PI",
             auto.key = FALSE)
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


pdf("eursmatrix44.pdf")
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

pdf("medematrix44.pdf")
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

pdf("afrwmatrix44.pdf")
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

pdf("afrmatrix44.pdf")
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