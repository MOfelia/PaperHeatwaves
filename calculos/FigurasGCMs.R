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

c1 <- list("hwmidCanESM45box.nc", "hwmidCanESM85box.nc")
c2 <- list("hwmidCNRM45box.nc", "hwmidCNRM85box.nc")
c3 <- list("hwmidHadGEM45box.nc", "hwmidHadGEM85box.nc")
c4 <- list("hwmidEC-EARTH45box.nc")
c5 <- list("hwmidIPSL45box.nc", "hwmidIPSL85box.nc")
c6 <- list("hwmidMPI45box.nc", "hwmidMPI85box.nc")

nn1 <- c("CanESM", "CanESM")
nn2 <- c("CNRM", "CNRM")
nn3 <- c("HadGEM45", "HadGEM85")
nn4 <- c("EC-EARTH")
nn5 <- c("IPSL", "IPSL")
nn6 <- c("MPI", "MPI")

hg_indexes1 <- lapply(c1, stack)
hg_indexes2 <- lapply(c2, stack)
hg_indexes3 <- lapply(c3, stack)
hg_indexes4 <- lapply(c4, stack)
hg_indexes5 <- lapply(c5, stack)
hg_indexes6 <- lapply(c6, stack)

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

border1 <- fooborder(hg_indexes1[[1]])
border2 <- fooborder(hg_indexes2[[1]])
border3 <- fooborder(hg_indexes3[[1]])
border4 <- fooborder(hg_indexes4[[1]])
border5 <- fooborder(hg_indexes5[[1]])
border6 <- fooborder(hg_indexes6[[1]])

##Creo un indice temporal para hacer SetZ que me pide la función xyplot:

t <- seq(as.Date("1971-01-01"), as.Date("2100-01-01"), "year")

hg_indexes1 <- lapply(hg_indexes1, FUN=function(x) setZ(x, t))
hg_indexes2 <- lapply(hg_indexes2, FUN=function(x) setZ(x, t))
hg_indexes3 <- lapply(hg_indexes3, FUN=function(x) setZ(x, t))
hg_indexes4 <- lapply(hg_indexes4, FUN=function(x) setZ(x, t))
hg_indexes5 <- lapply(hg_indexes5, FUN=function(x) setZ(x, t))
hg_indexes6 <- lapply(hg_indexes6, FUN=function(x) setZ(x, t))

#1. Mediterráneo

#m <- extent(-25.63, 50.65, 26.73, 52.34)
med <- extent(-12, 40, 28, 50)

hg_indexesmed1 <- lapply(hg_indexes1, FUN=function(x) mask(x, border1))
hg_indexesmed2 <- lapply(hg_indexes2, FUN=function(x) mask(x, border2))
hg_indexesmed3 <- lapply(hg_indexes3, FUN=function(x) mask(x, border3))
hg_indexesmed4 <- lapply(hg_indexes4, FUN=function(x) mask(x, border4))
hg_indexesmed5 <- lapply(hg_indexes5, FUN=function(x) mask(x, border5))
hg_indexesmed6 <- lapply(hg_indexes6, FUN=function(x) mask(x, border6))

hg_indexesmed1 <- lapply(hg_indexesmed1, FUN=function(x) crop(x, med))
hg_indexesmed2 <- lapply(hg_indexesmed2, FUN=function(x) crop(x, med))
hg_indexesmed3 <- lapply(hg_indexesmed3, FUN=function(x) crop(x, med))
hg_indexesmed4 <- lapply(hg_indexesmed4, FUN=function(x) crop(x, med))
hg_indexesmed5 <- lapply(hg_indexesmed5, FUN=function(x) crop(x, med))
hg_indexesmed6 <- lapply(hg_indexesmed6, FUN=function(x) crop(x, med))

#1. Norte Europa

#ne <- extent(-12, 40, 50, 73) 

hg_indexesne1 <- lapply(hg_indexes1, FUN=function(x) mask(x, border))
hg_indexesne2 <- lapply(hg_indexes2, FUN=function(x) mask(x, border))
hg_indexesne3 <- lapply(hg_indexes3, FUN=function(x) mask(x, border))
hg_indexesne4 <- lapply(hg_indexes4, FUN=function(x) mask(x, border))
hg_indexesne5 <- lapply(hg_indexes5, FUN=function(x) mask(x, border))
hg_indexesne6 <- lapply(hg_indexes6, FUN=function(x) mask(x, border))

hg_indexesne1 <- lapply(hg_indexesne1, FUN=function(x) crop(x, e))
hg_indexesne2 <- lapply(hg_indexesne2, FUN=function(x) crop(x, e))
hg_indexesne3 <- lapply(hg_indexesne3, FUN=function(x) crop(x, e))
hg_indexesne4 <- lapply(hg_indexesne4, FUN=function(x) crop(x, e))
hg_indexesne5 <- lapply(hg_indexesne5, FUN=function(x) crop(x, e))
hg_indexesne6 <- lapply(hg_indexesne6, FUN=function(x) crop(x, e))

##Media de los últimos 30 años MED:

hg_indexesLast301 <- lapply(hg_indexesmed1, FUN=function(x) x[[101:130]])
hg_indexesLast302 <- lapply(hg_indexesmed2, FUN=function(x) x[[101:130]])
hg_indexesLast303 <- lapply(hg_indexesmed3, FUN=function(x) x[[101:130]])
hg_indexesLast304 <- lapply(hg_indexesmed4, FUN=function(x) x[[101:130]])
hg_indexesLast305 <- lapply(hg_indexesmed5, FUN=function(x) x[[101:130]])
hg_indexesLast306 <- lapply(hg_indexesmed6, FUN=function(x) x[[101:130]])

hg_indexesRef <- lapply(hg_indexesmed, FUN=function(x) x[[1:30]])

hg_indexesLast30mean1 <- lapply(hg_indexesLast301, FUN=function(x) mean(x, na.rm=TRUE))
hg_indexesLast30mean2 <- lapply(hg_indexesLast302, FUN=function(x) mean(x, na.rm=TRUE))
hg_indexesLast30mean3 <- lapply(hg_indexesLast303, FUN=function(x) mean(x, na.rm=TRUE))
hg_indexesLast30mean4 <- lapply(hg_indexesLast304, FUN=function(x) mean(x, na.rm=TRUE))
hg_indexesLast30mean5 <- lapply(hg_indexesLast305, FUN=function(x) mean(x, na.rm=TRUE))
hg_indexesLast30mean6 <- lapply(hg_indexesLast306, FUN=function(x) mean(x, na.rm=TRUE))

hg_indexesRefmean <- lapply(hg_indexesRef, FUN=function(x) mean(x, na.rm=TRUE))

##Media de los últimos 30 años NE:

hg_indexesLast301 <- lapply(hg_indexesne1, FUN=function(x) x[[101:130]])
hg_indexesLast302 <- lapply(hg_indexesne2, FUN=function(x) x[[101:130]])
hg_indexesLast303 <- lapply(hg_indexesne3, FUN=function(x) x[[101:130]])
hg_indexesLast304 <- lapply(hg_indexesne4, FUN=function(x) x[[101:130]])
hg_indexesLast305 <- lapply(hg_indexesne5, FUN=function(x) x[[101:130]])
hg_indexesLast306 <- lapply(hg_indexesne6, FUN=function(x) x[[101:130]])

hg_indexesRef <- lapply(hg_indexesne, FUN=function(x) x[[1:30]])

hg_indexesLast30mean1 <- lapply(hg_indexesLast301, FUN=function(x) mean(x, na.rm=TRUE))
hg_indexesLast30mean2 <- lapply(hg_indexesLast302, FUN=function(x) mean(x, na.rm=TRUE))
hg_indexesLast30mean3 <- lapply(hg_indexesLast303, FUN=function(x) mean(x, na.rm=TRUE))
hg_indexesLast30mean4 <- lapply(hg_indexesLast304, FUN=function(x) mean(x, na.rm=TRUE))
hg_indexesLast30mean5 <- lapply(hg_indexesLast305, FUN=function(x) mean(x, na.rm=TRUE))
hg_indexesLast30mean6 <- lapply(hg_indexesLast306, FUN=function(x) mean(x, na.rm=TRUE))



HWMI1 <- stack(unlist(hg_indexesLast30mean1))
HWMI2 <- stack(unlist(hg_indexesLast30mean2))
HWMI3 <- stack(unlist(hg_indexesLast30mean3))
HWMI4 <- stack(unlist(hg_indexesLast30mean4))
HWMI5 <- stack(unlist(hg_indexesLast30mean5))
HWMI6 <- stack(unlist(hg_indexesLast30mean6))

HWMIsinmar1 <- mask(HWMI1, border1)
HWMIsinmar2 <- mask(HWMI2, border2)
HWMIsinmar3 <- mask(HWMI3, border3)
HWMIsinmar4 <- mask(HWMI4, border4)
HWMIsinmar5 <- mask(HWMI5, border5)
HWMIsinmar6 <- mask(HWMI6, border6)

names(HWMIsinmar1) <- nn1
names(HWMIsinmar2) <- nn2
names(HWMIsinmar3) <- nn3
names(HWMIsinmar4) <- nn4
names(HWMIsinmar5) <- nn5
names(HWMIsinmar6) <- nn6

##Figura 1

h_imean1 <- lapply(hg_indexesLast30mean1, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
h_imean2 <- lapply(hg_indexesLast30mean2, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
h_imean3 <- lapply(hg_indexesLast30mean3, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
h_imean4 <- lapply(hg_indexesLast30mean4, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
h_imean5 <- lapply(hg_indexesLast30mean5, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
h_imean6 <- lapply(hg_indexesLast30mean6, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

h_df1 <- lapply(h_imean1, as.data.frame) #13.1, 54.4
h_df2 <- lapply(h_imean2, as.data.frame) #8.31, 18.3
h_df3 <- lapply(h_imean3, as.data.frame) #40.5, 146
h_df4 <- lapply(h_imean4, as.data.frame) #7
h_df5 <- lapply(h_imean5, as.data.frame) #21.6, 108
h_df6 <- lapply(h_imean6, as.data.frame) #10.7, 44.3

h_mg <- merge.data.frame(h_df1, h_df2, by="row.names")
h_mg <- merge.data.frame(h_mg, h_df3, by="row.names")
h_mg <- merge.data.frame(h_mg, h_df4, by="row.names")
h_mg <- merge.data.frame(h_mg, h_df5, by="row.names")
h_mg <- merge.data.frame(h_mg, h_df6, by="row.names")
h_mg <- as.matrix(h_mg[-1])
h_mg <- as.matrix(h_mg[-1])
h_mg <- as.matrix(h_mg[-1])
h_mg <- as.matrix(h_mg[-1])
h_mg <- as.matrix(h_mg[-1])

modg <- c(1,1,1,1,1,1,1,1,1,1,1)

h_M <- matrix(c(h_mg, modg), 11, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))

hg_M1_45 <- matrix(c(h_M[[1]], h_M[[3]], h_M[[5]], h_M[[7]], h_M[[8]],h_M[[10]], modg), 6, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))
hg_M1_85 <- matrix(c(h_M[[2]], h_M[4], h_M[[6]], h_M[[9]], h_M[[11]], modg), 5, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))

hgM <- as.data.frame(h_M)
hgM45 <- as.data.frame(hg_M1_45)
hgM85 <- as.data.frame(hg_M1_85)

h_df$color <- palette[as.factor(hgM45$resolution)]


##Ordeno para representar

w1 <- stack(HWMIsinmar1[[1:2]])
w2 <- stack(HWMIsinmar2[[1:2]])
w3 <- stack(HWMIsinmar3[[1:2]])
w4 <- stack(HWMIsinmar4[[1]])
w5 <- stack(HWMIsinmar5[[1:2]])
w6 <- stack(HWMIsinmar6[[1:2]])

my.at <- seq(0,20,1)
my.at <- c(my.at, 30, 40, 50, 75, 100, 125)
w1[w1[]>100] <- 100
length(w1[w1[] > 150]) <- HWMIsinmar1
HWMIsinmar1[HWMIsinmar1[]>100] <- 100
HWMIsinmar1[HWMIsinmar1[]>100] <- 100
myColorkey <- list(at=my.at, ## where the colors change
                   space="bottom",
                   width= .8)


div.pal <- brewer.pal(n=9, 'Set3')

div.pal <- brewer.pal(n=9, 'RdYlGn')

pdf("hwmidCCC.pdf")
ccc <- levelplot(w1, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c("CanESM"), cex=1), names.attr=c("", ""), layout=c(2,1), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at)))  +     layer(sp.lines(border1, lwd=0.5))
dev.off()
pdf("hwmidCNRM.pdf")
cnrm <- levelplot(w2, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c("CNRM"), cex=1), names.attr=c("", ""), layout=c(2,1), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at)))  +     layer(sp.lines(border2, lwd=0.5))
dev.off()
pdf("hwmidHadGEM.pdf", width=7, height=4)
mohc<- levelplot(w3, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c("HadGEM"), cex=1), names.attr=c("", ""), layout=c(2,1), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at)))  +     layer(sp.lines(border3, lwd=0.5))
dev.off()
pdf("hwmidEC-EARTH.pdf", width=7, height=4)
ecearth<- levelplot(w4, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c("EC-EARTH"), cex=1), names.attr=c(""), layout=c(2,1), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at)))  +     layer(sp.lines(border4, lwd=0.5))
dev.off()
pdf("hwmidIPSL.pdf", width=7, height=4)
ipsl<- levelplot(w5, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c("IPSL"), cex=1), names.attr=c("", ""), layout=c(2,1), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at)))  +     layer(sp.lines(border5, lwd=0.5))
dev.off()
pdf("hwmidMPI.pdf", width=7, height=4)
mpi<- levelplot(w6, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c("MPI"), cex=1), names.attr=c("", ""), layout=c(2,1), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at)))  +     layer(sp.lines(border6, lwd=0.5))
dev.off()

pdf("hwmidGCMprint.pdf", width=15, height=7)
print(ccc, split=c(1, 1, 3, 2), more=TRUE)
print(cnrm, split=c(2, 1, 3, 2), more=TRUE)
print(mohc, split=c(3, 1, 3, 2), more=TRUE)
print(ecearth, split=c(1, 2, 3, 2), more=TRUE)
print(ipsl, split=c(2, 2, 3, 2),more=TRUE)
print(mpi, split=c(3, 2, 3, 2))
dev.off()

pdf("hwmidGCMgrid.pdf")
grid.arrange(ccc, cnrm, mohc, ecearth, ipsl, mpi,  main = "HWMId")
dev.off()


## Barplot

bmed <- brick(hg_indexesLast30mean1)
bmed <- cellStats(bmed, mean)
pdf("hwmidbarplotCCC.pdf", width=7, height=4)
b1 <- barplot(bmed, col=c('grey','orange'), las=2, ylab="HWMId", main="CanESM", legend.text= c("RCP45", "RCP85"), names.arg= nn1)
dev.off()

#bne <- brick(hg_indexesLast30mean1)
#bne <- cellStats(bne, mean)
#pdf("hwmidnebarplotCCC.pdf", width=7, height=4)
#bne <- barplot(bne, col=c('grey','orange'), las=2, ylab="HWMId", main="NE CanESM", legend.text= c("RCP45", "RCP85"), names.arg= nn1)
#dev.off()

bmed <- brick(hg_indexesLast30mean2)
bmed <- cellStats(bmed, mean)
pdf("hwmidbarplotCNRM.pdf", width=7, height=4)
b2 <- barplot(bmed, col=c('grey','orange'), las=2, ylab="HWMId", main="CNRM", legend.text= c("RCP45", "RCP85"), names.arg= nn2)
dev.off()

#bne <- brick(hg_indexesLast30mean2)
#bne <- cellStats(bne, mean)
#pdf("hwmidnebarplotCNRM.pdf", width=7, height=4)
#bne <- barplot(bne, col=c('grey','orange'), las=2, ylab="HWMId", main="NE CNRM", legend.text= c("RCP45", "RCP85"), names.arg= nn2)
#dev.off()

bmed <- brick(hg_indexesLast30mean3)
bmed <- cellStats(bmed, mean)
pdf("hwmidmedbarplotHadGEM.pdf", width=7, height=4)
b3 <- barplot(bmed, col=c('grey','orange'), las=2, ylab="HWMId", main="HadGEM", legend.text= c("RCP45", "RCP85"), names.arg= nn3)
dev.off()

#bne <- brick(hg_indexesLast30mean3)
#bne <- cellStats(bne, mean)
#pdf("hwmidnebarplotHadGEM.pdf", width=7, height=4)
#bne <- barplot(bne, col=c('grey','orange'), las=2, ylab="HWMId", main="NE HadGEM", legend.text= c("RCP45", "RCP85"), names.arg= nn3)
#dev.off()

bmed <- brick(hg_indexesLast30mean4)
bmed <- cellStats(bmed, mean)
pdf("hwmidmedbarplotEC-EARTH.pdf", width=7, height=4)
b4 <- barplot(bmed, col=c('grey','orange'), las=2, ylab="HWMId", main="EC-EARTH", legend.text= c("RCP45", "RCP85"), names.arg= nn4)
dev.off()

#bne <- brick(hg_indexesLast30mean4)
#bne <- cellStats(bne, mean)
#pdf("hwmidnebarplotEC-EARTH.pdf", width=7, height=4)
#bne <- barplot(bne, col=c('grey','orange'), las=2, ylab="HWMId", main="NE EC-EARTH", legend.text= c("RCP45", "RCP85"), names.arg= nn4)
#dev.off()

bmed <- brick(hg_indexesLast30mean5)
bmed <- cellStats(bmed, mean)
pdf("hwmidmedbarplotIPSL.pdf", width=7, height=4)
b5 <- barplot(bmed, col=c('grey','orange'), las=2, ylab="HWMId", main="IPSL", legend.text= c("RCP45", "RCP85"), names.arg= nn5)
dev.off()

#bne <- brick(hg_indexesLast30mean5)
#bne <- cellStats(bne, mean)
#pdf("hwmidnebarplotIPSL.pdf", width=7, height=4)
#bne <- barplot(bne, col=c('grey','orange'), las=2, ylab="HWMId", main="NE IPSL", legend.text= c("RCP45", "RCP85"), names.arg= nn5)
#dev.off()

bmed <- brick(hg_indexesLast30mean6)
bmed <- cellStats(bmed, mean)
pdf("hwmidbarplotMPI.pdf", width=7, height=4)
b6 <- barplot(bmed, col=c('grey','orange'), las=2, ylab="HWMId", main="MED MPI", legend.text= c("RCP45", "RCP85"), names.arg= nn6)
dev.off()

#bne <- brick(hg_indexesLast30mean6)
#bne <- cellStats(bne, mean)
#pdf("hwmidnebarplotMPI.pdf", width=7, height=4)
#bne <- barplot(bne, col=c('grey','orange'), las=2, ylab="HWMId", main="NE MPI", legend.text= c("RCP45", "RCP85"), names.arg= nn6)
#dev.off()

pdf("hwmidbarplotGCMprint.pdf")
print(b1, split=c(1, 1, 3, 2), more=TRUE)
print(b2, split=c(2, 1, 3, 2), more=TRUE)
print(b3, split=c(3, 1, 3, 2), more=TRUE)
print(b4, split=c(1, 2, 3, 2), more=TRUE)
print(b5, split=c(2, 2, 3, 2),more=TRUE)
print(b6, split=c(3, 2, 3, 2))
dev.off()

##Series temporales HWMId

c1 <- list("hwmidCanESM85box_runmean.nc")
c2 <- list("hwmidCNRM85box_runmean.nc")
c4 <- list("hwmidHadGEM85box_runmean.nc")
c5 <- list("hwmidIPSL85box_runmean.nc")
c6 <- list("hwmidMPI85box_runmean.nc")

nn1 <- c("CanESM85")
nn2 <- c("CNRM85")
nn4 <- c("HadGEM85")
nn5 <- c("IPSL85")
nn6 <- c("MPI85")

h_indexes1 <- lapply(c1, stack)
h_indexes2 <- lapply(c2, stack)
h_indexes4 <- lapply(c4, stack)
h_indexes5 <- lapply(c5, stack)
h_indexes6 <- lapply(c6, stack)

border1 <- fooborder(h_indexes1[[1]])
border2 <- fooborder(h_indexes2[[1]])
border4 <- fooborder(h_indexes4[[1]])
border5 <- fooborder(h_indexes5[[1]])
border6 <- fooborder(h_indexes6[[1]])

idxm <- seq(as.Date("1973-01-01"), as.Date("2098-12-01"), "year")

h_indexes1 <- lapply(h_indexes1, FUN=function(x) setZ(x, idxm))
h_indexes2 <- lapply(h_indexes2, FUN=function(x) setZ(x, idxm))
h_indexes4 <- lapply(h_indexes4, FUN=function(x) setZ(x, idxm))
h_indexes5 <- lapply(h_indexes5, FUN=function(x) setZ(x, idxm))
h_indexes6 <- lapply(h_indexes6, FUN=function(x) setZ(x, idxm))

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
e1 <- e1 + e2
ext1 <- sp1 + sp2

piseries1 <- lapply(h_indexes1, FUN=function(x) mask(x, border1))
piseries2 <- lapply(h_indexes2, FUN=function(x) mask(x, border2))
piseries4 <- lapply(h_indexes4, FUN=function(x) mask(x, border4))
piseries5 <- lapply(h_indexes5, FUN=function(x) mask(x, border5))
piseries6 <- lapply(h_indexes6, FUN=function(x) mask(x, border6))

piseries1 <- lapply(piseries1, FUN=function(x) crop(x, ext1))
piseries2 <- lapply(piseries2, FUN=function(x) crop(x, ext1))
piseries4 <- lapply(piseries4, FUN=function(x) crop(x, ext1))
piseries5 <- lapply(piseries5, FUN=function(x) crop(x, ext1))
piseries6 <- lapply(piseries6, FUN=function(x) crop(x, ext1))

piseries1 <- lapply(piseries1, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
piseries2 <- lapply(piseries2, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
piseries3 <- lapply(piseries3, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
piseries4 <- lapply(piseries4, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
piseries5 <- lapply(piseries5, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
piseries6 <- lapply(piseries6, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

pimatrix1 <- do.call(cbind, piseries1)
pimatrix2 <- do.call(cbind, piseries2)
pimatrix4 <- do.call(cbind, piseries4)
pimatrix5 <- do.call(cbind, piseries5)
pimatrix6 <- do.call(cbind, piseries6)

z <- as.zoo(pimatrix1)
z <- as.zoo(pimatrix2)
z <- as.zoo(pimatrix4)
z <- as.zoo(pimatrix5)
z <- as.zoo(pimatrix6)

pimatrix1 <- as.data.frame(pimatrix1)
pimatrix2 <- as.data.frame(pimatrix2)
pimatrix4 <- as.data.frame(pimatrix4)
pimatrix5 <- as.data.frame(pimatrix5)
pimatrix6 <- as.data.frame(pimatrix6)

rows<- c(1, 2, 3, 4,5, 6,7, 8, 9,10, 11,12, 13,14, 15,  16, 17,18,19,20, 21,22,23,24,25,26, 27, 28, 29,30, 31,32,33, 34,35,36,37,38, 39,  40,41,42,43, 44,45, 46, 47, 48,49, 50,51, 52,  53, 54,55, 56, 57,58,59, 60, 61,62, 63, 64, 65, 66,  67, 68,69,70,71, 72,73,74, 75, 76,77,78,79,  80, 81, 82, 83, 84,  85, 86, 87, 88,  89,  90, 91, 92, 93,  94,  95,  96, 97, 98, 99, 100,101,102,103,104,105,106,107, 108, 109,110,111,112,113,114, 115,116, 117,118,119,120,121,122,123,124,125,126)
colnames(pimatrix1) <- nn1
colnames(pimatrix2) <- nn2
colnames(pimatrix4) <- nn4
colnames(pimatrix5) <- nn5
colnames(pimatrix6) <- nn6

rownames(pimatrix1) <- rows
rownames(pimatrix2) <- rows
rownames(pimatrix4) <- rows
rownames(pimatrix5) <- rows
rownames(pimatrix6) <- rows

pimatrixt <- merge(pimatrix1, pimatrix2, by="row.names", sort=FALSE)
pimatrixt <- merge(pimatrixt, pimatrix4, by="row.names", sort=FALSE)
pimatrixt <- merge(pimatrixt, pimatrix5, by="row.names", sort=FALSE)
pimatrixt <- merge(pimatrixt, pimatrix6, by="row.names", sort=FALSE)

pimatrixt <- as.data.frame(pimatrixt[-1])
pimatrixt <- as.data.frame(pimatrixt[-1])
pimatrixt <- as.data.frame(pimatrixt[-1])
pimatrixt <- as.data.frame(pimatrixt[-1])
library(zoo)
pimatrix <- as.zoo(pimatrixt)



myTheme <- custom.theme.2(pch = 20, cex = 0.7, alpha=0.7)
myTheme$strip.background$col <- 'transparent'
myTheme$strip.shingle$col <- 'transparent'

pdf("piseriesHWMIdGCM.pdf")
pi <- xyplot(pimatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,250),main="PI",
             auto.key = FALSE)
dev.off()

## 1. fr

frseries1 <- lapply(h_indexes1, FUN=function(x) mask(x, border1))
frseries2 <- lapply(h_indexes2, FUN=function(x) mask(x, border2))
frseries4 <- lapply(h_indexes4, FUN=function(x) mask(x, border4))
frseries5 <- lapply(h_indexes5, FUN=function(x) mask(x, border5))
frseries6 <- lapply(h_indexes6, FUN=function(x) mask(x, border6))

frseries1 <- lapply(frseries1, FUN=function(x) crop(x, e3))
frseries2 <- lapply(frseries2, FUN=function(x) crop(x, e3))
frseries4 <- lapply(frseries4, FUN=function(x) crop(x, e3))
frseries5 <- lapply(frseries5, FUN=function(x) crop(x, e3))
frseries6 <- lapply(frseries6, FUN=function(x) crop(x, e3))

frseries1 <- lapply(frseries1, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
frseries2 <- lapply(frseries2, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
frseries3 <- lapply(frseries3, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
frseries4 <- lapply(frseries4, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
frseries5 <- lapply(frseries5, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
frseries6 <- lapply(frseries6, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

frmatrix1 <- do.call(cbind, frseries1)
frmatrix2 <- do.call(cbind, frseries2)
frmatrix4 <- do.call(cbind, frseries4)
frmatrix5 <- do.call(cbind, frseries5)
frmatrix6 <- do.call(cbind, frseries6)

z <- as.zoo(frmatrix1)
z <- as.zoo(frmatrix2)
z <- as.zoo(frmatrix4)
z <- as.zoo(frmatrix5)
z <- as.zoo(frmatrix6)

frmatrix1 <- as.data.frame(frmatrix1)
frmatrix2 <- as.data.frame(frmatrix2)
frmatrix4 <- as.data.frame(frmatrix4)
frmatrix5 <- as.data.frame(frmatrix5)
frmatrix6 <- as.data.frame(frmatrix6)

rows<- c(1, 2, 3, 4,5, 6,7, 8, 9,10, 11,12, 13,14, 15,  16, 17,18,19,20, 21,22,23,24,25,26, 27, 28, 29,30, 31,32,33, 34,35,36,37,38, 39,  40,41,42,43, 44,45, 46, 47, 48,49, 50,51, 52,  53, 54,55, 56, 57,58,59, 60, 61,62, 63, 64, 65, 66,  67, 68,69,70,71, 72,73,74, 75, 76,77,78,79,  80, 81, 82, 83, 84,  85, 86, 87, 88,  89,  90, 91, 92, 93,  94,  95,  96, 97, 98, 99, 100,101,102,103,104,105,106,107, 108, 109,110,111,112,113,114, 115,116, 117,118,119,120,121,122,123,124,125,126)
colnames(frmatrix1) <- nn1
colnames(frmatrix2) <- nn2
colnames(frmatrix4) <- nn4
colnames(frmatrix5) <- nn5
colnames(frmatrix6) <- nn6

rownames(frmatrix1) <- rows
rownames(frmatrix2) <- rows
rownames(frmatrix4) <- rows
rownames(frmatrix5) <- rows
rownames(frmatrix6) <- rows

frmatrixt <- merge(frmatrix1, frmatrix2, by="row.names", sort=FALSE)
frmatrixt <- merge(frmatrixt, frmatrix4, by="row.names", sort=FALSE)
frmatrixt <- merge(frmatrixt, frmatrix5, by="row.names", sort=FALSE)
frmatrixt <- merge(frmatrixt, frmatrix6, by="row.names", sort=FALSE)

frmatrixt <- as.data.frame(frmatrixt[-1])
frmatrixt <- as.data.frame(frmatrixt[-1])
frmatrixt <- as.data.frame(frmatrixt[-1])
frmatrixt <- as.data.frame(frmatrixt[-1])

frmatrix <- as.zoo(frmatrixt)



myTheme <- custom.theme.2(pch = 20, cex = 0.7, alpha=0.7)
myTheme$strip.background$col <- 'transparent'
myTheme$strip.shingle$col <- 'transparent'

pdf("frseriesHWMIdGCM.pdf")
fr <- xyplot(frmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,250),main="FR",
             auto.key =list(corner=c(1, 1), x = 0.6, y = 0.95, cex=0.4))
dev.off()

## 1. alps

alpsseries1 <- lapply(h_indexes1, FUN=function(x) mask(x, border1))
alpsseries2 <- lapply(h_indexes2, FUN=function(x) mask(x, border2))
alpsseries4 <- lapply(h_indexes4, FUN=function(x) mask(x, border4))
alpsseries5 <- lapply(h_indexes5, FUN=function(x) mask(x, border5))
alpsseries6 <- lapply(h_indexes6, FUN=function(x) mask(x, border6))

alpsseries1 <- lapply(alpsseries1, FUN=function(x) crop(x, e4))
alpsseries2 <- lapply(alpsseries2, FUN=function(x) crop(x, e4))
alpsseries4 <- lapply(alpsseries4, FUN=function(x) crop(x, e4))
alpsseries5 <- lapply(alpsseries5, FUN=function(x) crop(x, e4))
alpsseries6 <- lapply(alpsseries6, FUN=function(x) crop(x, e4))

alpsseries1 <- lapply(alpsseries1, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
alpsseries2 <- lapply(alpsseries2, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
alpsseries3 <- lapply(alpsseries3, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
alpsseries4 <- lapply(alpsseries4, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
alpsseries5 <- lapply(alpsseries5, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
alpsseries6 <- lapply(alpsseries6, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

alpsmatrix1 <- do.call(cbind, alpsseries1)
alpsmatrix2 <- do.call(cbind, alpsseries2)
alpsmatrix4 <- do.call(cbind, alpsseries4)
alpsmatrix5 <- do.call(cbind, alpsseries5)
alpsmatrix6 <- do.call(cbind, alpsseries6)

z <- as.zoo(alpsmatrix1)
z <- as.zoo(alpsmatrix2)
z <- as.zoo(alpsmatrix4)
z <- as.zoo(alpsmatrix5)
z <- as.zoo(alpsmatrix6)

alpsmatrix1 <- as.data.frame(alpsmatrix1)
alpsmatrix2 <- as.data.frame(alpsmatrix2)
alpsmatrix4 <- as.data.frame(alpsmatrix4)
alpsmatrix5 <- as.data.frame(alpsmatrix5)
alpsmatrix6 <- as.data.frame(alpsmatrix6)

rows<- c(1, 2, 3, 4,5, 6,7, 8, 9,10, 11,12, 13,14, 15,  16, 17,18,19,20, 21,22,23,24,25,26, 27, 28, 29,30, 31,32,33, 34,35,36,37,38, 39,  40,41,42,43, 44,45, 46, 47, 48,49, 50,51, 52,  53, 54,55, 56, 57,58,59, 60, 61,62, 63, 64, 65, 66,  67, 68,69,70,71, 72,73,74, 75, 76,77,78,79,  80, 81, 82, 83, 84,  85, 86, 87, 88,  89,  90, 91, 92, 93,  94,  95,  96, 97, 98, 99, 100,101,102,103,104,105,106,107, 108, 109,110,111,112,113,114, 115,116, 117,118,119,120,121,122,123,124,125,126)
colnames(alpsmatrix1) <- nn1
colnames(alpsmatrix2) <- nn2
colnames(alpsmatrix4) <- nn4
colnames(alpsmatrix5) <- nn5
colnames(alpsmatrix6) <- nn6

rownames(alpsmatrix1) <- rows
rownames(alpsmatrix2) <- rows
rownames(alpsmatrix4) <- rows
rownames(alpsmatrix5) <- rows
rownames(alpsmatrix6) <- rows

alpsmatrixt <- merge(alpsmatrix1, alpsmatrix2, by="row.names", sort=FALSE)
alpsmatrixt <- merge(alpsmatrixt, alpsmatrix4, by="row.names", sort=FALSE)
alpsmatrixt <- merge(alpsmatrixt, alpsmatrix5, by="row.names", sort=FALSE)
alpsmatrixt <- merge(alpsmatrixt, alpsmatrix6, by="row.names", sort=FALSE)

alpsmatrixt <- as.data.frame(alpsmatrixt[-1])
alpsmatrixt <- as.data.frame(alpsmatrixt[-1])
alpsmatrixt <- as.data.frame(alpsmatrixt[-1])
alpsmatrixt <- as.data.frame(alpsmatrixt[-1])

alpsmatrix <- as.zoo(alpsmatrixt)


pdf("alpsseriesHWMIdGCM.pdf")
alps <- xyplot(alpsmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,250),main="ALPS",
             auto.key = FALSE)
dev.off()

## 1. wafr

wafrseries1 <- lapply(h_indexes1, FUN=function(x) mask(x, border1))
wafrseries2 <- lapply(h_indexes2, FUN=function(x) mask(x, border2))
wafrseries4 <- lapply(h_indexes4, FUN=function(x) mask(x, border4))
wafrseries5 <- lapply(h_indexes5, FUN=function(x) mask(x, border5))
wafrseries6 <- lapply(h_indexes6, FUN=function(x) mask(x, border6))

wafrseries1 <- lapply(wafrseries1, FUN=function(x) crop(x, e5))
wafrseries2 <- lapply(wafrseries2, FUN=function(x) crop(x, e5))
wafrseries4 <- lapply(wafrseries4, FUN=function(x) crop(x, e5))
wafrseries5 <- lapply(wafrseries5, FUN=function(x) crop(x, e5))
wafrseries6 <- lapply(wafrseries6, FUN=function(x) crop(x, e5))

wafrseries1 <- lapply(wafrseries1, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
wafrseries2 <- lapply(wafrseries2, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
wafrseries3 <- lapply(wafrseries3, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
wafrseries4 <- lapply(wafrseries4, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
wafrseries5 <- lapply(wafrseries5, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
wafrseries6 <- lapply(wafrseries6, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

wafrmatrix1 <- do.call(cbind, wafrseries1)
wafrmatrix2 <- do.call(cbind, wafrseries2)
wafrmatrix4 <- do.call(cbind, wafrseries4)
wafrmatrix5 <- do.call(cbind, wafrseries5)
wafrmatrix6 <- do.call(cbind, wafrseries6)

z <- as.zoo(wafrmatrix1)
z <- as.zoo(wafrmatrix2)
z <- as.zoo(wafrmatrix4)
z <- as.zoo(wafrmatrix5)
z <- as.zoo(wafrmatrix6)

wafrmatrix1 <- as.data.frame(wafrmatrix1)
wafrmatrix2 <- as.data.frame(wafrmatrix2)
wafrmatrix4 <- as.data.frame(wafrmatrix4)
wafrmatrix5 <- as.data.frame(wafrmatrix5)
wafrmatrix6 <- as.data.frame(wafrmatrix6)

rows<- c(1, 2, 3, 4,5, 6,7, 8, 9,10, 11,12, 13,14, 15,  16, 17,18,19,20, 21,22,23,24,25,26, 27, 28, 29,30, 31,32,33, 34,35,36,37,38, 39,  40,41,42,43, 44,45, 46, 47, 48,49, 50,51, 52,  53, 54,55, 56, 57,58,59, 60, 61,62, 63, 64, 65, 66,  67, 68,69,70,71, 72,73,74, 75, 76,77,78,79,  80, 81, 82, 83, 84,  85, 86, 87, 88,  89,  90, 91, 92, 93,  94,  95,  96, 97, 98, 99, 100,101,102,103,104,105,106,107, 108, 109,110,111,112,113,114, 115,116, 117,118,119,120,121,122,123,124,125,126)
colnames(wafrmatrix1) <- nn1
colnames(wafrmatrix2) <- nn2
colnames(wafrmatrix4) <- nn4
colnames(wafrmatrix5) <- nn5
colnames(wafrmatrix6) <- nn6

rownames(wafrmatrix1) <- rows
rownames(wafrmatrix2) <- rows
rownames(wafrmatrix4) <- rows
rownames(wafrmatrix5) <- rows
rownames(wafrmatrix6) <- rows

wafrmatrixt <- merge(wafrmatrix1, wafrmatrix2, by="row.names", sort=FALSE)
wafrmatrixt <- merge(wafrmatrixt, wafrmatrix4, by="row.names", sort=FALSE)
wafrmatrixt <- merge(wafrmatrixt, wafrmatrix5, by="row.names", sort=FALSE)
wafrmatrixt <- merge(wafrmatrixt, wafrmatrix6, by="row.names", sort=FALSE)

wafrmatrixt <- as.data.frame(wafrmatrixt[-1])
wafrmatrixt <- as.data.frame(wafrmatrixt[-1])
wafrmatrixt <- as.data.frame(wafrmatrixt[-1])
wafrmatrixt <- as.data.frame(wafrmatrixt[-1])

wafrmatrix <- as.zoo(wafrmatrixt)


pdf("wafrseriesHWMIdGCM.pdf")
wafr <- xyplot(wafrmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,450),main="WAFR",
             auto.key = FALSE)
dev.off()

## 1. ewafr

ewafrseries1 <- lapply(h_indexes1, FUN=function(x) mask(x, border1))
ewafrseries2 <- lapply(h_indexes2, FUN=function(x) mask(x, border2))
ewafrseries4 <- lapply(h_indexes4, FUN=function(x) mask(x, border4))
ewafrseries5 <- lapply(h_indexes5, FUN=function(x) mask(x, border5))
ewafrseries6 <- lapply(h_indexes6, FUN=function(x) mask(x, border6))

ewafrseries1 <- lapply(ewafrseries1, FUN=function(x) crop(x, e6))
ewafrseries2 <- lapply(ewafrseries2, FUN=function(x) crop(x, e6))
ewafrseries4 <- lapply(ewafrseries4, FUN=function(x) crop(x, e6))
ewafrseries5 <- lapply(ewafrseries5, FUN=function(x) crop(x, e6))
ewafrseries6 <- lapply(ewafrseries6, FUN=function(x) crop(x, e6))

ewafrseries1 <- lapply(ewafrseries1, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
ewafrseries2 <- lapply(ewafrseries2, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
ewafrseries3 <- lapply(ewafrseries3, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
ewafrseries4 <- lapply(ewafrseries4, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
ewafrseries5 <- lapply(ewafrseries5, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
ewafrseries6 <- lapply(ewafrseries6, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

ewafrmatrix1 <- do.call(cbind, ewafrseries1)
ewafrmatrix2 <- do.call(cbind, ewafrseries2)
ewafrmatrix4 <- do.call(cbind, ewafrseries4)
ewafrmatrix5 <- do.call(cbind, ewafrseries5)
ewafrmatrix6 <- do.call(cbind, ewafrseries6)

z <- as.zoo(ewafrmatrix1)
z <- as.zoo(ewafrmatrix2)
z <- as.zoo(ewafrmatrix4)
z <- as.zoo(ewafrmatrix5)
z <- as.zoo(ewafrmatrix6)

ewafrmatrix1 <- as.data.frame(ewafrmatrix1)
ewafrmatrix2 <- as.data.frame(ewafrmatrix2)
ewafrmatrix4 <- as.data.frame(ewafrmatrix4)
ewafrmatrix5 <- as.data.frame(ewafrmatrix5)
ewafrmatrix6 <- as.data.frame(ewafrmatrix6)

rows<- c(1, 2, 3, 4,5, 6,7, 8, 9,10, 11,12, 13,14, 15,  16, 17,18,19,20, 21,22,23,24,25,26, 27, 28, 29,30, 31,32,33, 34,35,36,37,38, 39,  40,41,42,43, 44,45, 46, 47, 48,49, 50,51, 52,  53, 54,55, 56, 57,58,59, 60, 61,62, 63, 64, 65, 66,  67, 68,69,70,71, 72,73,74, 75, 76,77,78,79,  80, 81, 82, 83, 84,  85, 86, 87, 88,  89,  90, 91, 92, 93,  94,  95,  96, 97, 98, 99, 100,101,102,103,104,105,106,107, 108, 109,110,111,112,113,114, 115,116, 117,118,119,120,121,122,123,124,125,126)
colnames(ewafrmatrix1) <- nn1
colnames(ewafrmatrix2) <- nn2
colnames(ewafrmatrix4) <- nn4
colnames(ewafrmatrix5) <- nn5
colnames(ewafrmatrix6) <- nn6

rownames(ewafrmatrix1) <- rows
rownames(ewafrmatrix2) <- rows
rownames(ewafrmatrix4) <- rows
rownames(ewafrmatrix5) <- rows
rownames(ewafrmatrix6) <- rows

ewafrmatrixt <- merge(ewafrmatrix1, ewafrmatrix2, by="row.names", sort=FALSE)
ewafrmatrixt <- merge(ewafrmatrixt, ewafrmatrix4, by="row.names", sort=FALSE)
ewafrmatrixt <- merge(ewafrmatrixt, ewafrmatrix5, by="row.names", sort=FALSE)
ewafrmatrixt <- merge(ewafrmatrixt, ewafrmatrix6, by="row.names", sort=FALSE)

ewafrmatrixt <- as.data.frame(ewafrmatrixt[-1])
ewafrmatrixt <- as.data.frame(ewafrmatrixt[-1])
ewafrmatrixt <- as.data.frame(ewafrmatrixt[-1])
ewafrmatrixt <- as.data.frame(ewafrmatrixt[-1])

ewafrmatrix <- as.zoo(ewafrmatrixt)


pdf("ewafrseriesHWMIdGCM.pdf")
ewafr <- xyplot(ewafrmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,450),main="EAFR",
             auto.key = FALSE)
dev.off()

## 1. neast

neastseries1 <- lapply(h_indexes1, FUN=function(x) mask(x, border1))
neastseries2 <- lapply(h_indexes2, FUN=function(x) mask(x, border2))
neastseries4 <- lapply(h_indexes4, FUN=function(x) mask(x, border4))
neastseries5 <- lapply(h_indexes5, FUN=function(x) mask(x, border5))
neastseries6 <- lapply(h_indexes6, FUN=function(x) mask(x, border6))

neastseries1 <- lapply(neastseries1, FUN=function(x) crop(x, e7))
neastseries2 <- lapply(neastseries2, FUN=function(x) crop(x, e7))
neastseries4 <- lapply(neastseries4, FUN=function(x) crop(x, e7))
neastseries5 <- lapply(neastseries5, FUN=function(x) crop(x, e7))
neastseries6 <- lapply(neastseries6, FUN=function(x) crop(x, e7))

neastseries1 <- lapply(neastseries1, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
neastseries2 <- lapply(neastseries2, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
neastseries3 <- lapply(neastseries3, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
neastseries4 <- lapply(neastseries4, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
neastseries5 <- lapply(neastseries5, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
neastseries6 <- lapply(neastseries6, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

neastmatrix1 <- do.call(cbind, neastseries1)
neastmatrix2 <- do.call(cbind, neastseries2)
neastmatrix4 <- do.call(cbind, neastseries4)
neastmatrix5 <- do.call(cbind, neastseries5)
neastmatrix6 <- do.call(cbind, neastseries6)

z <- as.zoo(neastmatrix1)
z <- as.zoo(neastmatrix2)
z <- as.zoo(neastmatrix4)
z <- as.zoo(neastmatrix5)
z <- as.zoo(neastmatrix6)

neastmatrix1 <- as.data.frame(neastmatrix1)
neastmatrix2 <- as.data.frame(neastmatrix2)
neastmatrix4 <- as.data.frame(neastmatrix4)
neastmatrix5 <- as.data.frame(neastmatrix5)
neastmatrix6 <- as.data.frame(neastmatrix6)

rows<- c(1, 2, 3, 4,5, 6,7, 8, 9,10, 11,12, 13,14, 15,  16, 17,18,19,20, 21,22,23,24,25,26, 27, 28, 29,30, 31,32,33, 34,35,36,37,38, 39,  40,41,42,43, 44,45, 46, 47, 48,49, 50,51, 52,  53, 54,55, 56, 57,58,59, 60, 61,62, 63, 64, 65, 66,  67, 68,69,70,71, 72,73,74, 75, 76,77,78,79,  80, 81, 82, 83, 84,  85, 86, 87, 88,  89,  90, 91, 92, 93,  94,  95,  96, 97, 98, 99, 100,101,102,103,104,105,106,107, 108, 109,110,111,112,113,114, 115,116, 117,118,119,120,121,122,123,124,125,126)
colnames(neastmatrix1) <- nn1
colnames(neastmatrix2) <- nn2
colnames(neastmatrix4) <- nn4
colnames(neastmatrix5) <- nn5
colnames(neastmatrix6) <- nn6

rownames(neastmatrix1) <- rows
rownames(neastmatrix2) <- rows
rownames(neastmatrix4) <- rows
rownames(neastmatrix5) <- rows
rownames(neastmatrix6) <- rows

neastmatrixt <- merge(neastmatrix1, neastmatrix2, by="row.names", sort=FALSE)
neastmatrixt <- merge(neastmatrixt, neastmatrix4, by="row.names", sort=FALSE)
neastmatrixt <- merge(neastmatrixt, neastmatrix5, by="row.names", sort=FALSE)
neastmatrixt <- merge(neastmatrixt, neastmatrix6, by="row.names", sort=FALSE)

neastmatrixt <- as.data.frame(neastmatrixt[-1])
neastmatrixt <- as.data.frame(neastmatrixt[-1])
neastmatrixt <- as.data.frame(neastmatrixt[-1])
neastmatrixt <- as.data.frame(neastmatrixt[-1])

neastmatrix <- as.zoo(neastmatrixt)


pdf("neastseriesHWMIdGCM.pdf")
neast <- xyplot(neastmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,250),main="NEAST",
             auto.key = FALSE)
dev.off()

## 1. cmed

ext8 <- sp8 + sp9
cmedseries1 <- lapply(h_indexes1, FUN=function(x) mask(x, border1))
cmedseries2 <- lapply(h_indexes2, FUN=function(x) mask(x, border2))
cmedseries4 <- lapply(h_indexes4, FUN=function(x) mask(x, border4))
cmedseries5 <- lapply(h_indexes5, FUN=function(x) mask(x, border5))
cmedseries6 <- lapply(h_indexes6, FUN=function(x) mask(x, border6))

cmedseries1 <- lapply(cmedseries1, FUN=function(x) crop(x, ext8))
cmedseries2 <- lapply(cmedseries2, FUN=function(x) crop(x, ext8))
cmedseries4 <- lapply(cmedseries4, FUN=function(x) crop(x, ext8))
cmedseries5 <- lapply(cmedseries5, FUN=function(x) crop(x, ext8))
cmedseries6 <- lapply(cmedseries6, FUN=function(x) crop(x, ext8))

cmedseries1 <- lapply(cmedseries1, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
cmedseries2 <- lapply(cmedseries2, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
cmedseries3 <- lapply(cmedseries3, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
cmedseries4 <- lapply(cmedseries4, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
cmedseries5 <- lapply(cmedseries5, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
cmedseries6 <- lapply(cmedseries6, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

cmedmatrix1 <- do.call(cbind, cmedseries1)
cmedmatrix2 <- do.call(cbind, cmedseries2)
cmedmatrix4 <- do.call(cbind, cmedseries4)
cmedmatrix5 <- do.call(cbind, cmedseries5)
cmedmatrix6 <- do.call(cbind, cmedseries6)

z <- as.zoo(cmedmatrix1)
z <- as.zoo(cmedmatrix2)
z <- as.zoo(cmedmatrix4)
z <- as.zoo(cmedmatrix5)
z <- as.zoo(cmedmatrix6)

cmedmatrix1 <- as.data.frame(cmedmatrix1)
cmedmatrix2 <- as.data.frame(cmedmatrix2)
cmedmatrix4 <- as.data.frame(cmedmatrix4)
cmedmatrix5 <- as.data.frame(cmedmatrix5)
cmedmatrix6 <- as.data.frame(cmedmatrix6)

rows<- c(1, 2, 3, 4,5, 6,7, 8, 9,10, 11,12, 13,14, 15,  16, 17,18,19,20, 21,22,23,24,25,26, 27, 28, 29,30, 31,32,33, 34,35,36,37,38, 39,  40,41,42,43, 44,45, 46, 47, 48,49, 50,51, 52,  53, 54,55, 56, 57,58,59, 60, 61,62, 63, 64, 65, 66,  67, 68,69,70,71, 72,73,74, 75, 76,77,78,79,  80, 81, 82, 83, 84,  85, 86, 87, 88,  89,  90, 91, 92, 93,  94,  95,  96, 97, 98, 99, 100,101,102,103,104,105,106,107, 108, 109,110,111,112,113,114, 115,116, 117,118,119,120,121,122,123,124,125,126)
colnames(cmedmatrix1) <- nn1
colnames(cmedmatrix2) <- nn2
colnames(cmedmatrix4) <- nn4
colnames(cmedmatrix5) <- nn5
colnames(cmedmatrix6) <- nn6

rownames(cmedmatrix1) <- rows
rownames(cmedmatrix2) <- rows
rownames(cmedmatrix4) <- rows
rownames(cmedmatrix5) <- rows
rownames(cmedmatrix6) <- rows

cmedmatrixt <- merge(cmedmatrix1, cmedmatrix2, by="row.names", sort=FALSE)
cmedmatrixt <- merge(cmedmatrixt, cmedmatrix4, by="row.names", sort=FALSE)
cmedmatrixt <- merge(cmedmatrixt, cmedmatrix5, by="row.names", sort=FALSE)
cmedmatrixt <- merge(cmedmatrixt, cmedmatrix6, by="row.names", sort=FALSE)

cmedmatrixt <- as.data.frame(cmedmatrixt[-1])
cmedmatrixt <- as.data.frame(cmedmatrixt[-1])
cmedmatrixt <- as.data.frame(cmedmatrixt[-1])
cmedmatrixt <- as.data.frame(cmedmatrixt[-1])

cmedmatrix <- as.zoo(cmedmatrixt)


myTheme$strip.background$col <- 'transparent'
myTheme$strip.shingle$col <- 'transparent'

pdf("cmedseriesHWMIdGCM.pdf")
cmed <- xyplot(cmedmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,450),main="CMED",
             auto.key = FALSE)
dev.off()


## 1. cafr

cafrseries1 <- lapply(h_indexes1, FUN=function(x) mask(x, border1))
cafrseries2 <- lapply(h_indexes2, FUN=function(x) mask(x, border2))
cafrseries4 <- lapply(h_indexes4, FUN=function(x) mask(x, border4))
cafrseries5 <- lapply(h_indexes5, FUN=function(x) mask(x, border5))
cafrseries6 <- lapply(h_indexes6, FUN=function(x) mask(x, border6))

cafrseries1 <- lapply(cafrseries1, FUN=function(x) crop(x, e10))
cafrseries2 <- lapply(cafrseries2, FUN=function(x) crop(x, e10))
cafrseries4 <- lapply(cafrseries4, FUN=function(x) crop(x, e10))
cafrseries5 <- lapply(cafrseries5, FUN=function(x) crop(x, e10))
cafrseries6 <- lapply(cafrseries6, FUN=function(x) crop(x, e10))

cafrseries1 <- lapply(cafrseries1, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
cafrseries2 <- lapply(cafrseries2, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
cafrseries3 <- lapply(cafrseries3, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
cafrseries4 <- lapply(cafrseries4, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
cafrseries5 <- lapply(cafrseries5, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
cafrseries6 <- lapply(cafrseries6, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

cafrmatrix1 <- do.call(cbind, cafrseries1)
cafrmatrix2 <- do.call(cbind, cafrseries2)
cafrmatrix4 <- do.call(cbind, cafrseries4)
cafrmatrix5 <- do.call(cbind, cafrseries5)
cafrmatrix6 <- do.call(cbind, cafrseries6)

z <- as.zoo(cafrmatrix1)
z <- as.zoo(cafrmatrix2)
z <- as.zoo(cafrmatrix4)
z <- as.zoo(cafrmatrix5)
z <- as.zoo(cafrmatrix6)

cafrmatrix1 <- as.data.frame(cafrmatrix1)
cafrmatrix2 <- as.data.frame(cafrmatrix2)
cafrmatrix4 <- as.data.frame(cafrmatrix4)
cafrmatrix5 <- as.data.frame(cafrmatrix5)
cafrmatrix6 <- as.data.frame(cafrmatrix6)

rows<- c(1, 2, 3, 4,5, 6,7, 8, 9,10, 11,12, 13,14, 15,  16, 17,18,19,20, 21,22,23,24,25,26, 27, 28, 29,30, 31,32,33, 34,35,36,37,38, 39,  40,41,42,43, 44,45, 46, 47, 48,49, 50,51, 52,  53, 54,55, 56, 57,58,59, 60, 61,62, 63, 64, 65, 66,  67, 68,69,70,71, 72,73,74, 75, 76,77,78,79,  80, 81, 82, 83, 84,  85, 86, 87, 88,  89,  90, 91, 92, 93,  94,  95,  96, 97, 98, 99, 100,101,102,103,104,105,106,107, 108, 109,110,111,112,113,114, 115,116, 117,118,119,120,121,122,123,124,125,126)
colnames(cafrmatrix1) <- nn1
colnames(cafrmatrix2) <- nn2
colnames(cafrmatrix4) <- nn4
colnames(cafrmatrix5) <- nn5
colnames(cafrmatrix6) <- nn6

rownames(cafrmatrix1) <- rows
rownames(cafrmatrix2) <- rows
rownames(cafrmatrix4) <- rows
rownames(cafrmatrix5) <- rows
rownames(cafrmatrix6) <- rows

cafrmatrixt <- merge(cafrmatrix1, cafrmatrix2, by="row.names", sort=FALSE)
cafrmatrixt <- merge(cafrmatrixt, cafrmatrix4, by="row.names", sort=FALSE)
cafrmatrixt <- merge(cafrmatrixt, cafrmatrix5, by="row.names", sort=FALSE)
cafrmatrixt <- merge(cafrmatrixt, cafrmatrix6, by="row.names", sort=FALSE)

cafrmatrixt <- as.data.frame(cafrmatrixt[-1])
cafrmatrixt <- as.data.frame(cafrmatrixt[-1])
cafrmatrixt <- as.data.frame(cafrmatrixt[-1])
cafrmatrixt <- as.data.frame(cafrmatrixt[-1])

cafrmatrix <- as.zoo(cafrmatrixt)



myTheme <- custom.theme.2(pch = 20, cex = 0.7, alpha=0.7)
myTheme$strip.background$col <- 'transparent'
myTheme$strip.shingle$col <- 'transparent'

pdf("cafrseriesHWMIdGCM.pdf")
cafr <- xyplot(cafrmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,450),main="CAFR",
             auto.key = FALSE)
dev.off()

## 1. east

eastseries1 <- lapply(h_indexes1, FUN=function(x) mask(x, border1))
eastseries2 <- lapply(h_indexes2, FUN=function(x) mask(x, border2))
eastseries4 <- lapply(h_indexes4, FUN=function(x) mask(x, border4))
eastseries5 <- lapply(h_indexes5, FUN=function(x) mask(x, border5))
eastseries6 <- lapply(h_indexes6, FUN=function(x) mask(x, border6))

eastseries1 <- lapply(eastseries1, FUN=function(x) crop(x, e11))
eastseries2 <- lapply(eastseries2, FUN=function(x) crop(x, e11))
eastseries4 <- lapply(eastseries4, FUN=function(x) crop(x, e11))
eastseries5 <- lapply(eastseries5, FUN=function(x) crop(x, e11))
eastseries6 <- lapply(eastseries6, FUN=function(x) crop(x, e11))

eastseries1 <- lapply(eastseries1, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
eastseries2 <- lapply(eastseries2, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
eastseries3 <- lapply(eastseries3, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
eastseries4 <- lapply(eastseries4, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
eastseries5 <- lapply(eastseries5, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
eastseries6 <- lapply(eastseries6, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

eastmatrix1 <- do.call(cbind, eastseries1)
eastmatrix2 <- do.call(cbind, eastseries2)
eastmatrix4 <- do.call(cbind, eastseries4)
eastmatrix5 <- do.call(cbind, eastseries5)
eastmatrix6 <- do.call(cbind, eastseries6)

z <- as.zoo(eastmatrix1)
z <- as.zoo(eastmatrix2)
z <- as.zoo(eastmatrix4)
z <- as.zoo(eastmatrix5)
z <- as.zoo(eastmatrix6)

eastmatrix1 <- as.data.frame(eastmatrix1)
eastmatrix2 <- as.data.frame(eastmatrix2)
eastmatrix4 <- as.data.frame(eastmatrix4)
eastmatrix5 <- as.data.frame(eastmatrix5)
eastmatrix6 <- as.data.frame(eastmatrix6)

rows<- c(1, 2, 3, 4,5, 6,7, 8, 9,10, 11,12, 13,14, 15,  16, 17,18,19,20, 21,22,23,24,25,26, 27, 28, 29,30, 31,32,33, 34,35,36,37,38, 39,  40,41,42,43, 44,45, 46, 47, 48,49, 50,51, 52,  53, 54,55, 56, 57,58,59, 60, 61,62, 63, 64, 65, 66,  67, 68,69,70,71, 72,73,74, 75, 76,77,78,79,  80, 81, 82, 83, 84,  85, 86, 87, 88,  89,  90, 91, 92, 93,  94,  95,  96, 97, 98, 99, 100,101,102,103,104,105,106,107, 108, 109,110,111,112,113,114, 115,116, 117,118,119,120,121,122,123,124,125,126)
colnames(eastmatrix1) <- nn1
colnames(eastmatrix2) <- nn2
colnames(eastmatrix4) <- nn4
colnames(eastmatrix5) <- nn5
colnames(eastmatrix6) <- nn6

rownames(eastmatrix1) <- rows
rownames(eastmatrix2) <- rows
rownames(eastmatrix4) <- rows
rownames(eastmatrix5) <- rows
rownames(eastmatrix6) <- rows

eastmatrixt <- merge(eastmatrix1, eastmatrix2, by="row.names", sort=FALSE)
eastmatrixt <- merge(eastmatrixt, eastmatrix4, by="row.names", sort=FALSE)
eastmatrixt <- merge(eastmatrixt, eastmatrix5, by="row.names", sort=FALSE)
eastmatrixt <- merge(eastmatrixt, eastmatrix6, by="row.names", sort=FALSE)

eastmatrixt <- as.data.frame(eastmatrixt[-1])
eastmatrixt <- as.data.frame(eastmatrixt[-1])
eastmatrixt <- as.data.frame(eastmatrixt[-1])
eastmatrixt <- as.data.frame(eastmatrixt[-1])

eastmatrix <- as.zoo(eastmatrixt)

pdf("eastseriesHWMIdGCM.pdf")
east <- xyplot(eastmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,250),main="EAST",
               auto.key =FALSE)
dev.off()

pdf("printseriesHWMIdGCM.pdf", width=10, height=10)
print(fr, split=c(1, 1, 3, 3), more=TRUE)
print(alps, split=c(2, 1, 3, 3), more=TRUE)
print(neast, split=c(3, 1, 3, 3), more=TRUE)
print(pi, split=c(1, 2, 3, 3), more=TRUE)
print(cmed, split=c(2, 2, 3, 3), more=TRUE)
print(east, split=c(3, 2, 3, 3), more=TRUE)
print(wafr, split=c(1, 3, 3, 3), more=TRUE)
print(cafr, split=c(2, 3, 3, 3), more=TRUE)
print(ewafr, split=c(3, 3, 3, 3))
dev.off()

##WSDI

c1 <- list("wsdiCanESM45_box.nc","wsdiCanESM85_box.nc")
c2 <- list("wsdiCNRM45_box.nc", "wsdiCNRM85_box.nc")
c3 <- list("wsdiEC-EARTH45_box.nc")
c4 <- list("wsdiIPSL45_box.nc", "wsdiIPSL85_box.nc")
c5 <- list("wsdiMPI45_box.nc", "wsdiMPI85_box.nc")
c6 <- list("wsdiHadGEM45_box.nc", "wsdiHadGEM85_box.nc")

nn1 <- c("CanESM", "CanESM")
nn2 <- c("CNRM", "CNRM")
nn3 <- c("EC-EARTH")
nn4 <- c("IPSL", "IPSL")
nn5 <- c("MPI", "MPI")
nn6 <- c("HadGEM", "HadGEM")

wg_indexes1 <- lapply(c1, stack)
wg_indexes2 <- lapply(c2, stack)
wg_indexes3 <- lapply(c3, stack)
wg_indexes4 <- lapply(c4, stack)
wg_indexes5 <- lapply(c5, stack)
wg_indexes6 <- lapply(c6, stack)

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

border1 <- fooborder(wg_indexes1[[1]])
border2 <- fooborder(wg_indexes2[[1]])
border3 <- fooborder(wg_indexes3[[1]])
border4 <- fooborder(wg_indexes4[[1]])
border5 <- fooborder(wg_indexes5[[1]])
border6 <- fooborder(wg_indexes6[[1]])

##Creo un indice temporal para hacer SetZ que me pide la función xyplot:

t <- seq(as.Date("1971-01-01"), as.Date("2100-01-01"), "year")

wg_indexes1 <- lapply(wg_indexes1, FUN=function(x) setZ(x, t))
wg_indexes2 <- lapply(wg_indexes2, FUN=function(x) setZ(x, t))
wg_indexes3 <- lapply(wg_indexes3, FUN=function(x) setZ(x, t))
wg_indexes4 <- lapply(wg_indexes4, FUN=function(x) setZ(x, t))
wg_indexes5 <- lapply(wg_indexes5, FUN=function(x) setZ(x, t))
wg_indexes6 <- lapply(wg_indexes6, FUN=function(x) setZ(x, t))

#1. Mediterráneo

#m <- extent(-25.63, 50.65, 26.73, 52.34)
med <- extent(-12, 40, 28, 50)

wg_indexesmed1 <- lapply(wg_indexes1, FUN=function(x) mask(x, border1))
wg_indexesmed2 <- lapply(wg_indexes2, FUN=function(x) mask(x, border2))
wg_indexesmed3 <- lapply(wg_indexes3, FUN=function(x) mask(x, border3))
wg_indexesmed4 <- lapply(wg_indexes4, FUN=function(x) mask(x, border4))
wg_indexesmed5 <- lapply(wg_indexes5, FUN=function(x) mask(x, border5))
wg_indexesmed6 <- lapply(wg_indexes6, FUN=function(x) mask(x, border6))

wg_indexesmed1 <- lapply(wg_indexesmed1, FUN=function(x) crop(x, med))
wg_indexesmed2 <- lapply(wg_indexesmed2, FUN=function(x) crop(x, med))
wg_indexesmed3 <- lapply(wg_indexesmed3, FUN=function(x) crop(x, med))
wg_indexesmed4 <- lapply(wg_indexesmed4, FUN=function(x) crop(x, med))
wg_indexesmed5 <- lapply(wg_indexesmed5, FUN=function(x) crop(x, med))
wg_indexesmed6 <- lapply(wg_indexesmed6, FUN=function(x) crop(x, med))

#1. Norte Europa

ne <- extent(-12, 40, 50, 73) 

wg_indexesne1 <- lapply(wg_indexes1, FUN=function(x) mask(x, border1))
wg_indexesne2 <- lapply(wg_indexes2, FUN=function(x) mask(x, border2))
wg_indexesne3 <- lapply(wg_indexes3, FUN=function(x) mask(x, border3))
wg_indexesne4 <- lapply(wg_indexes4, FUN=function(x) mask(x, border4))
wg_indexesne5 <- lapply(wg_indexes5, FUN=function(x) mask(x, border5))
wg_indexesne6 <- lapply(wg_indexes6, FUN=function(x) mask(x, border6))

wg_indexesne1 <- lapply(wg_indexesne1, FUN=function(x) crop(x, e))
wg_indexesne2 <- lapply(wg_indexesne2, FUN=function(x) crop(x, e))
wg_indexesne3 <- lapply(wg_indexesne3, FUN=function(x) crop(x, e))
wg_indexesne4 <- lapply(wg_indexesne4, FUN=function(x) crop(x, e))
wg_indexesne5 <- lapply(wg_indexesne5, FUN=function(x) crop(x, e))
wg_indexesne6 <- lapply(wg_indexesne6, FUN=function(x) crop(x, e))


##Media de los últimos 30 años MED:

wg_indexesLast301 <- lapply(wg_indexesmed1, FUN=function(x) x[[101:130]])
wg_indexesLast302 <- lapply(wg_indexesmed2, FUN=function(x) x[[101:130]])
wg_indexesLast303 <- lapply(wg_indexesmed3, FUN=function(x) x[[101:130]])
wg_indexesLast304 <- lapply(wg_indexesmed4, FUN=function(x) x[[101:130]])
wg_indexesLast305 <- lapply(wg_indexesmed5, FUN=function(x) x[[101:130]])
wg_indexesLast306 <- lapply(wg_indexesmed6, FUN=function(x) x[[101:130]])

wg_indexesmedLast30mean1 <- lapply(wg_indexesLast301, FUN=function(x) mean(x, na.rm=TRUE))
wg_indexesmedLast30mean2 <- lapply(wg_indexesLast302, FUN=function(x) mean(x, na.rm=TRUE))
wg_indexesmedLast30mean3 <- lapply(wg_indexesLast303, FUN=function(x) mean(x, na.rm=TRUE))
wg_indexesmedLast30mean4 <- lapply(wg_indexesLast304, FUN=function(x) mean(x, na.rm=TRUE))
wg_indexesmedLast30mean5 <- lapply(wg_indexesLast305, FUN=function(x) mean(x, na.rm=TRUE))
wg_indexesmedLast30mean6 <- lapply(wg_indexesLast306, FUN=function(x) mean(x, na.rm=TRUE))



##Media de los últimos 30 años NE:

wg_indexesLast301 <- lapply(wg_indexesne1, FUN=function(x) x[[101:130]])
wg_indexesLast302 <- lapply(wg_indexesne2, FUN=function(x) x[[101:130]])
wg_indexesLast303 <- lapply(wg_indexesne3, FUN=function(x) x[[101:130]])
wg_indexesLast304 <- lapply(wg_indexesne4, FUN=function(x) x[[101:130]])
wg_indexesLast305 <- lapply(wg_indexesne5, FUN=function(x) x[[101:130]])
wg_indexesLast306 <- lapply(wg_indexesne6, FUN=function(x) x[[101:130]])

wg_indexesRef <- lapply(wg_indexesne, FUN=function(x) x[[1:30]])

wg_indexesLast30mean1 <- lapply(wg_indexesLast301, FUN=function(x) mean(x, na.rm=TRUE))
wg_indexesLast30mean2 <- lapply(wg_indexesLast302, FUN=function(x) mean(x, na.rm=TRUE))
wg_indexesLast30mean3 <- lapply(wg_indexesLast303, FUN=function(x) mean(x, na.rm=TRUE))
wg_indexesLast30mean4 <- lapply(wg_indexesLast304, FUN=function(x) mean(x, na.rm=TRUE))
wg_indexesLast30mean5 <- lapply(wg_indexesLast305, FUN=function(x) mean(x, na.rm=TRUE))
wg_indexesLast30mean6 <- lapply(wg_indexesLast306, FUN=function(x) mean(x, na.rm=TRUE))

meanWSDI1med <- stack(unlist(wg_indexesmedLast30mean1))
meanWSDI2med <- stack(unlist(wg_indexesmedLast30mean2))
meanWSDI3med <- stack(unlist(wg_indexesmedLast30mean3))
meanWSDI4med <- stack(unlist(wg_indexesmedLast30mean4))
meanWSDI5med <- stack(unlist(wg_indexesmedLast30mean5))
meanWSDI6med <- stack(unlist(wg_indexesmedLast30mean6))

meanWSDIsinmar1med <- mask(meanWSDI1med, border1)
meanWSDIsinmar2med <- mask(meanWSDI2med, border2)
meanWSDIsinmar3med <- mask(meanWSDI3med, border3)
meanWSDIsinmar4med <- mask(meanWSDI4med, border4)
meanWSDIsinmar5med <- mask(meanWSDI5med, border5)
meanWSDIsinmar6med <- mask(meanWSDI6med, border6)

names(meanWSDIsinmar1med) <- nn1
names(meanWSDIsinmar2med) <- nn2
names(meanWSDIsinmar3med) <- nn3
names(meanWSDIsinmar4med) <- nn4
names(meanWSDIsinmar5med) <- nn5
names(meanWSDIsinmar6med) <- nn6

my.at <- seq(0,370, 30)

myColorkey <- list(at=my.at, ## where the colors change
                   labels=list(
                     at=my.at),
                   space="bottom",
                   width= .8)

w1 <- stack(meanWSDIsinmar1med[[1]],meanWSDIsinmar1med[[2]])
w2 <- stack(meanWSDIsinmar2med[[1]],meanWSDIsinmar2med[[2]])
w3 <- stack(meanWSDIsinmar3med[[1]])
w4 <- stack(meanWSDIsinmar4med[[1]],meanWSDIsinmar4med[[2]])
w5 <- stack(meanWSDIsinmar5med[[1]],meanWSDIsinmar5med[[2]])
w6 <- stack(meanWSDIsinmar6med[[1]],meanWSDIsinmar6med[[2]])

pdf("wsdiCCC.pdf", width=7, height=4)
levelplot(w1, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c("CanESM"), cex=1), names.attr=c("", ""), layout=c(2,1), colorkey=myColorkey)+
  layer(sp.lines(border, lwd=0.5))
dev.off()

pdf("wsdiCNRM.pdf", width=7, height=4)
levelplot(w2, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c("CNRM"), cex=1), names.attr=c("", ""), layout=c(2,1), colorkey=myColorkey)+
  layer(sp.lines(border, lwd=0.5))
dev.off()

pdf("wsdiEC-EARTH.pdf", width=7, height=4)
levelplot(w3, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45"), cex=1), xlab=NULL, ylab=list(c("EC-EARTH"), cex=1), names.attr=c(""), layout=c(1,1), colorkey=myColorkey)+
  layer(sp.lines(border, lwd=0.5))
dev.off()

pdf("wsdiIPSL.pdf", width=7, height=4)
levelplot(w4, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c("IPSL"), cex=1), names.attr=c("", ""), layout=c(2,1), colorkey=myColorkey)+
  layer(sp.lines(border, lwd=0.5))
dev.off()

pdf("wsdiMPI.pdf", width=7, height=4)
levelplot(w5, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c("MPI"), cex=1), names.attr=c("", ""), layout=c(2,1), colorkey=myColorkey)+
  layer(sp.lines(border, lwd=0.5))
dev.off()

pdf("wsdiHadGEM.pdf", width=7, height=4)
levelplot(w6, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("RCP45", "RCP85"), cex=1), xlab=NULL, ylab=list(c("HadGEM"), cex=1), names.attr=c("", ""), layout=c(2,1), colorkey=myColorkey)+
  layer(sp.lines(border, lwd=0.5))
dev.off()

##Figura 1

imean1 <- lapply(wg_indexesmedLast30mean1, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
imean2 <- lapply(wg_indexesmedLast30mean2, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
imean3 <- lapply(wg_indexesmedLast30mean3, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
imean4 <- lapply(wg_indexesmedLast30mean4, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
imean5 <- lapply(wg_indexesmedLast30mean5, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
imean6 <- lapply(wg_indexesmedLast30mean6, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

df1 <- lapply(imean1, as.data.frame) #66.4, 151
df2 <- lapply(imean2, as.data.frame) #44.4, 88
df3 <- lapply(imean3, as.data.frame) #44.5
df4 <- lapply(imean4, as.data.frame) #88.8, 187
df5 <- lapply(imean5, as.data.frame) #47, 133
df6 <- lapply(imean6, as.data.frame) #115, 200

mg <- merge.data.frame(df1, df2, by="row.names")
mg <- merge.data.frame(mg, df3, by="row.names")
mg <- merge.data.frame(mg, df4, by="row.names")
mg <- merge.data.frame(mg, df5, by="row.names")
mg <- merge.data.frame(mg, df6, by="row.names")
mg <- as.matrix(mg[-1])
mg <- as.matrix(mg[-1])
mg <- as.matrix(mg[-1])
mg <- as.matrix(mg[-1])
mg <- as.matrix(mg[-1])

mod <- c(1,1,1,1,1,1,1,1,1,1,1)

wgM <- matrix(c(mg, mod), 11, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))

wgM1_45 <- matrix(c(wgM[[1]], wgM[[3]], wgM[[5]], wgM[[7]], wgM[[8]],wgM[[10]], mod), 6, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))
wgM1_85 <- matrix(c(wgM[[2]], wgM[4], wgM[[6]], wgM[[9]], wgM[[11]], mod), 5, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))

wgM <- as.data.frame(wgM)
wgM45 <- as.data.frame(wgM1_45)
wgM85 <- as.data.frame(wgM1_85)

df$color <- palette[as.factor(wgM45$resolution)]


## Barplot WSDI

bmed <- brick(wg_indexesmedLast30mean1)
bmed <- cellStats(bmed, mean)
pdf("wsdibarplotCCC.pdf", width=7, height=4)
bmed <- barplot(bmed, col=c('grey','orange'), las=2, ylab="WSDI", main="MED CanESM", legend.text= nn)
dev.off()

#bne <- brick(wg_indexesLast30mean1)
#bne <- cellStats(bne, mean)
#pdf("wsdinebarplotCCC.pdf", width=7, height=4)
#bne <- barplot(bne, col=c('grey','orange'), las=2, ylab="WSDI", main="NE CanESM", legend.text= nn)
#dev.off()

bmed <- brick(wg_indexesmedLast30mean2)
bmed <- cellStats(bmed, mean)
pdf("wsdibarplotCNRM.pdf", width=7, height=4)
bmed <- barplot(bmed, col=c('grey','orange'), las=2, ylab="WSDI", main="MED CNRM", legend.text= nn)
dev.off()

#bne <- brick(wg_indexesLast30mean2)
#bne <- cellStats(bne, mean)
#pdf("wsdinebarplotCNRM.pdf", width=7, height=4)
#bne <- barplot(bne, col=c('grey','orange'), las=2, ylab="WSDI", main="NE CNRM", legend.text= nn)
#dev.off()

bmed <- brick(wg_indexesmedLast30mean6)
bmed <- cellStats(bmed, mean)
pdf("wsdibarplotHadGEM.pdf", width=7, height=4)
bmed <- barplot(bmed, col=c('grey','orange'), las=2, ylab="WSDI", main="MED HadGEM", legend.text= nn)
dev.off()

#bne <- brick(wg_indexesLast30mean6)
#bne <- cellStats(bne, mean)
#pdf("hwmidnebarplotHadGEM.pdf", width=7, height=4)
#bne <- barplot(bne, col=c('grey','orange'), las=2, ylab="WSDI", main="NE HadGEM", legend.text= nn)
#dev.off()

bmed <- brick(wg_indexesmedLast30mean3)
bmed <- cellStats(bmed, mean)
pdf("wsdibarplotEC-EARTH.pdf", width=7, height=4)
bmed <- barplot(bmed, col=c('grey','orange'), las=2, ylab="WSDI", main="MED EC-EARTH", legend.text= nn)
dev.off()

#bne <- brick(wg_indexesLast30mean3)
#bne <- cellStats(bne, mean)
#pdf("wsdinebarplotEC-EARTH.pdf", width=7, height=4)
#bne <- barplot(bne, col=c('grey','orange'), las=2, ylab="WSDI", main="NE EC-EARTH", legend.text= nn)
#dev.off()

bmed <- brick(wg_indexesLast30mean4)
bmed <- cellStats(bmed, mean)
pdf("wsdimedbarplotIPSL.pdf", width=7, height=4)
bmed <- barplot(bmed, col=c('grey','orange'), las=2, ylab="WSDI", main="MED IPSL", legend.text= nn)
dev.off()

#bne <- brick(wg_indexesLast30mean4)
#bne <- cellStats(bne, mean)
#pdf("wsdinebarplotIPSL.pdf", width=7, height=4)
#bne <- barplot(bne, col=c('grey','orange'), las=2, ylab="WSDI", main="NE IPSL", legend.text= nn)
#dev.off()

bmed <- brick(wg_indexesmedLast30mean5)
bmed <- cellStats(bmed, mean)
pdf("wsdibarplotMPI.pdf", width=7, height=4)
bmed <- barplot(bmed, col=c('grey','orange'), las=2, ylab="WSDI", main="MED MPI", legend.text= nn)
dev.off()

#bne <- brick(wg_indexesLast30mean5)
#bne <- cellStats(bne, mean)
#pdf("wsdinebarplotMPI.pdf", width=7, height=4)
#bne <- barplot(bne, col=c('grey','orange'), las=2, ylab="WSDI", main="NE MPI", legend.text= nn)
#dev.off()

##Series temporales wsdi

c1 <- list("wsdiCanESM85_box_runmean.nc")
c2 <- list("wsdiCNRM85_box_runmean.nc")
c4 <- list("wsdiHadGEM85_box_runmean.nc")
c5 <- list("wsdiIPSL85_box_runmean.nc")
c6 <- list("wsdiMPI85_box_runmean.nc")

nn1 <- c("CanESM85")
nn2 <- c("CNRM85")
nn4 <- c("HadGEM85")
nn5 <- c("IPSL85")
nn6 <- c("MPI85")

h_indexes1 <- lapply(c1, stack)
h_indexes2 <- lapply(c2, stack)
h_indexes4 <- lapply(c4, stack)
h_indexes5 <- lapply(c5, stack)
h_indexes6 <- lapply(c6, stack)

border1 <- fooborder(h_indexes1[[1]])
border2 <- fooborder(h_indexes2[[1]])
border4 <- fooborder(h_indexes4[[1]])
border5 <- fooborder(h_indexes5[[1]])
border6 <- fooborder(h_indexes6[[1]])

idxm <- seq(as.Date("1973-01-01"), as.Date("2098-12-01"), "year")

h_indexes1 <- lapply(h_indexes1, FUN=function(x) setZ(x, idxm))
h_indexes2 <- lapply(h_indexes2, FUN=function(x) setZ(x, idxm))
h_indexes4 <- lapply(h_indexes4, FUN=function(x) setZ(x, idxm))
h_indexes5 <- lapply(h_indexes5, FUN=function(x) setZ(x, idxm))
h_indexes6 <- lapply(h_indexes6, FUN=function(x) setZ(x, idxm))

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
e1 <- e1 + e2

piseries1 <- lapply(h_indexes1, FUN=function(x) mask(x, border1))
piseries2 <- lapply(h_indexes2, FUN=function(x) mask(x, border2))
piseries4 <- lapply(h_indexes4, FUN=function(x) mask(x, border4))
piseries5 <- lapply(h_indexes5, FUN=function(x) mask(x, border5))
piseries6 <- lapply(h_indexes6, FUN=function(x) mask(x, border6))

piseries1 <- lapply(piseries1, FUN=function(x) crop(x, ext1))
piseries2 <- lapply(piseries2, FUN=function(x) crop(x, ext1))
piseries4 <- lapply(piseries4, FUN=function(x) crop(x, ext1))
piseries5 <- lapply(piseries5, FUN=function(x) crop(x, ext1))
piseries6 <- lapply(piseries6, FUN=function(x) crop(x, ext1))

piseries1 <- lapply(piseries1, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
piseries2 <- lapply(piseries2, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
piseries3 <- lapply(piseries3, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
piseries4 <- lapply(piseries4, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
piseries5 <- lapply(piseries5, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
piseries6 <- lapply(piseries6, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

pimatrix1 <- do.call(cbind, piseries1)
pimatrix2 <- do.call(cbind, piseries2)
pimatrix4 <- do.call(cbind, piseries4)
pimatrix5 <- do.call(cbind, piseries5)
pimatrix6 <- do.call(cbind, piseries6)

z <- as.zoo(pimatrix1)
z <- as.zoo(pimatrix2)
z <- as.zoo(pimatrix4)
z <- as.zoo(pimatrix5)
z <- as.zoo(pimatrix6)

pimatrix1 <- as.data.frame(pimatrix1)
pimatrix2 <- as.data.frame(pimatrix2)
pimatrix4 <- as.data.frame(pimatrix4)
pimatrix5 <- as.data.frame(pimatrix5)
pimatrix6 <- as.data.frame(pimatrix6)

rows<- c(1, 2, 3, 4,5, 6,7, 8, 9,10, 11,12, 13,14, 15,  16, 17,18,19,20, 21,22,23,24,25,26, 27, 28, 29,30, 31,32,33, 34,35,36,37,38, 39,  40,41,42,43, 44,45, 46, 47, 48,49, 50,51, 52,  53, 54,55, 56, 57,58,59, 60, 61,62, 63, 64, 65, 66,  67, 68,69,70,71, 72,73,74, 75, 76,77,78,79,  80, 81, 82, 83, 84,  85, 86, 87, 88,  89,  90, 91, 92, 93,  94,  95,  96, 97, 98, 99, 100,101,102,103,104,105,106,107, 108, 109,110,111,112,113,114, 115,116, 117,118,119,120,121,122,123,124,125,126)
colnames(pimatrix1) <- nn1
colnames(pimatrix2) <- nn2
colnames(pimatrix4) <- nn4
colnames(pimatrix5) <- nn5
colnames(pimatrix6) <- nn6

rownames(pimatrix1) <- rows
rownames(pimatrix2) <- rows
rownames(pimatrix4) <- rows
rownames(pimatrix5) <- rows
rownames(pimatrix6) <- rows

pimatrixt <- merge(pimatrix1, pimatrix2, by="row.names", sort=FALSE)
pimatrixt <- merge(pimatrixt, pimatrix4, by="row.names", sort=FALSE)
pimatrixt <- merge(pimatrixt, pimatrix5, by="row.names", sort=FALSE)
pimatrixt <- merge(pimatrixt, pimatrix6, by="row.names", sort=FALSE)

pimatrixt <- as.data.frame(pimatrixt[-1])
pimatrixt <- as.data.frame(pimatrixt[-1])
pimatrixt <- as.data.frame(pimatrixt[-1])
pimatrixt <- as.data.frame(pimatrixt[-1])
library(zoo)
pimatrix <- as.zoo(pimatrixt)



myTheme <- custom.theme.2(pch = 20, cex = 0.7, alpha=0.7)
myTheme$strip.background$col <- 'transparent'
myTheme$strip.shingle$col <- 'transparent'

pdf("piserieswsdiGCM.pdf")
pi <- xyplot(pimatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,250),main="PI",
             auto.key = FALSE)
dev.off()

## 1. fr

frseries1 <- lapply(h_indexes1, FUN=function(x) mask(x, border1))
frseries2 <- lapply(h_indexes2, FUN=function(x) mask(x, border2))
frseries4 <- lapply(h_indexes4, FUN=function(x) mask(x, border4))
frseries5 <- lapply(h_indexes5, FUN=function(x) mask(x, border5))
frseries6 <- lapply(h_indexes6, FUN=function(x) mask(x, border6))

frseries1 <- lapply(frseries1, FUN=function(x) crop(x, e3))
frseries2 <- lapply(frseries2, FUN=function(x) crop(x, e3))
frseries4 <- lapply(frseries4, FUN=function(x) crop(x, e3))
frseries5 <- lapply(frseries5, FUN=function(x) crop(x, e3))
frseries6 <- lapply(frseries6, FUN=function(x) crop(x, e3))

frseries1 <- lapply(frseries1, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
frseries2 <- lapply(frseries2, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
frseries3 <- lapply(frseries3, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
frseries4 <- lapply(frseries4, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
frseries5 <- lapply(frseries5, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
frseries6 <- lapply(frseries6, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

frmatrix1 <- do.call(cbind, frseries1)
frmatrix2 <- do.call(cbind, frseries2)
frmatrix4 <- do.call(cbind, frseries4)
frmatrix5 <- do.call(cbind, frseries5)
frmatrix6 <- do.call(cbind, frseries6)

z <- as.zoo(frmatrix1)
z <- as.zoo(frmatrix2)
z <- as.zoo(frmatrix4)
z <- as.zoo(frmatrix5)
z <- as.zoo(frmatrix6)

frmatrix1 <- as.data.frame(frmatrix1)
frmatrix2 <- as.data.frame(frmatrix2)
frmatrix4 <- as.data.frame(frmatrix4)
frmatrix5 <- as.data.frame(frmatrix5)
frmatrix6 <- as.data.frame(frmatrix6)

rows<- c(1, 2, 3, 4,5, 6,7, 8, 9,10, 11,12, 13,14, 15,  16, 17,18,19,20, 21,22,23,24,25,26, 27, 28, 29,30, 31,32,33, 34,35,36,37,38, 39,  40,41,42,43, 44,45, 46, 47, 48,49, 50,51, 52,  53, 54,55, 56, 57,58,59, 60, 61,62, 63, 64, 65, 66,  67, 68,69,70,71, 72,73,74, 75, 76,77,78,79,  80, 81, 82, 83, 84,  85, 86, 87, 88,  89,  90, 91, 92, 93,  94,  95,  96, 97, 98, 99, 100,101,102,103,104,105,106,107, 108, 109,110,111,112,113,114, 115,116, 117,118,119,120,121,122,123,124,125,126)
colnames(frmatrix1) <- nn1
colnames(frmatrix2) <- nn2
colnames(frmatrix4) <- nn4
colnames(frmatrix5) <- nn5
colnames(frmatrix6) <- nn6

rownames(frmatrix1) <- rows
rownames(frmatrix2) <- rows
rownames(frmatrix4) <- rows
rownames(frmatrix5) <- rows
rownames(frmatrix6) <- rows

frmatrixt <- merge(frmatrix1, frmatrix2, by="row.names", sort=FALSE)
frmatrixt <- merge(frmatrixt, frmatrix4, by="row.names", sort=FALSE)
frmatrixt <- merge(frmatrixt, frmatrix5, by="row.names", sort=FALSE)
frmatrixt <- merge(frmatrixt, frmatrix6, by="row.names", sort=FALSE)

frmatrixt <- as.data.frame(frmatrixt[-1])
frmatrixt <- as.data.frame(frmatrixt[-1])
frmatrixt <- as.data.frame(frmatrixt[-1])
frmatrixt <- as.data.frame(frmatrixt[-1])

frmatrix <- as.zoo(frmatrixt)

pdf("frserieswsdiGCM.pdf")
fr <- xyplot(frmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,250),main="FR",
             auto.key =list(corner=c(1, 1), x = 0.6, y = 0.95, cex=0.4))
dev.off()

## 1. alps

alpsseries1 <- lapply(h_indexes1, FUN=function(x) mask(x, border1))
alpsseries2 <- lapply(h_indexes2, FUN=function(x) mask(x, border2))
alpsseries4 <- lapply(h_indexes4, FUN=function(x) mask(x, border4))
alpsseries5 <- lapply(h_indexes5, FUN=function(x) mask(x, border5))
alpsseries6 <- lapply(h_indexes6, FUN=function(x) mask(x, border6))

alpsseries1 <- lapply(alpsseries1, FUN=function(x) crop(x, e4))
alpsseries2 <- lapply(alpsseries2, FUN=function(x) crop(x, e4))
alpsseries4 <- lapply(alpsseries4, FUN=function(x) crop(x, e4))
alpsseries5 <- lapply(alpsseries5, FUN=function(x) crop(x, e4))
alpsseries6 <- lapply(alpsseries6, FUN=function(x) crop(x, e4))

alpsseries1 <- lapply(alpsseries1, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
alpsseries2 <- lapply(alpsseries2, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
alpsseries3 <- lapply(alpsseries3, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
alpsseries4 <- lapply(alpsseries4, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
alpsseries5 <- lapply(alpsseries5, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
alpsseries6 <- lapply(alpsseries6, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

alpsmatrix1 <- do.call(cbind, alpsseries1)
alpsmatrix2 <- do.call(cbind, alpsseries2)
alpsmatrix4 <- do.call(cbind, alpsseries4)
alpsmatrix5 <- do.call(cbind, alpsseries5)
alpsmatrix6 <- do.call(cbind, alpsseries6)

z <- as.zoo(alpsmatrix1)
z <- as.zoo(alpsmatrix2)
z <- as.zoo(alpsmatrix4)
z <- as.zoo(alpsmatrix5)
z <- as.zoo(alpsmatrix6)

alpsmatrix1 <- as.data.frame(alpsmatrix1)
alpsmatrix2 <- as.data.frame(alpsmatrix2)
alpsmatrix4 <- as.data.frame(alpsmatrix4)
alpsmatrix5 <- as.data.frame(alpsmatrix5)
alpsmatrix6 <- as.data.frame(alpsmatrix6)

rows<- c(1, 2, 3, 4,5, 6,7, 8, 9,10, 11,12, 13,14, 15,  16, 17,18,19,20, 21,22,23,24,25,26, 27, 28, 29,30, 31,32,33, 34,35,36,37,38, 39,  40,41,42,43, 44,45, 46, 47, 48,49, 50,51, 52,  53, 54,55, 56, 57,58,59, 60, 61,62, 63, 64, 65, 66,  67, 68,69,70,71, 72,73,74, 75, 76,77,78,79,  80, 81, 82, 83, 84,  85, 86, 87, 88,  89,  90, 91, 92, 93,  94,  95,  96, 97, 98, 99, 100,101,102,103,104,105,106,107, 108, 109,110,111,112,113,114, 115,116, 117,118,119,120,121,122,123,124,125,126)
colnames(alpsmatrix1) <- nn1
colnames(alpsmatrix2) <- nn2
colnames(alpsmatrix4) <- nn4
colnames(alpsmatrix5) <- nn5
colnames(alpsmatrix6) <- nn6

rownames(alpsmatrix1) <- rows
rownames(alpsmatrix2) <- rows
rownames(alpsmatrix4) <- rows
rownames(alpsmatrix5) <- rows
rownames(alpsmatrix6) <- rows

alpsmatrixt <- merge(alpsmatrix1, alpsmatrix2, by="row.names", sort=FALSE)
alpsmatrixt <- merge(alpsmatrixt, alpsmatrix4, by="row.names", sort=FALSE)
alpsmatrixt <- merge(alpsmatrixt, alpsmatrix5, by="row.names", sort=FALSE)
alpsmatrixt <- merge(alpsmatrixt, alpsmatrix6, by="row.names", sort=FALSE)

alpsmatrixt <- as.data.frame(alpsmatrixt[-1])
alpsmatrixt <- as.data.frame(alpsmatrixt[-1])
alpsmatrixt <- as.data.frame(alpsmatrixt[-1])
alpsmatrixt <- as.data.frame(alpsmatrixt[-1])

alpsmatrix <- as.zoo(alpsmatrixt)


pdf("alpsserieswsdiGCM.pdf")
alps <- xyplot(alpsmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,250),main="ALPS",
               auto.key = FALSE)
dev.off()

## 1. wafr

wafrseries1 <- lapply(h_indexes1, FUN=function(x) mask(x, border1))
wafrseries2 <- lapply(h_indexes2, FUN=function(x) mask(x, border2))
wafrseries4 <- lapply(h_indexes4, FUN=function(x) mask(x, border4))
wafrseries5 <- lapply(h_indexes5, FUN=function(x) mask(x, border5))
wafrseries6 <- lapply(h_indexes6, FUN=function(x) mask(x, border6))

wafrseries1 <- lapply(wafrseries1, FUN=function(x) crop(x, e5))
wafrseries2 <- lapply(wafrseries2, FUN=function(x) crop(x, e5))
wafrseries4 <- lapply(wafrseries4, FUN=function(x) crop(x, e5))
wafrseries5 <- lapply(wafrseries5, FUN=function(x) crop(x, e5))
wafrseries6 <- lapply(wafrseries6, FUN=function(x) crop(x, e5))

wafrseries1 <- lapply(wafrseries1, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
wafrseries2 <- lapply(wafrseries2, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
wafrseries3 <- lapply(wafrseries3, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
wafrseries4 <- lapply(wafrseries4, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
wafrseries5 <- lapply(wafrseries5, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
wafrseries6 <- lapply(wafrseries6, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

wafrmatrix1 <- do.call(cbind, wafrseries1)
wafrmatrix2 <- do.call(cbind, wafrseries2)
wafrmatrix4 <- do.call(cbind, wafrseries4)
wafrmatrix5 <- do.call(cbind, wafrseries5)
wafrmatrix6 <- do.call(cbind, wafrseries6)

z <- as.zoo(wafrmatrix1)
z <- as.zoo(wafrmatrix2)
z <- as.zoo(wafrmatrix4)
z <- as.zoo(wafrmatrix5)
z <- as.zoo(wafrmatrix6)

wafrmatrix1 <- as.data.frame(wafrmatrix1)
wafrmatrix2 <- as.data.frame(wafrmatrix2)
wafrmatrix4 <- as.data.frame(wafrmatrix4)
wafrmatrix5 <- as.data.frame(wafrmatrix5)
wafrmatrix6 <- as.data.frame(wafrmatrix6)

rows<- c(1, 2, 3, 4,5, 6,7, 8, 9,10, 11,12, 13,14, 15,  16, 17,18,19,20, 21,22,23,24,25,26, 27, 28, 29,30, 31,32,33, 34,35,36,37,38, 39,  40,41,42,43, 44,45, 46, 47, 48,49, 50,51, 52,  53, 54,55, 56, 57,58,59, 60, 61,62, 63, 64, 65, 66,  67, 68,69,70,71, 72,73,74, 75, 76,77,78,79,  80, 81, 82, 83, 84,  85, 86, 87, 88,  89,  90, 91, 92, 93,  94,  95,  96, 97, 98, 99, 100,101,102,103,104,105,106,107, 108, 109,110,111,112,113,114, 115,116, 117,118,119,120,121,122,123,124,125,126)
colnames(wafrmatrix1) <- nn1
colnames(wafrmatrix2) <- nn2
colnames(wafrmatrix4) <- nn4
colnames(wafrmatrix5) <- nn5
colnames(wafrmatrix6) <- nn6

rownames(wafrmatrix1) <- rows
rownames(wafrmatrix2) <- rows
rownames(wafrmatrix4) <- rows
rownames(wafrmatrix5) <- rows
rownames(wafrmatrix6) <- rows

wafrmatrixt <- merge(wafrmatrix1, wafrmatrix2, by="row.names", sort=FALSE)
wafrmatrixt <- merge(wafrmatrixt, wafrmatrix4, by="row.names", sort=FALSE)
wafrmatrixt <- merge(wafrmatrixt, wafrmatrix5, by="row.names", sort=FALSE)
wafrmatrixt <- merge(wafrmatrixt, wafrmatrix6, by="row.names", sort=FALSE)

wafrmatrixt <- as.data.frame(wafrmatrixt[-1])
wafrmatrixt <- as.data.frame(wafrmatrixt[-1])
wafrmatrixt <- as.data.frame(wafrmatrixt[-1])
wafrmatrixt <- as.data.frame(wafrmatrixt[-1])

wafrmatrix <- as.zoo(wafrmatrixt)


pdf("wafrserieswsdiGCM.pdf")
wafr <- xyplot(wafrmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,450),main="WAFR",
               auto.key = FALSE)
dev.off()

## 1. ewafr

ewafrseries1 <- lapply(h_indexes1, FUN=function(x) mask(x, border1))
ewafrseries2 <- lapply(h_indexes2, FUN=function(x) mask(x, border2))
ewafrseries4 <- lapply(h_indexes4, FUN=function(x) mask(x, border4))
ewafrseries5 <- lapply(h_indexes5, FUN=function(x) mask(x, border5))
ewafrseries6 <- lapply(h_indexes6, FUN=function(x) mask(x, border6))

ewafrseries1 <- lapply(ewafrseries1, FUN=function(x) crop(x, e6))
ewafrseries2 <- lapply(ewafrseries2, FUN=function(x) crop(x, e6))
ewafrseries4 <- lapply(ewafrseries4, FUN=function(x) crop(x, e6))
ewafrseries5 <- lapply(ewafrseries5, FUN=function(x) crop(x, e6))
ewafrseries6 <- lapply(ewafrseries6, FUN=function(x) crop(x, e6))

ewafrseries1 <- lapply(ewafrseries1, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
ewafrseries2 <- lapply(ewafrseries2, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
ewafrseries3 <- lapply(ewafrseries3, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
ewafrseries4 <- lapply(ewafrseries4, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
ewafrseries5 <- lapply(ewafrseries5, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
ewafrseries6 <- lapply(ewafrseries6, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

ewafrmatrix1 <- do.call(cbind, ewafrseries1)
ewafrmatrix2 <- do.call(cbind, ewafrseries2)
ewafrmatrix4 <- do.call(cbind, ewafrseries4)
ewafrmatrix5 <- do.call(cbind, ewafrseries5)
ewafrmatrix6 <- do.call(cbind, ewafrseries6)

z <- as.zoo(ewafrmatrix1)
z <- as.zoo(ewafrmatrix2)
z <- as.zoo(ewafrmatrix4)
z <- as.zoo(ewafrmatrix5)
z <- as.zoo(ewafrmatrix6)

ewafrmatrix1 <- as.data.frame(ewafrmatrix1)
ewafrmatrix2 <- as.data.frame(ewafrmatrix2)
ewafrmatrix4 <- as.data.frame(ewafrmatrix4)
ewafrmatrix5 <- as.data.frame(ewafrmatrix5)
ewafrmatrix6 <- as.data.frame(ewafrmatrix6)

rows<- c(1, 2, 3, 4,5, 6,7, 8, 9,10, 11,12, 13,14, 15,  16, 17,18,19,20, 21,22,23,24,25,26, 27, 28, 29,30, 31,32,33, 34,35,36,37,38, 39,  40,41,42,43, 44,45, 46, 47, 48,49, 50,51, 52,  53, 54,55, 56, 57,58,59, 60, 61,62, 63, 64, 65, 66,  67, 68,69,70,71, 72,73,74, 75, 76,77,78,79,  80, 81, 82, 83, 84,  85, 86, 87, 88,  89,  90, 91, 92, 93,  94,  95,  96, 97, 98, 99, 100,101,102,103,104,105,106,107, 108, 109,110,111,112,113,114, 115,116, 117,118,119,120,121,122,123,124,125,126)
colnames(ewafrmatrix1) <- nn1
colnames(ewafrmatrix2) <- nn2
colnames(ewafrmatrix4) <- nn4
colnames(ewafrmatrix5) <- nn5
colnames(ewafrmatrix6) <- nn6

rownames(ewafrmatrix1) <- rows
rownames(ewafrmatrix2) <- rows
rownames(ewafrmatrix4) <- rows
rownames(ewafrmatrix5) <- rows
rownames(ewafrmatrix6) <- rows

ewafrmatrixt <- merge(ewafrmatrix1, ewafrmatrix2, by="row.names", sort=FALSE)
ewafrmatrixt <- merge(ewafrmatrixt, ewafrmatrix4, by="row.names", sort=FALSE)
ewafrmatrixt <- merge(ewafrmatrixt, ewafrmatrix5, by="row.names", sort=FALSE)
ewafrmatrixt <- merge(ewafrmatrixt, ewafrmatrix6, by="row.names", sort=FALSE)

ewafrmatrixt <- as.data.frame(ewafrmatrixt[-1])
ewafrmatrixt <- as.data.frame(ewafrmatrixt[-1])
ewafrmatrixt <- as.data.frame(ewafrmatrixt[-1])
ewafrmatrixt <- as.data.frame(ewafrmatrixt[-1])

ewafrmatrix <- as.zoo(ewafrmatrixt)


pdf("ewafrserieswsdiGCM.pdf")
ewafr <- xyplot(ewafrmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,450),main="EAFR",
                auto.key = FALSE)
dev.off()

## 1. neast

neastseries1 <- lapply(h_indexes1, FUN=function(x) mask(x, border1))
neastseries2 <- lapply(h_indexes2, FUN=function(x) mask(x, border2))
neastseries4 <- lapply(h_indexes4, FUN=function(x) mask(x, border4))
neastseries5 <- lapply(h_indexes5, FUN=function(x) mask(x, border5))
neastseries6 <- lapply(h_indexes6, FUN=function(x) mask(x, border6))

neastseries1 <- lapply(neastseries1, FUN=function(x) crop(x, e7))
neastseries2 <- lapply(neastseries2, FUN=function(x) crop(x, e7))
neastseries4 <- lapply(neastseries4, FUN=function(x) crop(x, e7))
neastseries5 <- lapply(neastseries5, FUN=function(x) crop(x, e7))
neastseries6 <- lapply(neastseries6, FUN=function(x) crop(x, e7))

neastseries1 <- lapply(neastseries1, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
neastseries2 <- lapply(neastseries2, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
neastseries3 <- lapply(neastseries3, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
neastseries4 <- lapply(neastseries4, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
neastseries5 <- lapply(neastseries5, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
neastseries6 <- lapply(neastseries6, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

neastmatrix1 <- do.call(cbind, neastseries1)
neastmatrix2 <- do.call(cbind, neastseries2)
neastmatrix4 <- do.call(cbind, neastseries4)
neastmatrix5 <- do.call(cbind, neastseries5)
neastmatrix6 <- do.call(cbind, neastseries6)

z <- as.zoo(neastmatrix1)
z <- as.zoo(neastmatrix2)
z <- as.zoo(neastmatrix4)
z <- as.zoo(neastmatrix5)
z <- as.zoo(neastmatrix6)

neastmatrix1 <- as.data.frame(neastmatrix1)
neastmatrix2 <- as.data.frame(neastmatrix2)
neastmatrix4 <- as.data.frame(neastmatrix4)
neastmatrix5 <- as.data.frame(neastmatrix5)
neastmatrix6 <- as.data.frame(neastmatrix6)

rows<- c(1, 2, 3, 4,5, 6,7, 8, 9,10, 11,12, 13,14, 15,  16, 17,18,19,20, 21,22,23,24,25,26, 27, 28, 29,30, 31,32,33, 34,35,36,37,38, 39,  40,41,42,43, 44,45, 46, 47, 48,49, 50,51, 52,  53, 54,55, 56, 57,58,59, 60, 61,62, 63, 64, 65, 66,  67, 68,69,70,71, 72,73,74, 75, 76,77,78,79,  80, 81, 82, 83, 84,  85, 86, 87, 88,  89,  90, 91, 92, 93,  94,  95,  96, 97, 98, 99, 100,101,102,103,104,105,106,107, 108, 109,110,111,112,113,114, 115,116, 117,118,119,120,121,122,123,124,125,126)
colnames(neastmatrix1) <- nn1
colnames(neastmatrix2) <- nn2
colnames(neastmatrix4) <- nn4
colnames(neastmatrix5) <- nn5
colnames(neastmatrix6) <- nn6

rownames(neastmatrix1) <- rows
rownames(neastmatrix2) <- rows
rownames(neastmatrix4) <- rows
rownames(neastmatrix5) <- rows
rownames(neastmatrix6) <- rows

neastmatrixt <- merge(neastmatrix1, neastmatrix2, by="row.names", sort=FALSE)
neastmatrixt <- merge(neastmatrixt, neastmatrix4, by="row.names", sort=FALSE)
neastmatrixt <- merge(neastmatrixt, neastmatrix5, by="row.names", sort=FALSE)
neastmatrixt <- merge(neastmatrixt, neastmatrix6, by="row.names", sort=FALSE)

neastmatrixt <- as.data.frame(neastmatrixt[-1])
neastmatrixt <- as.data.frame(neastmatrixt[-1])
neastmatrixt <- as.data.frame(neastmatrixt[-1])
neastmatrixt <- as.data.frame(neastmatrixt[-1])

neastmatrix <- as.zoo(neastmatrixt)


pdf("neastserieswsdiGCM.pdf")
neast <- xyplot(neastmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,250),main="NEAST",
                auto.key = FALSE)
dev.off()

## 1. cmed

e8 <- sp8 + sp9
cmedseries1 <- lapply(h_indexes1, FUN=function(x) mask(x, border1))
cmedseries2 <- lapply(h_indexes2, FUN=function(x) mask(x, border2))
cmedseries4 <- lapply(h_indexes4, FUN=function(x) mask(x, border4))
cmedseries5 <- lapply(h_indexes5, FUN=function(x) mask(x, border5))
cmedseries6 <- lapply(h_indexes6, FUN=function(x) mask(x, border6))

cmedseries1 <- lapply(cmedseries1, FUN=function(x) crop(x, e8))
cmedseries2 <- lapply(cmedseries2, FUN=function(x) crop(x, e8))
cmedseries4 <- lapply(cmedseries4, FUN=function(x) crop(x, e8))
cmedseries5 <- lapply(cmedseries5, FUN=function(x) crop(x, e8))
cmedseries6 <- lapply(cmedseries6, FUN=function(x) crop(x, e8))

cmedseries1 <- lapply(cmedseries1, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
cmedseries2 <- lapply(cmedseries2, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
cmedseries3 <- lapply(cmedseries3, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
cmedseries4 <- lapply(cmedseries4, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
cmedseries5 <- lapply(cmedseries5, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
cmedseries6 <- lapply(cmedseries6, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

cmedmatrix1 <- do.call(cbind, cmedseries1)
cmedmatrix2 <- do.call(cbind, cmedseries2)
cmedmatrix4 <- do.call(cbind, cmedseries4)
cmedmatrix5 <- do.call(cbind, cmedseries5)
cmedmatrix6 <- do.call(cbind, cmedseries6)

z <- as.zoo(cmedmatrix1)
z <- as.zoo(cmedmatrix2)
z <- as.zoo(cmedmatrix4)
z <- as.zoo(cmedmatrix5)
z <- as.zoo(cmedmatrix6)

cmedmatrix1 <- as.data.frame(cmedmatrix1)
cmedmatrix2 <- as.data.frame(cmedmatrix2)
cmedmatrix4 <- as.data.frame(cmedmatrix4)
cmedmatrix5 <- as.data.frame(cmedmatrix5)
cmedmatrix6 <- as.data.frame(cmedmatrix6)

rows<- c(1, 2, 3, 4,5, 6,7, 8, 9,10, 11,12, 13,14, 15,  16, 17,18,19,20, 21,22,23,24,25,26, 27, 28, 29,30, 31,32,33, 34,35,36,37,38, 39,  40,41,42,43, 44,45, 46, 47, 48,49, 50,51, 52,  53, 54,55, 56, 57,58,59, 60, 61,62, 63, 64, 65, 66,  67, 68,69,70,71, 72,73,74, 75, 76,77,78,79,  80, 81, 82, 83, 84,  85, 86, 87, 88,  89,  90, 91, 92, 93,  94,  95,  96, 97, 98, 99, 100,101,102,103,104,105,106,107, 108, 109,110,111,112,113,114, 115,116, 117,118,119,120,121,122,123,124,125,126)
colnames(cmedmatrix1) <- nn1
colnames(cmedmatrix2) <- nn2
colnames(cmedmatrix4) <- nn4
colnames(cmedmatrix5) <- nn5
colnames(cmedmatrix6) <- nn6

rownames(cmedmatrix1) <- rows
rownames(cmedmatrix2) <- rows
rownames(cmedmatrix4) <- rows
rownames(cmedmatrix5) <- rows
rownames(cmedmatrix6) <- rows

cmedmatrixt <- merge(cmedmatrix1, cmedmatrix2, by="row.names", sort=FALSE)
cmedmatrixt <- merge(cmedmatrixt, cmedmatrix4, by="row.names", sort=FALSE)
cmedmatrixt <- merge(cmedmatrixt, cmedmatrix5, by="row.names", sort=FALSE)
cmedmatrixt <- merge(cmedmatrixt, cmedmatrix6, by="row.names", sort=FALSE)

cmedmatrixt <- as.data.frame(cmedmatrixt[-1])
cmedmatrixt <- as.data.frame(cmedmatrixt[-1])
cmedmatrixt <- as.data.frame(cmedmatrixt[-1])
cmedmatrixt <- as.data.frame(cmedmatrixt[-1])

cmedmatrix <- as.zoo(cmedmatrixt)

pdf("cmedserieswsdiGCM.pdf")
cmed <- xyplot(cmedmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,450),main="CMED",
               auto.key = FALSE)
dev.off()


## 1. cafr

cafrseries1 <- lapply(h_indexes1, FUN=function(x) mask(x, border1))
cafrseries2 <- lapply(h_indexes2, FUN=function(x) mask(x, border2))
cafrseries4 <- lapply(h_indexes4, FUN=function(x) mask(x, border4))
cafrseries5 <- lapply(h_indexes5, FUN=function(x) mask(x, border5))
cafrseries6 <- lapply(h_indexes6, FUN=function(x) mask(x, border6))

cafrseries1 <- lapply(cafrseries1, FUN=function(x) crop(x, e10))
cafrseries2 <- lapply(cafrseries2, FUN=function(x) crop(x, e10))
cafrseries4 <- lapply(cafrseries4, FUN=function(x) crop(x, e10))
cafrseries5 <- lapply(cafrseries5, FUN=function(x) crop(x, e10))
cafrseries6 <- lapply(cafrseries6, FUN=function(x) crop(x, e10))

cafrseries1 <- lapply(cafrseries1, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
cafrseries2 <- lapply(cafrseries2, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
cafrseries3 <- lapply(cafrseries3, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
cafrseries4 <- lapply(cafrseries4, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
cafrseries5 <- lapply(cafrseries5, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
cafrseries6 <- lapply(cafrseries6, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

cafrmatrix1 <- do.call(cbind, cafrseries1)
cafrmatrix2 <- do.call(cbind, cafrseries2)
cafrmatrix4 <- do.call(cbind, cafrseries4)
cafrmatrix5 <- do.call(cbind, cafrseries5)
cafrmatrix6 <- do.call(cbind, cafrseries6)

z <- as.zoo(cafrmatrix1)
z <- as.zoo(cafrmatrix2)
z <- as.zoo(cafrmatrix4)
z <- as.zoo(cafrmatrix5)
z <- as.zoo(cafrmatrix6)

cafrmatrix1 <- as.data.frame(cafrmatrix1)
cafrmatrix2 <- as.data.frame(cafrmatrix2)
cafrmatrix4 <- as.data.frame(cafrmatrix4)
cafrmatrix5 <- as.data.frame(cafrmatrix5)
cafrmatrix6 <- as.data.frame(cafrmatrix6)

rows<- c(1, 2, 3, 4,5, 6,7, 8, 9,10, 11,12, 13,14, 15,  16, 17,18,19,20, 21,22,23,24,25,26, 27, 28, 29,30, 31,32,33, 34,35,36,37,38, 39,  40,41,42,43, 44,45, 46, 47, 48,49, 50,51, 52,  53, 54,55, 56, 57,58,59, 60, 61,62, 63, 64, 65, 66,  67, 68,69,70,71, 72,73,74, 75, 76,77,78,79,  80, 81, 82, 83, 84,  85, 86, 87, 88,  89,  90, 91, 92, 93,  94,  95,  96, 97, 98, 99, 100,101,102,103,104,105,106,107, 108, 109,110,111,112,113,114, 115,116, 117,118,119,120,121,122,123,124,125,126)
colnames(cafrmatrix1) <- nn1
colnames(cafrmatrix2) <- nn2
colnames(cafrmatrix4) <- nn4
colnames(cafrmatrix5) <- nn5
colnames(cafrmatrix6) <- nn6

rownames(cafrmatrix1) <- rows
rownames(cafrmatrix2) <- rows
rownames(cafrmatrix4) <- rows
rownames(cafrmatrix5) <- rows
rownames(cafrmatrix6) <- rows

cafrmatrixt <- merge(cafrmatrix1, cafrmatrix2, by="row.names", sort=FALSE)
cafrmatrixt <- merge(cafrmatrixt, cafrmatrix4, by="row.names", sort=FALSE)
cafrmatrixt <- merge(cafrmatrixt, cafrmatrix5, by="row.names", sort=FALSE)
cafrmatrixt <- merge(cafrmatrixt, cafrmatrix6, by="row.names", sort=FALSE)

cafrmatrixt <- as.data.frame(cafrmatrixt[-1])
cafrmatrixt <- as.data.frame(cafrmatrixt[-1])
cafrmatrixt <- as.data.frame(cafrmatrixt[-1])
cafrmatrixt <- as.data.frame(cafrmatrixt[-1])

cafrmatrix <- as.zoo(cafrmatrixt)

pdf("cafrserieswsdiGCM.pdf")
cafr <- xyplot(cafrmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,450),main="CAFR",
               auto.key = FALSE)
dev.off()

## 1. east

eastseries1 <- lapply(h_indexes1, FUN=function(x) mask(x, border1))
eastseries2 <- lapply(h_indexes2, FUN=function(x) mask(x, border2))
eastseries4 <- lapply(h_indexes4, FUN=function(x) mask(x, border4))
eastseries5 <- lapply(h_indexes5, FUN=function(x) mask(x, border5))
eastseries6 <- lapply(h_indexes6, FUN=function(x) mask(x, border6))

eastseries1 <- lapply(eastseries1, FUN=function(x) crop(x, e11))
eastseries2 <- lapply(eastseries2, FUN=function(x) crop(x, e11))
eastseries4 <- lapply(eastseries4, FUN=function(x) crop(x, e11))
eastseries5 <- lapply(eastseries5, FUN=function(x) crop(x, e11))
eastseries6 <- lapply(eastseries6, FUN=function(x) crop(x, e11))

eastseries1 <- lapply(eastseries1, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
eastseries2 <- lapply(eastseries2, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
eastseries3 <- lapply(eastseries3, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
eastseries4 <- lapply(eastseries4, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
eastseries5 <- lapply(eastseries5, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
eastseries6 <- lapply(eastseries6, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))

eastmatrix1 <- do.call(cbind, eastseries1)
eastmatrix2 <- do.call(cbind, eastseries2)
eastmatrix4 <- do.call(cbind, eastseries4)
eastmatrix5 <- do.call(cbind, eastseries5)
eastmatrix6 <- do.call(cbind, eastseries6)

z <- as.zoo(eastmatrix1)
z <- as.zoo(eastmatrix2)
z <- as.zoo(eastmatrix4)
z <- as.zoo(eastmatrix5)
z <- as.zoo(eastmatrix6)

eastmatrix1 <- as.data.frame(eastmatrix1)
eastmatrix2 <- as.data.frame(eastmatrix2)
eastmatrix4 <- as.data.frame(eastmatrix4)
eastmatrix5 <- as.data.frame(eastmatrix5)
eastmatrix6 <- as.data.frame(eastmatrix6)

rows<- c(1, 2, 3, 4,5, 6,7, 8, 9,10, 11,12, 13,14, 15,  16, 17,18,19,20, 21,22,23,24,25,26, 27, 28, 29,30, 31,32,33, 34,35,36,37,38, 39,  40,41,42,43, 44,45, 46, 47, 48,49, 50,51, 52,  53, 54,55, 56, 57,58,59, 60, 61,62, 63, 64, 65, 66,  67, 68,69,70,71, 72,73,74, 75, 76,77,78,79,  80, 81, 82, 83, 84,  85, 86, 87, 88,  89,  90, 91, 92, 93,  94,  95,  96, 97, 98, 99, 100,101,102,103,104,105,106,107, 108, 109,110,111,112,113,114, 115,116, 117,118,119,120,121,122,123,124,125,126)
colnames(eastmatrix1) <- nn1
colnames(eastmatrix2) <- nn2
colnames(eastmatrix4) <- nn4
colnames(eastmatrix5) <- nn5
colnames(eastmatrix6) <- nn6

rownames(eastmatrix1) <- rows
rownames(eastmatrix2) <- rows
rownames(eastmatrix4) <- rows
rownames(eastmatrix5) <- rows
rownames(eastmatrix6) <- rows

eastmatrixt <- merge(eastmatrix1, eastmatrix2, by="row.names", sort=FALSE)
eastmatrixt <- merge(eastmatrixt, eastmatrix4, by="row.names", sort=FALSE)
eastmatrixt <- merge(eastmatrixt, eastmatrix5, by="row.names", sort=FALSE)
eastmatrixt <- merge(eastmatrixt, eastmatrix6, by="row.names", sort=FALSE)

eastmatrixt <- as.data.frame(eastmatrixt[-1])
eastmatrixt <- as.data.frame(eastmatrixt[-1])
eastmatrixt <- as.data.frame(eastmatrixt[-1])
eastmatrixt <- as.data.frame(eastmatrixt[-1])

eastmatrix <- as.zoo(eastmatrixt)


pdf("eastserieswsdiGCM.pdf")
east <- xyplot(eastmatrix, superpose=TRUE, type='b', cex=0.5, lwd=1.5, par.settings=myTheme, ylim=c(0,250),main="EAST",
               auto.key =FALSE)
dev.off()

pdf("printseriesWSDIGCM.pdf", width=10, height=10)
print(fr, split=c(1, 1, 3, 3), more=TRUE)
print(alps, split=c(2, 1, 3, 3), more=TRUE)
print(neast, split=c(3, 1, 3, 3), more=TRUE)
print(pi, split=c(1, 2, 3, 3), more=TRUE)
print(cmed, split=c(2, 2, 3, 3), more=TRUE)
print(east, split=c(3, 2, 3, 3), more=TRUE)
print(wafr, split=c(1, 3, 3, 3), more=TRUE)
print(cafr, split=c(2, 3, 3, 3), more=TRUE)
print(ewafr, split=c(3, 3, 3, 3))
dev.off()