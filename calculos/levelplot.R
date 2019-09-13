library(ncdf4)
library(raster)
library(rasterVis)
library(rgdal) 
library(maptools)
library(mapdata)
library(RColorBrewer)
myraster1=stack("hwmi130prueba1lonlat.nc")
hwmimean= calc(myraster1, fun=mean, na.rm=TRUE)

#mypalette<-brewer.pal(9,"YlOrRd")
#myTheme <- simpleTheme(col = mypalette,
                       pch=19, alpha=.6)

levelplot(myraster1[[1]])
levelplot(myraster1, layers=1, margin=FALSE, xlab="RCA", ylab="EC-EARTH", main="HWMI", 
          contour=FALSE, par.settings=YlOrRdTheme)

#proj <- CRS('+proj=longlat +ellps=WGS84')
#mapaSHP <- readShapeLines('/home/UCLM/mofelia.molina/R/x86_64-redhat-linux-gnu-library/3.4/', proj4string=proj)


#pruebas para hacer una figura de temperaturas:
raster45=stack("tasmax_EUR-11_CNRM-CERFACS-CNRM-CM5_rcp45_r1i1p1_CLMcom-CCLM4-8-17_v1_day_2006-2100.nc", varname="tasmax")
raster85=stack("tasmax_EUR-11_CNRM-CERFACS-CNRM-CM5_rcp85_r1i1p1_CLMcom-CCLM4-8-17_v1_day_2006-2100.nc", varname="tasmax")

#Ahora, habría que cambiar la proyección


#Después, calculamos la media del periodo
r45= calc(raster45, fun=mean, na.rm=TRUE)  #este cálculo tarda una hora

#con esta función se dibuja el mapa de Europa con los límites que tienen los .nc
#boundaries = map("worldHires", xlim = c(-45,65.5), ylim = c(21.5, 73))

#Opción 2 para el mapa:

ext <- as.vector(extent(hwmimean))

boundaries <- map('worldHires', fill=TRUE,
                   xlim=ext[1:2], ylim=ext[3:4],
                   plot=FALSE)
IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
bPols <- map2SpatialPolygons(boundaries, IDs=IDs,
                               proj4string=CRS(projection(hwmimean)))

p = levelplot(hwmimean, margin=FALSE, xlab="RCA", ylab="EC-EARTH", main="HWMI", 
          contour=FALSE, par.settings=YlOrRdTheme, at=do.breaks(c(0,100), 10))
    + layer(sp.polygons(bPols))


#paleta de color 
library(RColorBrewer)
display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE,
                                      colorblindFriendly=FALSE)

#Para mostrar todas las gráficas juntas:

c(p,p1,p2)
