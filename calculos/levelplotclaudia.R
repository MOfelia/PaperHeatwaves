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
levelplot(w, margin=FALSE, scales=list(x=list(cex=.3), y=list(cex=0.3)),  xlab.top=list(c("EC-EARTH", "CCC"), cex=1), xlab=NULL, ylab=list(c("RACMO", "RCA4"), cex=1), names.att
          r=c("RCP4.5", "RCP8.5", "RCP4.5", "RCP8.5", "RCP4.5", "RCP8.5"), layout=c(4,2), par.settings=rasterTheme(region=rev(div.pal)), at=my.at, colorkey=list(space='bottom', labels=list(at=my.at)))+                                                                                                                                                                  
  layer(sp.lines(border, lwd=0.5))                                                                                                                                             
dev.off()  