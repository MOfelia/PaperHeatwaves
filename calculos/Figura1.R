

imeanM <- lapply(indexesmedLast30meanM, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
imeanM_11 <- lapply(indexesmedLast30meanM_11, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
imean44 <- lapply(indexesmedLast30mean, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
imean11 <- lapply(indexesmedLast30mean_11, FUN=function(x) cellStats(x, stat='mean', na.rm=TRUE))
dfM <- lapply(imeanM, as.data.frame)
dfM_11 <- lapply(imeanM_11, as.data.frame)
df44 <- lapply(imean44, as.data.frame)
df11 <- lapply(imean11, as.data.frame)
m <- merge.data.frame(df44, df11, by="row.names")
m <- merge.data.frame(m, dfM, by="row.names")
m <- merge.data.frame(m, dfM_11, by="row.names")
m <- as.matrix(m[-3])
m <- as.matrix(m[-1])
m <- as.matrix(m[-1])
mod1 <- c(44, 11,44, 11, 44, 11,44, 11,44, 11,44, 11,44, 11,44, 11,44, 11,44, 11,44, 11, 44, 11)
mod <- c(44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44,11,11,11,11,11, 11,11,11,11,11,11,11,11,11,11, 11,11,11,11,11, 44, 44, 44, 44, 11, 11, 11, 11)

M <- matrix(c(m, mod), 48, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))


M1_45 <- matrix(c(M[[1]], M[23], M[[3]], M[[21]], M[[5]],M[[25]],M[[7]], M[[27]],M[[9]], M[[29]], M[[11]],M[[31]],M[[13]],M[[33]],M[[15]],M[[35]],M[[17]],M[[37]],M[[19]],M[[39]], M[[41]],M[[45]], M[[43]], M[[47]], mod1), 24, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))
M1_85 <- matrix(c(M[[2]],M[[24]], M[[4]], M[[22]],M[[6]],M[[26]],M[[8]],M[[28]],M[[10]], M[[30]],M[[12]],
                     M[[32]],M[14],M[34],M[[16]],M[[36]],M[[18]], M[[38]],M[[20]], M[[40]], M[[42]],M[[46]],M[[44]], M[[48]], mod1), 24, 2, byrow=FALSE, dimnames=list(NULL, c("model","resolution")))

M <- as.data.frame(M)
M45 <- as.data.frame(M1_45)
M85 <- as.data.frame(M1_85)

df$color <- palette[as.factor(M45$resolution)]


nn_44_45 <- c("44CCC-RCA", "11CNRM-CCLM","44CNRM-ALADIN", "11CNRM-ALADIN", "44CNRM-RCA", "11CNRM-RCA",  "44ICHEC-RACMO", "11ICHEC-RACMO", "44ICHEC-RCA", "11ICHEC-RCA", "44IPSL-RCA", "11IPSL-RCA", "44IPSL-WRF",  "11IPSL-WRF", "44MPI-CCLM","11MPI-CCLM", "44MPI-RCA", "11MPI-RCA", "44MPI-REMO", "11MPI-REMO", "44HadGEM-RACMO", "11HadGEM-RACMO","44HadGEM-RCA", "11HadGEM-RCA")

#b45<- barplot.default(as.numeric(M45$model), ylab = "HWMId", main = "RCP4.5", col=df$color, las=2, names.arg = nn_44_45, las=2, ylim=c(0,50), cex.names=0.6)

nn_44_85 <- c("44CCC-RCA", "11CNRM-CCLM","44CNRM-ALADIN", "11CNRM-ALADIN", "44CNRM-RCA", "11CNRM-RCA",  "44ICHEC-RACMO", "11ICHEC-RACMO", "44ICHEC-RCA", "11ICHEC-RCA", "44IPSL-RCA", "11IPSL-RCA", "44IPSL-WRF",  "11IPSL-WRF", "44MPI-CCLM","11MPI-CCLM", "44MPI-RCA", "11MPI-RCA", "44MPI-REMO", "11MPI-REMO", "44HadGEM-RACMO", "11HadGEM-RACMO", "44HadGEM-RCA", "11HadGEM-RCA")

c45 <- c(16.06598,8.019021,6.254013, 7.444083,  6.045284, 6.980263, 6.017233, 5.630969, 8.167413,8.690041, 16.00991,17.65458, 14.12973
            ,15.61324,9.168062, 7.879629,9.665896, 10.50521,9.218978, 8.483061, 19.73375,18.12334, 22.68577,25.44299)

c85 <- c(71.91478, 20.31296,13.88244, 18.28386,  16.4474, 18.80206, 20.85873, 18.18016, 29.18743,
        31.76524 ,71.94312, 78.52964, 61.68495, 66.23563, 34.47505, 27.17715, 37.88862, 41.77478,
         35.50312 ,33.21821, 77.74431, 70.04025, 93.91026 ,97.57647)

mycols <- c("orange", "gray")
b45 <- barplot(c45, col = mycols[M45$resolution], names.arg = nn_44_45, las=2, ylim=c(0,100), cex.names=0.6, main="RCP4.5", ylab="HWMId", legend=(M85$resolution[1:2]), args.legend = list(x = 'right'))
b85 <- barplot(c85, col = mycols[M45$resolution], names.arg = nn_44_85, las=2, ylim=c(0,100), cex.names=0.6, main="RCP8.5", ylab="HWMId", legend=(M85$resolution[1:2]), args.legend = list(x = 'right'))


#b45<- barplot.default(height = cbind(m[[1]], m[[23]], m[[3]], m[21], m[[5]],m[[25]],m[[7]], m[[27]],m[[9]], m[[29]], m[[11]],m[31],m[13],m[33],m[[15]],m[[35]],m[[17]],m[[37]],m[[19]],m[[39]], m[[41]],m[[45]], m[[43]], m[[47]]), ylab = "HWMId", main = "RCP4.5", col=c("lightblue"), names.arg = nn_44_45, las=2, ylim=c(0,50), cex.names=0.6)

#b45<- barplot.default(height = cbind(M[[1]], M[[23]], M[[3]], M[21], M[[5]],M[[25]],M[[7]], M[[27]],M[[9]], M[[29]], M[[11]],M[31],M[13],M[33],M[[15]],M[[35]],M[[17]],M[[37]],M[[19]],M[[39]], M[[41]],M[[45]], M[[43]], M[[47]]), ylab = "HWMId", main = "RCP4.5", col=c("lightblue", "orange"), names.arg = nn_44_45, las=2, ylim=c(0,50), cex.names=0.6, width = 0.7)                                  

#b85 <-barplot.default(height = cbind(m[[2]],m[[24]], m[[4]], m[[22]],m[[6]],m[[26]],m[[8]],m[[28]],m[[10]], m[[30]],m[[12]], m[[32]],m[14],m[34],m[[16]],m[[36]],m[[18]], m[[38]],m[[20]], m[[40]], m[[42]],m[[44]]), ylab = "HWMId", main = "RCP8.5", col=c("orange"), names.arg = nn_44_85, las=2, ylim=c(0,100), cex.names=0.6)
                                  

#names <- c("44CCC-RCA45", "44CCC-RCA85", "44CNRM-ALADIN45", "44CNRM-ALADIN85", "44CNRM-RCA45", "44CNRM-RCA85", "44ICHEC-RACMO45", "44ICHEC-RACMO85",  "44ICHEC-RCA45", "44ICHEC-RCA85", "44IPSL-RCA45", "44IPSL-RCA85",  "44IPSL-WRF45", "44IPSL-WRF85", "44MPI-CCLM45", "44MPI-CCLM85",  "44MPI-RCA45", "44MPI-RCA85",  "44MPI-REMO45", "44MPI-REMO85", "11CNRM-ALADIN45", "11CNRM-ALADIN85", "11CNRM-CCLM45", "11CNRM-CCLM85", "11CNRM-RCA45", "11CNRM-RCA85", "11ICHEC-RACMO45", "11ICHEC-RACMO85", "11ICHEC-RCA45", "11ICHEC-RCA85", "11IPSL-RCA45", "11IPSL-RCA85",  "11IPSL-WRF45", "11IPSL-WRF85", "11MPI-CCLM45", "11MPI-CCLM85",  "11MPI-RCA45", "11MPI-RCA85",  "11MPI-REMO45", "11MPI-REMO85", "44MOHC-RACMO45", "44MOHC-RACMO85", "44MOHC-RCA45",  "44MOHC-RCA85")
#resolution <- c(44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44,11,11,11,11,11, 11,11,11,11,11,11,11,11,11,11, 11,11,11,11,11, 44, 44, 44, 44)
#df <- data.frame(names, m, resolution)

                                                                    
                                  
                              