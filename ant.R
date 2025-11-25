

library(Metrics)
cal_NSE <- function(observed, predicted) {
  mean_observed <- mean(observed)
  numerator <- sum((observed - predicted)^2)
  denominator <- sum((observed - mean_observed)^2)
  NSE <- 1 - (numerator / denominator)
  return(NSE)
}

cal_PBIAS <- function(observed, predicted) {
  sum_observed <- sum(observed)
  sum_diff <- sum(observed-predicted)
  PBIAS <- (sum_diff / sum_observed) * 100
  return(PBIAS)
}

cwd <- "/home/smq/code/1/tyh_20240406"
setwd(cwd)

SHUI <- file.path(cwd, "SHUI")
parfile <- file.path(cwd, "dat/tyh_par_ant.txt")
usefile <- file.path(cwd, "dat/tyh_antuse.txt")
resultfile<-file.path(cwd, "antres0426.txt")

par_file <- file.path(cwd, "par_rmse.txt")
par_data <- read.csv(par_file, sep = "\t", header = TRUE)
gw_mix    <- par_data$gw_mix  #gw_mix
percop    <- par_data$percop  #percop           
ratem     <- par_data$ratem   #ratem         
kex       <- par_data$kex     #kex           
kt        <- par_data$kt      #kt        
kdif      <- par_data$kdif    #kdif  

DTsoil    <- par_data$DTsoil #DTsoil
DTwater   <- par_data$DTwater #DTwater
Kpwater   <- par_data$Kpwater #Kpwater
Kpsoil    <- par_data$Kpsoil #Kpsoil
Kpsed     <- par_data$Kpsed #Kpsed


data1 <- readLines(parfile)   #<Parameters
data1 <-as.data.frame(data1)
data2 <- readLines(usefile)   #<ant use
data2 <-as.data.frame(data2)

RMSE   <- matrix(nrow = nrow(par_data))
rmse5  <- matrix(nrow = nrow(par_data))
rmse9  <- matrix(nrow = nrow(par_data))
rmse7  <- matrix(nrow = nrow(par_data))
rmse15 <- matrix(nrow = nrow(par_data))
rmse17 <- matrix(nrow = nrow(par_data))
rmse20 <- matrix(nrow = nrow(par_data))
rmse23 <- matrix(nrow = nrow(par_data))

NSE<-matrix(nrow = nrow(par_data))
nse5  <- matrix(nrow = nrow(par_data))
nse9  <- matrix(nrow = nrow(par_data))
nse7  <- matrix(nrow = nrow(par_data))
nse15 <- matrix(nrow = nrow(par_data))
nse17 <- matrix(nrow = nrow(par_data))
nse20 <- matrix(nrow = nrow(par_data))
nse23 <- matrix(nrow = nrow(par_data))

R2  <- matrix(nrow = nrow(par_data))
r5  <- matrix(nrow = nrow(par_data))
r9  <- matrix(nrow = nrow(par_data))
r7  <- matrix(nrow = nrow(par_data))
r15 <- matrix(nrow = nrow(par_data))
r17 <- matrix(nrow = nrow(par_data))
r20 <- matrix(nrow = nrow(par_data))
r23 <- matrix(nrow = nrow(par_data))

PBAIS  <- matrix(nrow = nrow(par_data))
pbais5  <- matrix(nrow = nrow(par_data))
pbais9  <- matrix(nrow = nrow(par_data))
pbais7  <- matrix(nrow = nrow(par_data))
pbais15 <- matrix(nrow = nrow(par_data))
pbais17 <- matrix(nrow = nrow(par_data))
pbais20 <- matrix(nrow = nrow(par_data))
pbais23 <- matrix(nrow = nrow(par_data))

for (i in 1:nrow(par_data)) {
  data1[5,]  <-paste("gw_mix",gw_mix[i],  sep=" ")
  data1[6,]  <-paste("percop",percop[i], sep=" ")
  data1[7,]  <-paste("ratem" ,ratem[i], sep=" ")
  data1[9,]  <-paste("kex",kex[i], sep=" ")
  data1[12,]  <-paste("kt",kt[i], sep=" ")
  data1[13,]  <-paste("kdif",kdif[i], sep=" ")
  
  data2[2,]<-paste("TC	0.930147059	0.579	0.75	0.734	0.154754474	0.775	0.004243575	0.525",
                   DTsoil[i],DTwater[i],Kpwater[i],"0.046881338	444.43	17.2",Kpsoil[i],
                   "0.002751	0.00212	0.001489",Kpsed[i],"1700",sep=" ")
  
  write.table(data1,parfile,append = FALSE,row.names=F,col.names=F,quote=F)
  write.table(data2,usefile,append = FALSE,row.names=F,col.names=F,quote=F)
  
  system2(SHUI, args = c("-n 80"))
  
  
  ##calculate rems
  simriv5  <- read.table(file.path(cwd, "out/conc_riv5.txt"),header = T)#ng/L
  obsriv5  <- read.table(file.path(cwd, "dat/antriv5.txt")  ,header = T)
  riv5     <- merge(simriv5,obsriv5,by="date")
  rmse5[i]  <- rmse(riv5$TC.y,riv5$TC.x)
  nse5[i]   <- cal_NSE(riv5$TC.y,riv5$TC.x)
  r5[i]     <-cor(riv5$TC.y,riv5$TC.x)^2
  pbais5[i]    <- cal_PBIAS(riv5$TC.y,riv5$TC.x)
  
  simriv9  <- read.table(file.path(cwd, "out/conc_riv9.txt"),header = T)#ng/L
  obsriv9  <- read.table(file.path(cwd, "dat/antriv9.txt")  ,header = T)
  riv9     <- merge(simriv9,obsriv9,by="date")
  rmse9[i]    <- rmse(riv9$TC.y,riv9$TC.x)
  nse9[i]   <- cal_NSE(riv9$TC.y,riv9$TC.x)
  r9[i]     <-cor(riv9$TC.y,riv9$TC.x)^2
  pbais9[i]    <- cal_PBIAS(riv9$TC.y,riv9$TC.x)
  
  simriv7  <- read.table(file.path(cwd, "out/conc_riv7.txt"),header = T)#ng/L
  obsriv7  <- read.table(file.path(cwd, "dat/antriv7.txt")  ,header = T)
  riv7     <- merge(simriv7,obsriv7,by="date")
  rmse7[i] <- rmse(riv7$TC.y,riv7$TC.x)
  nse7[i]   <- cal_NSE(riv7$TC.y,riv7$TC.x)
  r7[i]     <-cor(riv7$TC.y,riv7$TC.x)^2
  pbais7[i]    <- cal_PBIAS(riv7$TC.y,riv7$TC.x)
  
  
  simriv15  <- read.table(file.path(cwd, "out/conc_riv15.txt"),header = T)#ng/L
  obsriv15  <- read.table(file.path(cwd, "dat/antriv15.txt")  ,header = T)
  riv15     <- merge(simriv15,obsriv15,by="date")
  rmse15[i]    <- rmse(riv15$TC.y,riv15$TC.x)
  nse15[i]   <- cal_NSE(riv15$TC.y,riv15$TC.x)
  r15[i]     <-cor(riv15$TC.y,riv15$TC.x)^2
  pbais15[i]    <- cal_PBIAS(riv15$TC.y,riv15$TC.x)
  
  simriv17  <- read.table(file.path(cwd, "out/conc_riv17.txt"),header = T)#ng/L
  obsriv17  <- read.table(file.path(cwd, "dat/antriv17.txt")  ,header = T)
  riv17     <- merge(simriv17,obsriv17,by="date")
  rmse17[i]    <- rmse(riv17$TC.y,riv17$TC.x)
  nse17[i]   <- cal_NSE(riv17$TC.y,riv17$TC.x)
  r17[i]     <-cor(riv17$TC.y,riv17$TC.x)^2
  pbais17[i]    <- cal_PBIAS(riv17$TC.y,riv17$TC.x)
  
  simriv20  <- read.table(file.path(cwd, "out/conc_riv20.txt"),header = T)#ng/L
  obsriv20  <- read.table(file.path(cwd, "dat/antriv20.txt")  ,header = T)
  riv20     <- merge(simriv20,obsriv20,by="date")
  rmse20[i]    <- rmse(riv20$TC.y,riv20$TC.x)
  nse20[i]   <- cal_NSE(riv20$TC.y,riv20$TC.x)
  r20[i]     <-cor(riv20$TC.y,riv20$TC.x)^2
  pbais20[i]    <- cal_PBIAS(riv20$TC.y,riv20$TC.x)
  
  
  simriv23  <- read.table(file.path(cwd, "out/conc_riv23.txt"),header = T)#ng/L
  obsriv23  <- read.table(file.path(cwd, "dat/antriv23.txt")  ,header = T)
  riv23     <- merge(simriv23,obsriv23,by="date")
  rmse23[i]    <- rmse(riv23$TC.y,riv23$TC.x)
  nse23[i]   <- cal_NSE(riv23$TC.y,riv23$TC.x)
  r23[i]     <-cor(riv23$TC.y,riv23$TC.x)^2
  pbais23[i]    <- cal_PBIAS(riv23$TC.y,riv23$TC.x)
                         
  # RMSE[i]=(rmse5[i]+rmse7[i]+rmse15[i]+rmse17[i]+rmse20[i]+rmse23[i])/6#
  
  rmsedate<-cbind(rmse5[i],rmse9[i],rmse7[i],rmse15[i],rmse17[i],rmse20[i],rmse23[i])
  rdate<-cbind(r5[i],r9[i],r7[i],r15[i],r17[i],r20[i],r23[i])
  nsedate<-cbind(nse5[i],nse9[i],nse7[i],nse15[i],nse17[i],nse20[i],nse23[i])
  pdate<-cbind(pbais5[i],pbais9[i],pbais7[i],pbais15[i],pbais17[i],pbais20[i],pbais23[i])
  resultdate<-cbind(rmsedate,rdate,nsedate,pdate)
  write.table(resultdate,resultfile,append = T,sep = " ",
              col.names = F,row.names = F)
}
