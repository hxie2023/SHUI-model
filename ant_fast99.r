
library(boot)
library(tcltk)
library(sensitivity)
library(Metrics)


cwd <- "/home/smq/code/1/tyh_20240406"
setwd(cwd)

SHUI <- file.path(cwd, "SHUI")
parfile <- file.path(cwd, "dat/tyh_par_ant.txt")
usefile <- file.path(cwd, "dat/tyh_antuse.txt")
resultfile<-file.path(cwd, "ant_fast99_1.txt")


RMSE <- function(V) {
  
  data1 <- readLines(parfile)   #<Parameters
  data1 <-as.data.frame(data1)
  data2 <- readLines(usefile)   #<ant use
  data2 <-as.data.frame(data2)

  RMSE   <- matrix(nrow = nrow(V))
  rmse5  <- matrix(nrow = nrow(V))
  rmse9  <- matrix(nrow = nrow(V))
  rmse7  <- matrix(nrow = nrow(V))
  rmse15 <- matrix(nrow = nrow(V))
  rmse17 <- matrix(nrow = nrow(V))
  rmse20 <- matrix(nrow = nrow(V))
  rmse23 <- matrix(nrow = nrow(V))
  
  gw_mix    <- V[,1]  #gw_mix      
  percop    <- V[,2]  #percop           
  ratem     <- V[,3]  #ratem         
  kex       <- V[,4]  #kex           
  kt        <- V[,5]  #kt        
  kdif      <- V[,6]  #kdif  
  
  DTsoil    <- V[,7]  #DTsoil
  DTwater   <- V[,8]  #DTwater
  Kpwater   <- V[,9]  #Kpwater
  Kpsoil    <- V[,10] #Kpsoil
  Kpsed     <- V[,11] #Kpsed


  for (i in 1:nrow(V)) {
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
    rmse5[i]    <- rmse(riv5$TC.y,riv5$TC.x)

    simriv9  <- read.table(file.path(cwd, "out/conc_riv9.txt"),header = T)#ng/L
    obsriv9  <- read.table(file.path(cwd, "dat/antriv9.txt")  ,header = T)
    riv9     <- merge(simriv9,obsriv9,by="date")
    rmse9[i]    <- rmse(riv9$TC.y,riv9$TC.x)
    
    simriv7  <- read.table(file.path(cwd, "out/conc_riv7.txt"),header = T)#ng/L
    obsriv7  <- read.table(file.path(cwd, "dat/antriv7.txt")  ,header = T)
    riv7     <- merge(simriv7,obsriv7,by="date")
    rmse7[i]    <- rmse(riv7$TC.y,riv7$TC.x)
    
    simriv15  <- read.table(file.path(cwd, "out/conc_riv15.txt"),header = T)#ng/L
    obsriv15  <- read.table(file.path(cwd, "dat/antriv15.txt")  ,header = T)
    riv15     <- merge(simriv15,obsriv15,by="date")
    rmse15[i]    <- rmse(riv15$TC.y,riv15$TC.x)
    
    simriv17  <- read.table(file.path(cwd, "out/conc_riv17.txt"),header = T)#ng/L
    obsriv17  <- read.table(file.path(cwd, "dat/antriv17.txt")  ,header = T)
    riv17     <- merge(simriv17,obsriv17,by="date")
    rmse17[i]    <- rmse(riv17$TC.y,riv17$TC.x)
    
    simriv20  <- read.table(file.path(cwd, "out/conc_riv20.txt"),header = T)#ng/L
    obsriv20  <- read.table(file.path(cwd, "dat/antriv20.txt")  ,header = T)
    riv20     <- merge(simriv20,obsriv20,by="date")
    rmse20[i]    <- rmse(riv20$TC.y,riv20$TC.x)
    
    simriv23  <- read.table(file.path(cwd, "out/conc_riv23.txt"),header = T)#ng/L
    obsriv23  <- read.table(file.path(cwd, "dat/antriv23.txt")  ,header = T)
    riv23     <- merge(simriv23,obsriv23,by="date")
    rmse23[i]    <- rmse(riv23$TC.y,riv23$TC.x)
    
    RMSE[i]=(rmse5[i]+rmse7[i]+rmse15[i]+rmse17[i]+rmse20[i]+rmse23[i])/6#
    
    resultdate<-cbind(rmse5[i],rmse9[i],rmse7[i],rmse15[i],rmse17[i],rmse20[i],rmse23[i],RMSE[i],V[i,])
    write.table(resultdate,resultfile,append = T,sep = " ",
                col.names = F,row.names = F)
  }
  return(RMSE)
}

#sensibility function                                                                
set.seed(1113)
x <- fast99(model = RMSE, factors = 11, n = 200,
            q = "qunif", q.arg = list(list(min = 0.1,      max = 1),       #1gw_mix     
                                      list(min = 0.1,      max = 1),       #2percop    
                                      list(min = 0.01,     max = 0.5),     #3ratem     
                                      list(min = 0.0005,   max = 0.005),   #4kex       
                                      list(min = 1.01,     max = 1.10),    #5kt        
                                      list(min = 0.0005,   max = 0.005),   #6kdif
                                      list(min = 2,        max = 86.6),    #7DTsoil
                                      list(min = 4.7,      max = 7.7),     #8DTwater
                                      list(min = 0.0000142,max =0.133398), #9Kpwater
                                      list(min = 10,       max = 1941),    #10Kpsoil
                                      list(min = 0.000174, max = 0.022933) #11Kpsed
                                      ))        

# View(x$y)
a<-as.data.frame(x$y) #指标结果
b<-as.data.frame(x$X) #参数取值



sink(file.path(cwd, "fast99.txt"))
print(x)
sink()

write.table(a,file.path(cwd, "par.txt"),row.names = T)
write.table(b,file.path(cwd, "rmes.txt"),row.names = T)

print(x)
p<-plot(x)
save(p,"/home/smq/code/1/tyh_20240406/p.jpg")