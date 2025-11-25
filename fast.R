rm(list=ls())
library(this.path)
cwd=dirname(this.path())
setwd(cwd)

library(boot)
library(tcltk)
library(sensitivity)

num_obs <- 139

NSE <- function(V) {
  data <- readLines("/home/xiehui/Codes/shui_R/dat/parameter.txt")   #<Parameters
  data<-as.data.frame(data)
  # V[is.na(V)]<-0#
  RES_riverflux <- matrix(nrow = num_obs, ncol = nrow(V)) 
  NSE <- matrix(nrow = nrow(V))
  NSE1 <- matrix(nrow = nrow(V))
  NSE2 <- matrix(nrow = nrow(V))
  L9   <- V[,1]   #<sorocc
  L10  <- V[,2]   #<gwdelay
  L11  <- V[,3]   #<gwrc
  L13  <- V[,4]   #<sopetdis
  L17  <- V[,5]   #<petcoef
  L33  <- V[,6]   #<macro
  L34  <- V[,7]   #<surocc1
  L35  <- V[,8]   #<surocc2
  L36  <- V[,9]   #<surocc3
  L37  <- V[,10]  #<adhc
  L38  <- V[,11]  #<b
  L39  <- V[,12]  #<bf
  
  
  for (i in 1:nrow(V)) {
    data[9,]  <-paste("sorocc",   L9[i],  sep=" ")
    data[10,] <-paste("gwdelay",  L10[i], sep=" ")
    data[11,] <-paste("gwrc",     L11[i], sep=" ")
    data[13,] <-paste("sopetdis", L13[i], sep=" ")
    data[17,] <-paste("petcoef",  paste0(rep(L17[i], 5), collapse=" "), sep=" ")
    data[33,] <-paste("macro",    paste0(rep(L33[i], 7), collapse=" "), sep=" ")
    data[34,] <-paste("surocc1",  paste0(rep(L34[i], 7), collapse=" "), sep=" ")
    data[35,] <-paste("surocc2",  paste0(rep(L35[i], 7), collapse=" "), sep=" ")
    data[36,] <-paste("surocc3",  paste0(rep(L36[i], 7), collapse=" "), sep=" ")
    data[37,] <-paste("adhc",     paste0(rep(L37[i], 7), collapse=" "), sep=" ")
    data[38,] <-paste("b",        paste0(rep(L38[i], 7), collapse=" "), sep=" ")
    data[39,] <-paste("bf",       paste0(rep(L39[i], 7), collapse=" "), sep=" ")
    
    write.table(data,"/home/xiehui/Codes/shui_R/dat/parameter.txt",
                append = FALSE,row.names=F,col.names=F,quote=F)
    
    system2("/home/xiehui/Codes/shui_R/SHUI", args = c("-n 96"))
    outsim1<-read.table("/home/xiehui/Codes/shui_R/out/rch9.txt")
    outsim2<-read.table("/home/xiehui/Codes/shui_R/out/rch20.txt")
    #RES_9[,i]<-outsim1$V2
    #RES_20[,i]<-outsim2$V2
    #View(RES_riverflux)
    sim1<-outsim1$V2[227:365]
    sim2<-outsim2$V2[229:365]
    readobs<-read.table("/home/xiehui/Codes/shui_R/dat/obs.txt")
    obs1<-readobs$V2[1:139]
    obs2<-readobs$V2[3:139]
    stopifnot(length(obs1) == length(sim1))
    NSE1[i]<-1-((sum((obs1 - sim1)^2))/(sum((obs1 - mean(obs1))^2)))
    stopifnot(length(obs2) == length(sim2))
    NSE2[i]<-1-((sum((obs2 - sim2)^2))/(sum((obs2 - mean(obs2))^2)))
    
    NSE[i]=(NSE1[i]+NSE2[i])/2#

  }
  return(NSE)
}

#sensibility function                                                                
set.seed(1111)
x <- fast99(model = NSE, factors = 12, n = 400,
            q = "qunif", q.arg = list(list(min = 0.4,   max = 0.999),   #<sorocc  
                                      list(min = 0,     max = 500),     #<gwdelay
                                      list(min = 0.1,   max = 0.999),   #<gwrc
                                      list(min = 1,     max = 10),      #<sopetdis
                                      list(min = 0.1,   max = 0.99),    #<petcoef
                                      list(min = 0,     max = 0.8),     #<macro
                                      list(min = 0.001, max = 0.99),    #<surocc1
                                      list(min = 0.001, max = 0.99),    #<surocc2
                                      list(min = 0.001, max = 0.99),    #<surocc3
                                      list(min = 0.1,   max = 0.9),     #<adhc
                                      list(min = 0.01,  max = 0.99),    #<b
                                      list(min = 0.01,  max = 0.99)))   #<bf


# View(x$y)
a<-as.data.frame(x$y)
b<-as.data.frame(x$X)

write.table(a,"/home/xiehui/Codes/shui_R/1111.txt",row.names = T)
write.table(b,"/home/xiehui/Codes/shui_R/1111var.txt",row.names = T)

# print(x)

p<-plot(x)
save(p,"/home/xiehui/Codes/shui_R/p.jpg")