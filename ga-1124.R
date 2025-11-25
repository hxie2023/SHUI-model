rm(list=ls())
library("genalg")

# setwd("/home/smq/code/shui_0layer")
# library(genalg)
cwd <- "/Users/alwiee/Library/Mobile Documents/com~apple~CloudDocs/Coding/SHUI/shui_ga_tyh_1124"
setwd(cwd)
parfile <- readLines(file.path(cwd, "dat/parameter.txt"))
parfile <- as.data.frame(parfile)
SHUI <- file.path(cwd, "SHUI")
resultfile<-file.path(cwd, "NSE1124_ga.txt")
if(file.exists(resultfile))
{
  file.remove(resultfile)
  file.create(resultfile)
}else
{
  file.create(resultfile)
}


sim9_file  <- file.path(cwd, "out/rch9.txt")
sim20_file <- file.path(cwd, "out/rch20.txt")
obs_file <- file.path(cwd, "dat/obs2018_2019.txt")
obs <- read.table(obs_file)
obs9 <- obs$V2[1:443]  #2018-09-01~2019-07-14~2019-08-28~2019-12-31
obs20 <- obs$V3[1:317] #2018-09-01~2019-07-14

getAdjust <- function(x)
{
  sorocc   <- x[1]
  gwdelay  <- x[2]
  gwrc     <- x[3]
  sopetdis <- x[4]
  petcoef  <- x[5]
  macro    <- x[6]
  sorocc1  <- x[7]
  sorocc2  <- x[8]
  sorocc3  <- x[9]
  adhc     <- x[10]
  b        <- x[11]
  bf       <- x[12]
  
  par_sample <- data.frame(sorocc,gwdelay,gwrc,sopetdis,petcoef,macro,
                           sorocc1,sorocc2,sorocc3,adhc,b,bf)
  # print("start")
  print(par_sample)
  
  parfile[9, ] <- paste("sorocc",    par_sample[1, "sorocc"],  sep = " ")
  parfile[10, ] <- paste("gwdelay",  par_sample[1, "gwdelay"],  sep = " ")
  parfile[11, ] <- paste("gwrc",     par_sample[1, "gwrc"],  sep = " ")
  parfile[13, ] <- paste("sopetdis", par_sample[1, "sopetdis"],  sep = " ")
  parfile[17, ] <- paste("petcoef",  paste0(rep(par_sample[1, "petcoef"],  5),collapse=" "), sep=" ")
  parfile[34, ] <- paste("macro",    paste0(rep(par_sample[1, "macro"],  7),collapse=" "), sep=" ") 
  parfile[35, ] <- paste("sorocc1",  paste0(rep(par_sample[1, "sorocc1"],  7),collapse=" "), sep=" ")
  parfile[36, ] <- paste("sorocc2",  paste0(rep(par_sample[1, "sorocc2"],  7),collapse=" "), sep=" ") 
  parfile[37, ] <- paste("sorocc3",  paste0(rep(par_sample[1, "sorocc3"],  7),collapse=" "), sep=" ") 
  parfile[38, ] <- paste("adhc",     paste0(rep(par_sample[1, "adhc"], 7),collapse=" "), sep=" ")
  parfile[39, ] <- paste("b",        paste0(rep(par_sample[1, "b"], 7),collapse=" "), sep=" ")
  parfile[40, ] <- paste("bf",       paste0(rep(par_sample[1, "bf"], 7),collapse=" "), sep=" ")
  
  # return ((x[1]-7)^2 + (x[2]-77)^2 +(x[3]-777)^2 +(x[4]-7777)^2 +(x[5]-77777)^2)
  write.table(parfile, file.path(cwd, "dat/parameter.txt"),
              append = FALSE, row.names = F, col.names = F, quote = F )
  system2(SHUI, args = c("-n 10"))
  sim9_data <- read.table(sim9_file)
  # sim20_data <- read.table(sim20_file)
  sim9 <- c(sim9_data$V2[244:560], sim9_data$V2[605:730]) #317+126=443
  # sim20 <- sim20_data$V2[244:560] #317
  stopifnot(length(obs9) == length(sim9))
  NSE9 <- 1-((sum((obs9 - sim9)^2))/(sum((obs9 - mean(obs9))^2))) #不做
  # stopifnot(length(obs20) == length(sim20))
  # NSE20 <- 1-((sum((obs20 - sim20)^2))/(sum((obs20 - mean(obs20))^2)))#只看这个
  # NSE <- 1-(NSE9+NSE20)/2
  # NSE <- 1-NSE20
  NSE <- 1-NSE9
  # 
  NSE_ALL <- cbind(par_sample,NSE9,NSE)
  # write.csv(NSE_ALL, file.path(cwd,"out/NSE1121.txt",row.names = FALSE)
  write.table(NSE_ALL,resultfile,append = T,sep = " ",
              col.names = F,row.names = F)
  print(NSE)
  # print(NSE9)
  # print(NSE_ALL)
  # print("over")
  return(NSE)
}



# monitor<-function(rbga0)
# 自定义的监视函数
monitor <- function(obj) 
{
  # 收敛标准参数
  tolerance <- 1e-3  # 定义一个适应度变化的小阈值
  nStableGenerations <- 20  # 连续多少代内适应度变化小于阈值时认为已收敛
  
  # 初始化或更新历史最佳适应度
  if (!exists("bestFitnessHistory")) 
  {
    assign("bestFitnessHistory", numeric(nStableGenerations), envir = .GlobalEnv)
  }
  
  # 更新历史最佳适应度记录
  history <- get("bestFitnessHistory", envir = .GlobalEnv)
  history <- c(min(obj$evaluations), history[1:(nStableGenerations - 1)])
  assign("bestFitnessHistory", history, envir = .GlobalEnv)
  print(min(obj$evaluations))
  
  # 检查是否收敛
  if (max(history) - min(history) < tolerance) 
  {
    print(max(history) - min(history))
    return(TRUE)
  }
  print(max(history) - min(history))
  return(FALSE)
}   

rbgaObj<-rbga(stringMin = c(0.1, 0,  0.1, 1, 0.1,0.1,
                            0.001, 0.001, 0.001, 0.01, 0.01, 0.01 ), 
              stringMax = c(0.99,500,0.99,10,0.9,0.8,
                            0.99,0.99,0.99,0.99,0.99,0.99), popSize = 100, 
              iters = 100, mutationChance = 0.01, monitorFunc = monitor, 
              evalFunc = getAdjust, verbose = TRUE)

# pdf("/home/smq/code/shui_ga_1122/p1.pdf")
# par(mar=c(5,3,5,3))
# plot(rbgaObj)
# dev.off()

# pdf("/home/smq/code/shui_ga_1122/p1.pdf")
# par(mar=c(5,3,5,3))
# plot(rbgaObj,type="vars")
# dev.off()

# pdf("/home/smq/code/shui_ga_1122/p1.pdf")
# par(mar=c(5,3,5,3))
# plot(rbgaObj,type='hist')
# dev.off()




# dev.off()
# str(rbgaObj)




# View(rbgaObj$population)
# str(rbgaObj$evaluations[])
# rbgaObj$best
