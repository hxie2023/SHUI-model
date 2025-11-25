#GA, tyh, for riv20, 1130
#2018-08-17 ~ 2018-12-31

rm(list=ls())
library("genalg")

# setwd("/home/smq/code/shui_0layer")
# library(genalg)
cwd <- "/Users/alwiee/Library/Mobile Documents/com~apple~CloudDocs/Coding/SHUI/shui_ga_tyh_1124"
setwd(cwd)
parfile <- readLines(file.path(cwd, "dat/parameter.txt"))
parfile <- as.data.frame(parfile)
SHUI <- file.path(cwd, "SHUI")
resultfile<-file.path(cwd, "ga_1130_res.txt")
if(file.exists(resultfile))
{
  file.remove(resultfile)
  file.create(resultfile)
}else
{
  file.create(resultfile)
}

sim20_file <- file.path(cwd, "out/rch20.txt")
obs_file <- file.path(cwd, "dat/obs_2018.txt")
obs <- read.table(obs_file)
obs20 <- obs$V3[3:139] #2018-08-17~2018-12-31


#Define evaluation statitics
cal_PBIAS <- function(observed, predicted) {
  sum_observed <- sum(observed)
  sum_diff <- sum(observed - predicted)
  PBIAS <- (sum_diff / sum_observed) * 100
  return(PBIAS)
  }
cal_NSE <- function(observed, predicted) {
  mean_observed <- mean(observed)
  numerator <- sum((observed - predicted)^2)
  denominator <- sum((observed - mean_observed)^2)
  NSE <- 1 - (numerator / denominator)
  return(NSE)
}
#Kling-Gupta Efficiency
cal_KGE <- function(simulated, observed) {
  # 确保数据长度相同
  if(length(simulated) != length(observed)) {
    stop("The length of simulated and observed data must be the same.")
  }
  # 计算相关系数
  r <- cor(simulated, observed)
  # 计算偏差比
  beta <- mean(simulated) / mean(observed)
  # 计算波动性比
  alpha <- sd(simulated) / sd(observed)
  # 计算KGE
  KGE <- 1 - sqrt((r - 1)^2 + (beta - 1)^2 + (alpha - 1)^2)
  return(KGE)
}

cal_R2 <- function(simulated, observed) {
  r <- cor(simulated, observed)
  R2 <- r^2
  return(R2)
}

run_shui <- function(x)
{
  surocc   <-  x[1]
  sorocc   <-  x[2]
  gwdelay  <-  x[3]
  gwrc     <-  x[4]
  sopetdis <-  x[5]

  petcoef1  <- x[6]
  petcoef2  <- x[7]
  petcoef3  <- x[8]
  petcoef4  <- x[9]

  macro    <-  x[10]
  sorocc1  <-  x[11]
  sorocc2  <-  x[12]
  sorocc3  <-  x[13]
  adhc     <-  x[14]
  b        <-  x[15]
  bf       <-  x[16]
  
  par_sample <- data.frame(surocc,sorocc,gwdelay,gwrc,sopetdis,
                           petcoef1,petcoef2,petcoef3,petcoef4,
                           macro,sorocc1,sorocc2,sorocc3,adhc,b,bf)
  print(par_sample)
  
  parfile[8, ] <- paste("surocc",    par_sample[1, "surocc"],  sep = " ")
  parfile[9, ] <- paste("sorocc",    par_sample[1, "sorocc"],  sep = " ")
  parfile[10, ] <- paste("gwdelay",  par_sample[1, "gwdelay"],  sep = " ")
  parfile[11, ] <- paste("gwrc",     par_sample[1, "gwrc"],  sep = " ")
  parfile[13, ] <- paste("sopetdis", par_sample[1, "sopetdis"],  sep = " ")
  parfile[17, ] <- paste("petcoef",  par_sample[1, "petcoef1"],
                                     par_sample[1, "petcoef2"],
                                     par_sample[1, "petcoef3"],
                                     par_sample[1, "petcoef4"], "0.5", sep=" ")

  parfile[34, ] <- paste("macro",    paste0(rep(par_sample[1, "macro"],  7),collapse=" "), sep=" ") 
  parfile[35, ] <- paste("sorocc1",  paste0(rep(par_sample[1, "sorocc1"],  7),collapse=" "), sep=" ")
  parfile[36, ] <- paste("sorocc2",  paste0(rep(par_sample[1, "sorocc2"],  7),collapse=" "), sep=" ") 
  parfile[37, ] <- paste("sorocc3",  paste0(rep(par_sample[1, "sorocc3"],  7),collapse=" "), sep=" ") 
  parfile[38, ] <- paste("adhc",     paste0(rep(par_sample[1, "adhc"], 7),collapse=" "), sep=" ")
  parfile[39, ] <- paste("b",        paste0(rep(par_sample[1, "b"], 7),collapse=" "), sep=" ")
  parfile[40, ] <- paste("bf",       paste0(rep(par_sample[1, "bf"], 7),collapse=" "), sep=" ")
  
  write.table(parfile, file.path(cwd, "dat/parameter.txt"),
              append = FALSE, row.names = F, col.names = F, quote = F )
  system2(SHUI, args = c("-n 50"))
  sim20_data <- read.table(sim20_file)
  sim20 <- sim20_data$V2[229:365]   
  stopifnot(length(obs20) == length(sim20))

  NSE20 <- cal_NSE(obs20,sim20)
  PBIAS20 <- cal_PBIAS(obs20,sim20)
  R2_20 <- cal_R2(obs20,sim20)
  KGE20 <- cal_KGE(obs20,sim20)

  fit20 <- 1-NSE20

  res <- cbind(par_sample,KGE20,R2_20,PBIAS20,NSE20)
  write.table(res,resultfile,append = T,sep = " ",
              col.names = F,row.names = F)
  print(paste("NSE20=", NSE20))
  return(fit20)
}

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

lower_bounds <- c(0.01, 0.01, 1, 0.1, 1,
                0.1, 0.1, 0.1, 0.1, 
                0.01, 0.001,0.001,0.001,0.001,0.001,0.001)
upper_bounds <- c(0.99,0.99, 500,0.99,10,
                  0.9,0.9,0.9,0.9,
                  0.8,0.99,0.99,0.99,0.99,0.99,0.99)

rbgaObj<-rbga(stringMin = lower_bounds, 
              stringMax = upper_bounds, 
              popSize = 100, 
              iters = 100, 
              mutationChance = 0.01,
              elitism = 20,
              monitorFunc = monitor, 
              evalFunc = run_shui, 
              verbose = TRUE)

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
