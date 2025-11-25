#Evaluate 2
#Uniformly distribution, 500 samplings 

cwd <- "/home/xiehui/R/random/shui_0layer"
setwd(cwd)

num_sample <- 1000
sorocc  <- runif(num_sample, min = 0.1, max = 0.99)
gwdelay <- runif(num_sample, min = 0, max = 500)
gwrc  <- runif(num_sample, min = 0.1, max = 0.99)
sopetdis <- runif(num_sample, min = 1, max = 10)
petcoef  <- runif(num_sample, min = 0.1, max = 0.9)
macro <- runif(num_sample, min = 0.1, max = 0.8)
sorocc1  <- runif(num_sample, min = 0.001, max = 0.99)
sorocc2 <- runif(num_sample, min = 0.001, max = 0.99)
sorocc3 <- runif(num_sample, min = 0.001, max = 0.99)
adhc <- runif(num_sample, min = 0.01, max = 0.2)
b  <- runif(num_sample, min = 0.01, max = 0.2)
bf <- runif(num_sample, min = 0.01, max = 0.2)

par_sample <- data.frame(sorocc,gwdelay,gwrc,sopetdis,petcoef,macro,sorocc1,sorocc2,sorocc3,adhc,b,bf)
write.table(par_sample, file.path(cwd, "par_sample_1121.txt"))
# head(par_sample)

parfile <- readLines(file.path(cwd, "dat/parameter.txt"))
parfile <- as.data.frame(parfile)
SHUI <- file.path(cwd, "SHUI")

sim9_file  <- file.path(cwd, "out/rch9.txt")
sim20_file <- file.path(cwd, "out/rch20.txt")
obs_file <- file.path(cwd, "dat/obs2018_2019.txt")
obs <- read.table(obs_file)
obs9 <- obs$V2[1:443]  #2018-09-01~2019-07-14~2019-08-28~2019-12-31
obs20 <- obs$V2[1:317] #2018-09-01~2019-07-14

NSE9 <- numeric(num_sample) 
NSE20 <- numeric(num_sample) 
NSE <- numeric(num_sample) 

for (i in 1:num_sample) {
  print(i)
  parfile[9, ] <- paste("sorocc",    par_sample[i, "sorocc"],  sep = " ")
  parfile[10, ] <- paste("gwdelay",  par_sample[i, "gwdelay"],  sep = " ")
  parfile[11, ] <- paste("gwrc",     par_sample[i, "gwrc"],  sep = " ")
  parfile[13, ] <- paste("sopetdis", par_sample[i, "sopetdis"],  sep = " ")
  parfile[17, ] <- paste("petcoef",  paste0(rep(par_sample[i, "petcoef"],  5),collapse=" "), sep=" ")
  parfile[34, ] <- paste("macro",    paste0(rep(par_sample[i, "macro"],  7),collapse=" "), sep=" ") 
  parfile[35, ] <- paste("sorocc1",  paste0(rep(par_sample[i, "sorocc1"],  7),collapse=" "), sep=" ")
  parfile[36, ] <- paste("sorocc2",  paste0(rep(par_sample[i, "sorocc2"],  7),collapse=" "), sep=" ") 
  parfile[37, ] <- paste("sorocc3",  paste0(rep(par_sample[i, "sorocc3"],  7),collapse=" "), sep=" ") 
  parfile[38, ] <- paste("adhc",     paste0(rep(par_sample[i, "adhc"], 7),collapse=" "), sep=" ")
  parfile[39, ] <- paste("b",        paste0(rep(par_sample[i, "b"], 7),collapse=" "), sep=" ")
  parfile[40, ] <- paste("bf",       paste0(rep(par_sample[i, "bf"], 7),collapse=" "), sep=" ")
  write.table(parfile, file.path(cwd, "dat/parameter.txt"),
              append = FALSE, row.names = F, col.names = F, quote = F )
  system2(SHUI, args = c("-n 96"))
  sim9_data <- read.table(sim9_file)
  sim20_data <- read.table(sim20_file)
  sim9 <- c(sim9_data$V2[244:560], sim9_data$V2[605:730]) #317+126=443
  sim20 <- sim20_data$V2[244:560] #317
  stopifnot(length(obs9) == length(sim9))
  NSE9[i] <- 1-((sum((obs9 - sim9)^2))/(sum((obs9 - mean(obs9))^2)))
  stopifnot(length(obs20) == length(sim20))
  NSE20[i] <- 1-((sum((obs20 - sim20)^2))/(sum((obs20 - mean(obs20))^2)))
  NSE[i] <- (NSE9[i]+NSE20[i])/2
  print(NSE[i])
  print(NSE9[i])
  print(NSE20[i])
  
}

NSE_ALL <- cbind(NSE,NSE9,NSE20)
write.csv(NSE_ALL, file.path(cwd, "out/NSE_1121.txt"), row.names = FALSE)

