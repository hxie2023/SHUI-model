#For 1111var.txt, multiple running
rm(list = ls())

cwd <- "/Users/alwiee/Library/Mobile Documents/com~apple~CloudDocs/Coding/FORTRAN_tyh/shui_0layer"

nse_file <- file.path(cwd, "out/1111.txt")
par_file <- file.path(cwd, "out/1111var.txt")
nse_data <- read.table(nse_file, header = TRUE)
par_data <- read.table(par_file, header = TRUE)
nse_par_data <- merge(nse_data, par_data, by="row.names")
nse_par_data <- nse_par_data[order(-nse_par_data$x.y), ]

#Select parameter groups with high NSE values
select <- c(25,35,14)
nse_par_select <- nse_par_data[select,]

parfile <- readLines(file.path(cwd, "dat/parameter.txt"))
parfile <- as.data.frame(parfile)
SHUI <- file.path(cwd, "SHUI")

sim9_file  <- file.path(cwd, "out/rch9.txt")
sim20_file <- file.path(cwd, "out/rch20.txt")
obs_file <- file.path(cwd, "dat/obs2018_2019.txt")
obs <- read.table(obs_file)
obs9 <- obs$V2[1:443]  #2018-09-01~2019-07-14~2019-08-28~2019-12-31
obs20 <- obs$V2[1:317] #2018-09-01~2019-07-14

NSE9 <- numeric(10) 
NSE20 <- numeric(10) 
NSE <- numeric(10) 

for (i in 1:3) {
  print(i)
  parfile[9, ] <- paste("sorocc",    nse_par_select[i, "X1"],  sep = " ")
  parfile[10, ] <- paste("gwdelay",  nse_par_select[i, "X2"],  sep = " ")
  parfile[11, ] <- paste("gwrc",     nse_par_select[i, "X3"],  sep = " ")
  parfile[13, ] <- paste("sopetdis", nse_par_select[i, "X4"],  sep = " ")
  parfile[17, ] <- paste("petcoef", paste0(rep(nse_par_select[i, "X5"],  5),collapse=" "), sep=" ")
  parfile[34, ] <- paste("macro",   paste0(rep(nse_par_select[i, "X7"],  7),collapse=" "), sep=" ") 
  parfile[35, ] <- paste("surocc1", paste0(rep(nse_par_select[i, "X8"],  7),collapse=" "), sep=" ")
  parfile[36, ] <- paste("surocc2", paste0(rep(nse_par_select[i, "X9"],  7),collapse=" "), sep=" ") 
  parfile[37, ] <- paste("surocc3", paste0(rep(nse_par_select[i, "X10"],  7),collapse=" "), sep=" ") 
  parfile[38, ] <- paste("adhc",    paste0(rep(nse_par_select[i, "X11"], 7),collapse=" "), sep=" ")
  parfile[39, ] <- paste("b",       paste0(rep(nse_par_select[i, "X12"], 7),collapse=" "), sep=" ")
  # parfile[40, ] <- paste("bf",      paste0(rep(nse_par_100[i, "X11"], 7),collapse=" "), sep=" ")
  write.table(parfile, file.path(cwd, "dat/parameter.txt"),
              append = FALSE, row.names = F, col.names = F, quote = F )
  system2(SHUI, args = c("-n 100"))
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

# NSE_ALL <- cbind(NSE,NSE9,NSE20)
# write.csv(NSE_ALL, "/home/xiehui/R/shui_0layer_R/out/NSE_1111_501_700.txt", row.names = FALSE)

# system2(SHUI, args = c("-n 100"))
# sim9_data <- read.table(sim9_file)
# sim20_data <- read.table(sim20_file)
# sim9 <- c(sim9_data$V2[244:560], sim9_data$V2[605:730]) #317+126=443
# sim20 <- sim20_data$V2[244:560] #317
# stopifnot(length(obs9) == length(sim9))
# NSE9 <- 1-((sum((obs9 - sim9)^2))/(sum((obs9 - mean(obs9))^2)))
# stopifnot(length(obs20) == length(sim20))
# NSE20 <- 1-((sum((obs20 - sim20)^2))/(sum((obs20 - mean(obs20))^2)))
# NSE <- (NSE9+NSE20)/2

# print(NSE9)
# print(NSE20)
# print(NSE)

# df <- data.frame(time = 1:443, obs9 = obs9, sim9 = sim9)
# p <- ggplot(df, aes(x = time)) + 
#      geom_line(aes(y = obs9, colour = "Observed")) + 
#      geom_line(aes(y = sim9, colour = "Simulated")) +
#      labs(colour = "Legend Title") +
#      theme_minimal()




