#Only for calibration period
#Evaluate one parameter group
#NSE, RMSE, etc.
#Also plot for hydrograph comparison
rm(list = ls())

cwd <- "/home/xiehui/R/random/shui_0layer"
setwd(cwd)

par_file <- file.path(cwd, "par_cal.txt")
par_data <- read.csv(par_file, sep = "\t", header = TRUE)
par_data <- par_data[1,]

parfile <- readLines(file.path(cwd, "dat/parameter.txt"))
parfile <- as.data.frame(parfile)
SHUI <- file.path(cwd, "SHUI")

sim9_file  <- file.path(cwd, "out/rch9.txt")
sim20_file <- file.path(cwd, "out/rch20.txt")
obs_file <- file.path(cwd, "dat/obs_2018_2021.txt")
obs <- read.table(obs_file)
#Calibration period
obs9 <- obs$V2[1:443]  #2018-09-01~2019-07-14~2019-08-28~2019-12-31
obs20 <- obs$V3[1:317] #2018-09-01~2019-07-14

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

NSE9 <- numeric(10) 
NSE20 <- numeric(10) 
NSE <- numeric(10) 

#Writer parameter.txt
parfile[9, ]  <- paste("sorocc",   par_data$sorocc,    sep = " ")
parfile[10, ] <- paste("gwdelay",  par_data$gwdelay,   sep = " ")
parfile[11, ] <- paste("gwrc",     par_data$gwrc,      sep = " ")
parfile[13, ] <- paste("sopetdis", par_data$sopetdis,  sep = " ")
parfile[17, ] <- paste("petcoef",  paste0(rep(par_data$petcoef, 5),collapse=" "), sep=" ")
parfile[34, ] <- paste("macro",    paste0(rep(par_data$macro,   7),collapse=" "), sep=" ") 
parfile[35, ] <- paste("sorc1",    paste0(rep(par_data$sorc1,   7),collapse=" "), sep=" ")
parfile[36, ] <- paste("sorc2",    paste0(rep(par_data$sorc2,   7),collapse=" "), sep=" ") 
parfile[37, ] <- paste("sorc3",    paste0(rep(par_data$sorc3,   7),collapse=" "), sep=" ") 
parfile[38, ] <- paste("adhc",     paste0(rep(par_data$adhc,    7),collapse=" "), sep=" ")
parfile[39, ] <- paste("b",        paste0(rep(par_data$b,       7),collapse=" "), sep=" ")
parfile[40, ] <- paste("bf",       paste0(rep(par_data$bf,      7),collapse=" "), sep=" ")
write.table(parfile, file.path(cwd, "dat/parameter.txt"),
            append = FALSE, row.names = F, col.names = F, quote = F )

system2(SHUI, args = c("-n 100"))
sim9_data <- read.table(sim9_file)
sim20_data <- read.table(sim20_file)
sim9 <- c(sim9_data$V2[244:560], sim9_data$V2[605:730]) #317+126=443
sim20 <- sim20_data$V2[244:560] #317
stopifnot(length(obs9) == length(sim9))
# NSE9 <- 1-((sum((obs9 - sim9)^2))/(sum((obs9 - mean(obs9))^2)))
NSE9 <- cal_NSE(obs9, sim9)
stopifnot(length(obs20) == length(sim20))
# NSE20 <- 1-((sum((obs20 - sim20)^2))/(sum((obs20 - mean(obs20))^2)))
NSE20 <- cal_NSE(obs20, sim20)
NSE <- (NSE9 + NSE20)/2
PBIAS9 <- cal_PBIAS(obs9, sim9)
PBIAS20 <- cal_PBIAS(obs20, sim20)

print("Calibration:")
# print(NSE)
print(NSE9)
print(NSE20)
print(PBIAS9)
print(PBIAS20)

library(lubridate)
obs_file_plot <- file.path(cwd, "dat/obs_2018_2021_plot.txt") #Null values were NAs
data <- read.csv(obs_file_plot, , sep = "\t", header = FALSE)
data <- data[1:443,]
colnames(data) <- c("Date", "obs_9", "obs_20")
data$Date <- as.Date(data$Date)
all_dates <- seq(from = min(data$Date), to = max(data$Date), by = "day")
complete_data <- data.frame(Date = all_dates)
complete_data <- merge(complete_data, data, by = "Date", all.x = TRUE)
complete_data$sim_9 <- sim9_data$V2[244:730]
complete_data$sim_20 <- sim20_data$V2[244:730]
# rainfall <- read.csv("dat/rainfall_plot.txt", sep = "\t", header = FALSE)
# complete_data$rainfall <- rainfall$V4[1:487]

library(ggplot2)
p_9 <- ggplot(complete_data, aes(x = Date)) +
  # 添加散点图层，指定y轴为obs_9，设置点的形状(shape)和颜色(color)
  geom_point(aes(y = obs_9), shape = 1, color = "blue") +
  # 添加折线图层，指定y轴为sim_9，设置线的类型和颜色
  geom_line(aes(y = sim_9), linetype = "solid", color = "red") +
  # 选项性地添加图标题、x轴和y轴的标签
  ggtitle("Tong River") +
  scale_x_date(date_breaks = "6 month", date_labels = "%Y-%m") +
  xlab("Date") +
  ylab("Flow (m3/s)")
p_9
# p_9 <- p_9 + geom_bar(aes(y = rainfall / 20), stat = "identity", fill = "gray",alpha = 0.5) +
#              scale_y_continuous(limits = c(0, 2.5),  # 设置左侧Y轴的范围
#              sec.axis = sec_axis(~ . * 20, name = "Rainfall"))  # 设置右侧Y轴的转换
# 
# p_9
p_20 <- ggplot(complete_data, aes(x = Date)) +
  # 添加散点图层，指定y轴为obs_9，设置点的形状(shape)和颜色(color)
  geom_point(aes(y = obs_20), shape = 1, color = "blue") +
  # 添加折线图层，指定y轴为sim_9，设置线的类型和颜色
  geom_line(aes(y = sim_20), linetype = "solid", color = "red") +
  # 选项性地添加图标题、x轴和y轴的标签
  ggtitle("Yang River") +
  scale_x_date(date_breaks = "6 month", date_labels = "%Y-%m") +
  xlab("Date") +
  ylab("Flow (m3/s)")
p_20


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






