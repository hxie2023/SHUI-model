#ga_1130 results plot
#Only for 2018 yr
rm(list = ls())

cwd <- "/Users/alwiee/Library/Mobile Documents/com~apple~CloudDocs/Coding/SHUI/shui_ga_tyh_1124"
setwd(cwd)

par_file <- file.path(cwd, "par_cal_ga_1130.txt")
par_data <- read.csv(par_file, sep = "\t", header = TRUE)
par_data <- par_data[1,]

parfile <- readLines(file.path(cwd, "dat/parameter.txt"))
parfile <- as.data.frame(parfile)
SHUI <- file.path(cwd, "SHUI")

sim9_file  <- file.path(cwd, "out/rch9.txt")
sim20_file <- file.path(cwd, "out/rch20.txt")

# obs_file <- file.path(cwd, "dat/obs_2018_2021.txt")
obs_file <- file.path(cwd, "dat/obs_2018.txt")
obs <- read.table(obs_file)
#-------------------------------------------------------------------------
#Calibration period
# obs9 <- obs$V2[1:443]  #2018-09-01~2019-07-14~2019-08-28~2019-12-31
# obs20 <- obs$V3[1:317] #2018-09-01~2019-07-14
obs9 <- obs$V2[1:139]
obs20 <- c(obs$V3[3:50],obs$V3[115:139])  #2018-08-17~2018-12-31,中间有缺失值,共73个有效值
#Validation
# obs9_v <- obs$V2[444:1105]
# obs20_v <- obs$V3[871:1105]
#-------------------------------------------------------------------------
#Define evaluation statitics
cal_PBIAS <- function(observed, predicted) {
  sum_observed <- sum(observed)
  sum_diff <- sum(observed-predicted)
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

parfile[8, ] <- paste("surocc", par_data$surocc,  sep = " ")
parfile[9, ] <- paste("sorocc", par_data$sorocc,  sep = " ")
parfile[10, ] <- paste("gwdelay", par_data$gwdelay,  sep = " ")
parfile[11, ] <- paste("gwrc",     par_data$gwrc,  sep = " ")
parfile[13, ] <- paste("sopetdis", par_data$sopetdis,  sep = " ")
parfile[17, ] <- paste("petcoef",  par_data$pet_forest,
                       par_data$pet_dry,
                       par_data$pet_urb,
                       par_data$pet_paddy, "0.5", sep=" ")

parfile[34, ] <- paste("macro",    paste0(rep(par_data$macro, 7),collapse=" "), sep=" ") 
parfile[35, ] <- paste("sorocc1",  paste0(rep(par_data$rc1, 7),collapse=" "), sep=" ")
parfile[36, ] <- paste("sorocc2",  paste0(rep(par_data$rc2, 7),collapse=" "), sep=" ") 
parfile[37, ] <- paste("sorocc3",  paste0(rep(par_data$rc3, 7),collapse=" "), sep=" ") 
parfile[38, ] <- paste("adhc",     paste0(rep(par_data$adhc, 7),collapse=" "), sep=" ")
parfile[39, ] <- paste("b",        paste0(rep(par_data$b, 7),collapse=" "), sep=" ")
parfile[40, ] <- paste("bf",       paste0(rep(par_data$bf, 7),collapse=" "), sep=" ")

write.table(parfile, file.path(cwd, "dat/parameter.txt"),
            append = FALSE, row.names = F, col.names = F, quote = F )

system2(SHUI, args = c("-n 50"))

sim9_data <- read.table(sim9_file)
sim9 <- sim9_data$V2[227:365]   

sim20_data <- read.table(sim20_file)
sim20 <- c(sim20_data$V2[229:276],sim20_data$V2[341:365])  

stopifnot(length(obs20) == length(sim20))

NSE20 <- cal_NSE(obs20,sim20)
PBIAS20 <- cal_PBIAS(obs20,sim20)

NSE9 <- cal_NSE(obs9,sim9)
PBIAS9 <- cal_PBIAS(obs9,sim9)

print(paste("NSE20 =", NSE20))
print(paste("PBIAS20 =", PBIAS20))
print(paste("NSE9 =", NSE9))
print(paste("PBIAS9 =", PBIAS9))

library(lubridate)
# obs_file_plot <- file.path(cwd, "dat/obs_2018_plot_for20.txt") #Null values were NAs
# data <- read.csv(obs_file_plot, , sep = "\t", header = FALSE)
# colnames(data) <- c("Date", "obs_9", "obs_20")
# data$Date <- as.Date(data$Date)
# all_dates <- seq(from = min(data$Date), to = max(data$Date), by = "day")
# complete_data <- data.frame(Date = all_dates)
# complete_data <- merge(complete_data, data, by = "Date", all.x = TRUE)
# complete_data$sim_9 <- sim9_data$V2[227:365]
# complete_data$sim_20 <- sim20_data$V2[227:365]

# Load the data
data <- read.table("dat/obs_2018.txt", header = FALSE, sep = "\t")
# Convert the first column to Date type
data$V1 <- as.Date(data$V1, format = "%Y/%m/%d")
# Identify the date range
start_date <- as.Date("2018-10-04")
end_date <- as.Date("2018-12-06")
# Set the third column to NA for the specified date range
data$V3[data$V1 >= start_date & data$V1 <= end_date] <- NA
start_date <- as.Date("2018-08-15")
end_date <- as.Date("2018-08-16")
data$V3[data$V1 >= start_date & data$V1 <= end_date] <- NA
colnames(data) <- c("Date", "obs_9", "obs_20")
complete_data <- data
complete_data$sim_9 <- sim9_data$V2[227:365]
complete_data$sim_20 <- sim20_data$V2[227:365]


library(ggplot2)
p_20 <- ggplot(complete_data, aes(x = Date)) +
  # 添加散点图层，指定y轴为obs_9，设置点的形状(shape)和颜色(color)
  geom_point(aes(y = obs_20), shape = 1, size = 0.4, color = "blue") +
  # 添加折线图层，指定y轴为sim_9，设置线的类型和颜色
  geom_line(aes(y = sim_20), linetype = "solid", color = "red") +
  # geom_vline(xintercept = as.numeric(as.Date("2019-12-31","%Y-%m-%d")))+
  # geom_vline(xintercept = as.numeric(as.Date("2018-9-1","%Y-%m-%d")))+
  # 选项性地添加图标题、x轴和y轴的标签
  ggtitle("Tong River_20") +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  xlab("Date") +
  ylab("Flow (m3/s)")
  # annotate("text",y=3.8,x = as.Date("2019-3-15","%Y-%m-%d"), 
  #          label = "calibration",color = "#53868B",size=5)+
  # annotate("text",y=3.5,x = as.Date("2019-3-15","%Y-%m-%d"),
  #          label = paste("NSE:",round(NSE20, 3),sep = " "),size=4)+
  # annotate("text",y=3.2,x = as.Date("2019-3-15","%Y-%m-%d"),
  #          label = paste("PBIAS:",round(PBIAS20, 3),sep = " "),size=4)+
  # annotate("text",y=3.8,x = as.Date("2021-3-15","%Y-%m-%d"), 
  #          label = "validation",color = "#53868B",size=5)+
  # annotate("text",y=3.5,x = as.Date("2021-3-15","%Y-%m-%d"),
  #          label = paste("NSE:",round(NSE20_v, 3),sep = " "),size=4)+
  # annotate("text",y=3.2,x = as.Date("2021-3-15","%Y-%m-%d"),
  #          label = paste("PBIAS:",round(PBIAS20_v, 3),sep = " "),size=4)
p_20

p_9 <- ggplot(complete_data, aes(x = Date)) +
  # 添加散点图层，指定y轴为obs_9，设置点的形状(shape)和颜色(color)
  geom_point(aes(y = obs_9), shape = 1, size = 0.4, color = "blue") +
  # 添加折线图层，指定y轴为sim_9，设置线的类型和颜色
  geom_line(aes(y = sim_9), linetype = "solid", color = "red") +
  # geom_vline(xintercept = as.numeric(as.Date("2019-12-31","%Y-%m-%d")))+
  # geom_vline(xintercept = as.numeric(as.Date("2018-9-1","%Y-%m-%d")))+
  # 选项性地添加图标题、x轴和y轴的标签
  ggtitle("Tong River_9") +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  xlab("Date") +
  ylab("Flow (m3/s)")
# annotate("text",y=3.8,x = as.Date("2019-3-15","%Y-%m-%d"),
#          label = "calibration",color = "#53868B",size=5)+
# annotate("text",y=3.5,x = as.Date("2019-3-15","%Y-%m-%d"),
#          label = paste("NSE:",round(NSE20, 3),sep = " "),size=4)+
# annotate("text",y=3.2,x = as.Date("2019-3-15","%Y-%m-%d"),
#          label = paste("PBIAS:",round(PBIAS20, 3),sep = " "),size=4)+
# annotate("text",y=3.8,x = as.Date("2021-3-15","%Y-%m-%d"),
#          label = "validation",color = "#53868B",size=5)+
# annotate("text",y=3.5,x = as.Date("2021-3-15","%Y-%m-%d"),
#          label = paste("NSE:",round(NSE20_v, 3),sep = " "),size=4)+
# annotate("text",y=3.2,x = as.Date("2021-3-15","%Y-%m-%d"),
#          label = paste("PBIAS:",round(PBIAS20_v, 3),sep = " "),size=4)
p_9


