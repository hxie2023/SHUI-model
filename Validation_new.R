#Validation_new, 1202
# rm(list = ls())

cwd <- "/Users/alwiee/Library/Mobile Documents/com~apple~CloudDocs/Coding/SHUI/tyh_flow_cal_riv9_暂定版"
setwd(cwd)

par_file <- file.path(cwd, "par_cal_ga_1130.txt")
par_data <- read.csv(par_file, sep = "\t", header = TRUE)
par_data <- par_data[1,]

parfile <- readLines(file.path(cwd, "dat/parameter.txt"))
parfile <- as.data.frame(parfile)
SHUI <- file.path(cwd, "SHUI")

sim9_file  <- file.path(cwd, "out/rch9.txt")
sim20_file <- file.path(cwd, "out/rch20.txt")
obs_file <- file.path(cwd, "dat/obs_2018_2021_new.txt")
obs <- read.table(obs_file) 
colnames(obs) <- c("date","riv9","riv20","rainfall")

#Calibration new, 2018
obs9_c <- obs$riv9[1:139]  #2018-08-15 ~ 2018-12-31
obs20_c <- c(obs$riv20[3:50],obs$riv20[115:139])  #2018-08-17~2018-12-31,中间有缺失值,共73个有效值
#Validation
obs9_v <- c(obs$riv9[140:697],obs$riv9[753:1051]) #2019-01-01~2020-07-11; 2020-09-05~2021-06-30
obs20_v <- obs$riv20[1001:1051]
# obs20_v <- obs$riv20[1001:1235]

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

#Write parameter file and run model
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

#Extract simulated data
sim9_data <- read.table(sim9_file)
sim20_data <- read.table(sim20_file)
#Calibration
sim9_c <- sim9_data$V2[227:365]   
sim20_c <- c(sim20_data$V2[229:276],sim20_data$V2[341:365])  
stopifnot(length(obs9_c) == length(sim9_c))
stopifnot(length(obs20_c) == length(sim20_c))
#Validation
sim9_v <- c(sim9_data$V2[366:923],sim9_data$V2[979:1277])
sim20_v <- sim20_data$V2[1227:1277]
# sim20_v <- sim20_data$V2[1227:1461] 
stopifnot(length(obs9_v) == length(sim9_v))
stopifnot(length(obs20_v) == length(sim20_v))

NSE20_c <- cal_NSE(obs20_c,sim20_c)
PBIAS20_c <- cal_PBIAS(obs20_c,sim20_c)
NSE20_v <- cal_NSE(obs20_v,sim20_v)
PBIAS20_v <- cal_PBIAS(obs20_v,sim20_v)

NSE9_c <- cal_NSE(obs9_c,sim9_c)
PBIAS9_c <- cal_PBIAS(obs9_c,sim9_c)
NSE9_v <- cal_NSE(obs9_v,sim9_v)
PBIAS9_v <- cal_PBIAS(obs9_v,sim9_v)

print(paste("NSE20 calibration =", NSE20_c))
print(paste("PBIAS20 calibration =", PBIAS20_c))
print(paste("NSE9 calibration =", NSE9_c))
print(paste("PBIAS9 calibration =", PBIAS9_c))

print(paste("NSE20 validation =", NSE20_v))
print(paste("PBIAS20 validation=", PBIAS20_v))
print(paste("NSE9 validation =", NSE9_v))
print(paste("PBIAS9 validation=", PBIAS9_v))

library(lubridate)
# Load the data
data <- read.table("dat/obs_2018_2021_new.txt", header = FALSE, sep = "\t")
# Convert the first column to Date type
data$V1 <- as.Date(data$V1, format = "%Y/%m/%d")

# Set NAs for riv20
start_date <- as.Date("2018-10-04")
end_date <- as.Date("2018-12-06")
# Set the third column to NA for the specified date range
data$V3[data$V1 >= start_date & data$V1 <= end_date] <- NA
start_date <- as.Date("2018-08-15")
end_date <- as.Date("2018-08-16")
data$V3[data$V1 >= start_date & data$V1 <= end_date] <- NA
start_date <- as.Date("2019-01-01")
end_date <- as.Date("2021-05-10")
data$V3[data$V1 >= start_date & data$V1 <= end_date] <- NA

#Set NAs for riv9
start_date <- as.Date("2019-07-15")
end_date <- as.Date("2019-08-27")
data$V2[data$V1 >= start_date & data$V1 <= end_date] <- NA
start_date <- as.Date("2020-07-12")
end_date <- as.Date("2020-09-04")
data$V2[data$V1 >= start_date & data$V1 <= end_date] <- NA

colnames(data) <- c("date", "obs_9", "obs_20", "rainfall")
complete_data <- data[1:1051,]
complete_data$sim_9 <- sim9_data$V2[227:1277]
complete_data$sim_20 <- sim20_data$V2[227:1277]

library(ggplot2)
p_20 <- ggplot(complete_data, aes(x = date)) +
  geom_point(aes(y = obs_20), shape = 1, size = 0.4, color = "blue") +
  geom_line(aes(y = sim_20), linetype = "solid", color = "red") +
  # geom_bar(aes(y = rainfall), stat = "identity", alpha = 0.5)+
  geom_vline(xintercept = as.numeric(as.Date("2018-12-31","%Y-%m-%d")))+
  ggtitle("Tong River_20") +
  scale_x_date(date_breaks = "6 month", date_labels = "%Y-%m") +
  scale_y_continuous(limits = c(0, 5))+
  xlab("Date") +
  ylab("Flow (m3/s)")+
  annotate("text",y=10,x = as.Date("2018-10-15","%Y-%m-%d"), 
           label = "calibration",color = "#53868B",size=5)+
  annotate("text",y=9,x = as.Date("2018-10-15","%Y-%m-%d"),
           label = paste("NSE:",round(NSE20_c, 3),sep = " "),size=4)+
  annotate("text",y=8,x = as.Date("2018-10-15","%Y-%m-%d"),
           label = paste("PBIAS:",round(PBIAS20_c, 3),sep = " "),size=4)+
  annotate("text",y=10,x = as.Date("2021-3-15","%Y-%m-%d"), 
           label = "validation",color = "#53868B",size=5)+
  annotate("text",y=9,x = as.Date("2021-3-15","%Y-%m-%d"),
           label = paste("NSE:",round(NSE20_v, 3),sep = " "),size=4)+
  annotate("text",y=8,x = as.Date("2021-3-15","%Y-%m-%d"),
           label = paste("PBIAS:",round(PBIAS20_v, 3),sep = " "),size=4)
print(p_20)

p_9 <- ggplot(complete_data, aes(x = date)) +
  geom_point(aes(y = obs_9), shape = 1, size = 0.4, color = "blue") +
  geom_line(aes(y = sim_9), linetype = "solid", color = "red") +
  geom_vline(xintercept = as.numeric(as.Date("2018-12-31","%Y-%m-%d")))+
  ggtitle("Tong River_9") +
  scale_x_date(date_breaks = "6 month", date_labels = "%Y-%m") +
  scale_y_continuous(limits = c(0, 40))+
  xlab("Date") +
  ylab("Flow (m3/s)")+
  annotate("text",y=30,x = as.Date("2018-10-15","%Y-%m-%d"),
           label = "calibration",color = "#53868B",size=5)+
  annotate("text",y=27,x = as.Date("2018-10-15","%Y-%m-%d"),
           label = paste("NSE:",round(NSE9_c, 3),sep = " "),size=4)+
  annotate("text",y=24,x = as.Date("2018-10-15","%Y-%m-%d"),
           label = paste("PBIAS:",round(PBIAS9_c, 3),sep = " "),size=4)+
  annotate("text",y=30,x = as.Date("2021-3-15","%Y-%m-%d"),
           label = "validation",color = "#53868B",size=5)+
  annotate("text",y=27,x = as.Date("2021-3-15","%Y-%m-%d"),
           label = paste("NSE:",round(NSE9_v, 3),sep = " "),size=4)+
  annotate("text",y=24,x = as.Date("2021-3-15","%Y-%m-%d"),
           label = paste("PBIAS:",round(PBIAS9_v, 3),sep = " "),size=4)
p_9
