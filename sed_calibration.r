#calibration & validation period
#Evaluate one parameter group
#NSE, RMSE, etc.
#Also plot for hydrograph comparison
rm(list = ls())

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

#
cwd <- "/home/smq/code/1/tyh_20240406"
setwd(cwd)

riv<-20
rivid<-paste("riv",riv,sep="")
SHUI <- file.path(cwd, "SHUI")

parfile <- readLines(file.path(cwd, "dat/tyh_par_sed.txt"))
parfile <- as.data.frame(parfile)
sim_file  <- file.path(cwd, "out/sedconc.txt")
# sim20_file <- file.path(cwd, "out/rch20.txt")
obs_file <- file.path(cwd, "dat/obssed.txt")

parfile[21, ] <- paste("idriv", riv,  sep = " ")
write.table(parfile, file.path(cwd, "dat/tyh_par_sed.txt"),
            append = FALSE, row.names = F, col.names = F, quote = F )


system2(SHUI, args = c("-n 50"))

obs_data <- read.table(obs_file,header = T)
sim_data <- read.table(sim_file,header = T)

d<-merge(obs_data,sim_data,by="date")
D<-merge(sim_data,obs_data,by="date",all=TRUE)
# View(D)
stopifnot(length(d[,2]) == length(d$sedconc))
NSE <- cal_NSE(d[,2], d$sedconc)
PBIAS <- cal_PBIAS(d[,2], d$sedconc) #unit : %
NSE
PBIAS


complete_data <- merge(sim_data[367:731,],obs_data,by="date",all=TRUE)
complete_data<-subset(complete_data,select = c("date",rivid,"sedconc"))
colnames(complete_data) <- c("date","obs","sim")
complete_data$date <- as.Date(complete_data$date)
View(complete_data)

print("Calibration:")


library(lubridate)
library(ggplot2)
# 

p <- ggplot(complete_data, aes(x = date)) +
  # 添加散点图层，指定y轴为obs_9，设置点的形状(shape)和颜色(color)
  geom_point(aes(y = obs), shape = 1, color = "#ED8D5A",stroke = 1) +
  # 添加折线图层，指定y轴为sim_9，设置线的类型和颜色
  geom_line(aes(y = sim), linetype = "solid", color = "#3E90BF") +
  # geom_vline(xintercept = as.numeric(as.Date("2019-12-31","%Y-%m-%d")))+
  # geom_vline(xintercept = as.numeric(as.Date("2018-9-1","%Y-%m-%d")))+
  # 选项性地添加图标题、x轴和y轴的标签
  ggtitle(
    paste("River_",riv)) +
    xlab("Date")+
    ylab("Suspended sediment concentration(mg/L)")+
    theme_light() +
    scale_y_continuous(expand = c(0,0),limits = c (0,550))+
    scale_x_date(limits = as.Date(c("2021-01-01", "2021-12-31")),
                date_breaks = "2 month",
                date_labels = "%Y-%m")+
    theme(axis.text = element_text(size = 11.5,colour="black",family = "Times New Roman"),
          text =element_text(size = 12,colour="black",family = "Times New Roman")
          )+
   annotate("text" ,y=395,x = as.Date("2021-11-7","%Y-%m-%d"),
           label = paste("NSE:",round(NSE, 3),sep = " "),size=4,family = "Times New Roman")+
   annotate("text",y=380,x = as.Date("2021-11-15","%Y-%m-%d"),
             label = paste("PBIAS:",round(PBIAS, 3),"%"),size=4,family = "Times New Roman")
p         
ggsave(
  filename = paste("River_",riv,".png",sep = ""), # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 6.5,             # 宽
  height = 3.5,            # 高
  units = "in",          # 单位
  dpi = 300,              # 分辨率DPI
  path = file.path(cwd,"fig")
  )


dev.off()







