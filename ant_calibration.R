#calibration & validation period
#Evaluate one parameter group
#NSE, RMSE, etc.
#Also plot for hydrograph comparison
rm(list = ls())

# install.packages("Metrics")
library(Metrics)

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

# cal_RMSE <- function(observed, predicted) {
#   errors<-(observed-predicted)^2
#   RMSE<-sqrt(mean(errors))
#   return(RMSE)
# }

library(Metrics)

#
cwd <- "/home/smq/code/1/tyh_20240406"
setwd(cwd)


# rivid<-paste("riv",riv,sep="")
SHUI <- file.path(cwd, "SHUI")

# parfile <- readLines(file.path(cwd, "dat/tyh_par_sed.txt"))
# parfile <- as.data.frame(parfile)
# 
# parfile[21, ] <- paste("idriv", riv,  sep = " ")
# write.table(parfile, file.path(cwd, "dat/tyh_par_sed.txt"),
#             append = FALSE, row.names = F, col.names = F, quote = F )

system2(SHUI, args = c("-n 100"))

# sim_file  <- file.path(cwd, "out/antconc.txt")#ng/L
# # sim20_file <- file.path(cwd, "out/rch20.txt")
# obs_file <- file.path(cwd, "dat/obsant.txt")

# obs_data <- read.table(obs_file,header = T)
# sim_data <- read.table(sim_file,header = T)
# obs_data [obs_data==0] = NA 
riv<-5
sim_data <- read.table(file.path(cwd, "out/conc_riv5.txt"),header = T)#ng/L
obs_data <- read.table(file.path(cwd,   "dat/antriv5.txt")  ,header = T)


d<- merge(obs_data,sim_data,by="date")
# View(d)

D<- merge(obs_data,sim_data,by="date",all=TRUE)
# View(D)

calTC<-subset(d,select = c("date","TC.x","TC.y"))#X:OBS;Y:SIM
calDXC<-subset(d,select = c("date","DXC.x","DXC.y"))
calOTC<-subset(d,select = c("date","OTC.x","OTC.y"))
calCTC<-subset(d,select = c("date","CTC.x","CTC.y"))

# View(calTC)

calTC  <- calTC [apply(calTC != 0 , 1 , all),]
calDXC <- calDXC[apply(calDXC!= 0 , 1 , all),]
calOTC <- calOTC[apply(calOTC!= 0 , 1 , all),]
calCTC <- calCTC[apply(calCTC!= 0 , 1 , all),]

stopifnot(length(calTC$TC.x) == length(calTC$TC.y))
NSE_tc <- cal_NSE(calTC$TC.x, calTC$TC.y)
PBIAS_tc  <- cal_PBIAS(calTC$TC.x, calTC$TC.y)
RMSE_tc<-rmse(calTC$TC.y, calTC$TC.x)
Rsq_tc <-cor(calTC$TC.x, calTC$TC.y)^2
PBIAS_tc

stopifnot(length(calDXC$DXC.x) == length(calDXC$DXC.y))
NSE_dxc    <- cal_NSE(calDXC$DXC.x,calDXC$DXC.y)
PBIAS_dxc  <- cal_PBIAS(calDXC$DXC.x,calDXC$DXC.y)
RMSE_dxc   <-rmse(calDXC$DXC.x,calDXC$DXC.y)
Rsq_dxc    <-cor(calDXC$DXC.x,calDXC$DXC.y)^2
PBIAS_dxc

stopifnot(length(calOTC$OTC.x) == length(calOTC$OTC.y))
NSE_otc    <- cal_NSE(calOTC$OTC.x,calOTC$OTC.y)
PBIAS_otc  <- cal_PBIAS(calOTC$OTC.x,calOTC$OTC.y)
RMSE_otc   <-rmse(calOTC$OTC.x,calOTC$OTC.y)
Rsq_otc    <-cor(calOTC$OTC.x,calOTC$OTC.y)^2

stopifnot(length(calCTC$CTC.x) == length(calCTC$CTC.y))
NSE_ctc    <- cal_NSE(calCTC$CTC.x,calCTC$CTC.y)
PBIAS_ctc  <- cal_PBIAS(calCTC$CTC.x,calCTC$CTC.y)
RMSE_ctc   <-rmse(calCTC$CTC.x,calCTC$CTC.y)
Rsq_ctc    <-cor(calCTC$CTC.x,calCTC$CTC.y)^2



figTC <-subset (D,select = c("date","TC.x","TC.y"))#X:OBS;Y:SIM
figDXC<-subset(D,select = c("date","DXC.x","DXC.y"))
figOTC<-subset(D,select = c("date","OTC.x","OTC.y"))
figCTC<-subset(D,select = c("date","CTC.x","CTC.y"))

FIG<-merge(figTC,figCTC,by="date")
FIG<-merge(FIG,figOTC,by="date")
FIG<-merge(FIG,figDXC,by="date")
colnames(FIG) <- c("Date","Observed-tc", "Simulated-tc","Observed-ctc", "Simulated-ctc",
                     "Observed-otc", "Simulated-otc","Observed-dxc", "Simulated-dxc")
write.csv(FIG,file = file.path(cwd,"figtc.txt"),row.names = F)
# View(FIG)
# View(figDXC)
# colnames(complete_data) <- c("date","obs","sim")
figTC$date <- as.Date(figTC$date)
figDXC$date <- as.Date(figDXC$date)
figOTC$date <- as.Date(figOTC$date)
figCTC$date <- as.Date(figCTC$date)

# View(complete_data)
figTC [figTC  == 0 ] <- NA
figDXC[figDXC == 0 ] <- NA
figOTC[figOTC == 0 ] <- NA
figCTC[figCTC == 0 ] <- NA


y=c(0,30)
ynse=12
ypb=12
yrmse=16
yr2=9
yt=30
library(lubridate)
library(ggplot2)
library(cowplot)


# plot_tc
p1 <- ggplot(figTC , aes(x = date)) +
  # 添加散点图层，指定y轴为obs_9，设置点的形状(shape)和颜色(color)
  geom_point(aes(y = TC.x), shape = 1, color = "#ED8D5A",stroke = 1) +
  # 添加折线图层，指定y轴为sim_9，设置线的类型和颜色
  geom_line(aes(y = TC.y), linetype = "solid", color = "#3E90BF") +
  # geom_vline(xintercept = as.numeric(as.Date("2019-12-31","%Y-%m-%d")))+
  # geom_vline(xintercept = as.numeric(as.Date("2018-9-1","%Y-%m-%d")))+
  # 选项性地添加图标题、x轴和y轴的标签
  # ggtitle(
  #   paste("River_",riv,"  antibiotic calibration")) +
  # xlab("Date")+
  ylab("TC Concentration(ng/L)")+
  theme_light() +
  scale_y_continuous(expand = c(0,0),limits = y)+
  scale_x_date(limits = as.Date(c("2020-12-31", "2022-01-01")),
               date_breaks = "5 month",
               date_labels = "%Y-%m")+
  # theme(axis.title.y = element_blank())+
  theme(axis.text = element_text(size = 11.5,colour="black",family = "Times New Roman"),
        text =element_text(size = 12,colour="black",family = "Times New Roman"))+
  annotate("text", x = as.Date("2021-9-1","%Y-%m-%d"), y = yr2,
                  label = sprintf("rmse = %.2f\nrsq = %.3f\npbais=%.2f%%\nnse=%.2f", RMSE_tc,Rsq_tc,PBIAS_tc,NSE_tc),
                  hjust = 0, vjust = 0, size = 6,family = "Times New Roman")
        
  

# p1
# plot_dxc
p2 <- ggplot(figDXC , aes(x = date)) +
  # 添加散点图层，指定y轴为obs_9，设置点的形状(shape)和颜色(color)
  geom_point(aes(y = DXC.x), shape = 1, color = "#ED8D5A",stroke = 1) +
  # 添加折线图层，指定y轴为sim_9，设置线的类型和颜色
  geom_line(aes(y = DXC.y), linetype = "solid", color = "#3E90BF") +
  # geom_vline(xintercept = as.numeric(as.Date("2019-12-31","%Y-%m-%d")))+
  # geom_vline(xintercept = as.numeric(as.Date("2018-9-1","%Y-%m-%d")))+
  # 选项性地添加图标题、x轴和y轴的标签
  ggtitle(
    paste("River_",riv,"  antibiotic calibration")) +
  # xlab("Date")+
  ylab("DXC Concentration(ng/L)")+
  theme_light() +
  scale_y_continuous(expand = c(0,0),limits = y)+
  scale_x_date(limits = as.Date(c("2020-12-31", "2022-01-01")),
               date_breaks = "5 month",
               date_labels = "%Y-%m")+
  # theme(axis.title.y = element_blank())+
  theme(axis.text = element_text(size = 11.5,colour="black",family = "Times New Roman"),
        text =element_text(size = 12,colour="black",family = "Times New Roman"))+
  annotate("text", x = as.Date("2021-9-1","%Y-%m-%d"), y = yr2,
                  label = sprintf("rmse = %.2f\nrsq = %.3f\npbais=%.2f%%\nnse=%.2f", RMSE_dxc,Rsq_dxc,PBIAS_dxc,NSE_dxc),
                  hjust = 0, vjust = 0, size = 6,family = "Times New Roman")     
        


# plot_otc
p3 <- ggplot(figOTC , aes(x = date)) +
  # 添加散点图层，指定y轴为obs_9，设置点的形状(shape)和颜色(color)
  geom_point(aes(y = OTC.x), shape = 1, color = "#ED8D5A",stroke = 1) +
  # 添加折线图层，指定y轴为sim_9，设置线的类型和颜色
  geom_line(aes(y = OTC.y), linetype = "solid", color = "#3E90BF") +
  # geom_vline(xintercept = as.numeric(as.Date("2019-12-31","%Y-%m-%d")))+
  # geom_vline(xintercept = as.numeric(as.Date("2018-9-1","%Y-%m-%d")))+
  # 选项性地添加图标题、x轴和y轴的标签
  # ggtitle(
  #   paste("River_",riv,"  antibiotic calibration")) +
  # xlab("Date")+
  ylab("OTC Concentration(ng/L)")+
  theme_light() +
  scale_y_continuous(expand = c(0,0),limits =y)+
  scale_x_date(limits = as.Date(c("2020-12-31", "2022-01-01")),
               date_breaks = "5 month",
               date_labels = "%Y-%m")+
  # theme(axis.title.y = element_blank())+
  theme(axis.text = element_text(size = 11.5,colour="black",family = "Times New Roman"),
        text =element_text(size = 12,colour="black",family = "Times New Roman")) +
 annotate("text", x = as.Date("2021-9-1","%Y-%m-%d"), y = yr2,
                  label = sprintf("rmse = %.2f\nrsq = %.3f\npbais=%.2f%%\nnse=%.2f", RMSE_otc,Rsq_otc,PBIAS_otc,NSE_otc),
                  hjust = 0, vjust = 0, size = 6,family = "Times New Roman")     
        


# plot_ctc
p4 <- ggplot(figCTC , aes(x = date)) +
  # 添加散点图层，指定y轴为obs_9，设置点的形状(shape)和颜色(color)
  geom_point(aes(y = CTC.x), shape = 1, color = "#ED8D5A",stroke = 1) +
  # 添加折线图层，指定y轴为sim_9，设置线的类型和颜色
  geom_line(aes(y = CTC.y), linetype = "solid", color = "#3E90BF") +
  # geom_vline(xintercept = as.numeric(as.Date("2019-12-31","%Y-%m-%d")))+
  # geom_vline(xintercept = as.numeric(as.Date("2018-9-1","%Y-%m-%d")))+
  # 选项性地添加图标题、x轴和y轴的标签
  # ggtitle(
    # paste("River_",riv,"  antibiotic calibration")) +
  # xlab("Date")+
  ylab("CTC Concentration(ng/L)")+
  theme_light() +
  scale_y_continuous(expand = c(0,0),limits =y)+
  scale_x_date(limits = as.Date(c("2020-12-31", "2022-01-01")),
               date_breaks = "5 month",
               date_labels = "%Y-%m")+
  # theme(axis.title.y = element_blank())+
  theme(axis.text = element_text(size = 11.5,colour="black",family = "Times New Roman"),
        text =element_text(size = 12,colour="black",family = "Times New Roman") )+
  annotate("text", x = as.Date("2021-9-1","%Y-%m-%d"), y = yr2,
                  label = sprintf("rmse = %.2f\nrsq = %.3f\npbais=%.2f%%\nnse=%.2f", RMSE_ctc,Rsq_ctc,PBIAS_ctc,NSE_ctc),
                  hjust = 0, vjust = 0, size = 6,family = "Times New Roman")     
       


p5 <-cowplot::plot_grid(p1, p2, p3, p4, nrow = 1)#将p1-p4四幅图组合成一幅图，按照两行两列排列，标签分别为A、B、C、D。（LETTERS[1:4] 意为提取26个大写英文字母的前四个:A、B、C、D）
p5


# colnames(figTC) <- c("Date","Observed-tc", "Simulated-tc")
# write.csv(figTC,file = file.path(cwd,"figtc.txt"),row.names = F)
# 
# colnames(figCTC) <- c("Date","Observed-ctc", "Simulated-ctc")
# write.csv(figCTC,file = file.path(cwd,"figctc.txt"),row.names = F)
# 
# colnames(figOTC) <- c("Date","Observed-otc", "Simulated-otc")
# write.csv(figOTC,file = file.path(cwd,"figotc.txt"),row.names = F)
# 
# colnames(figDXC) <- c("Date","Observed-dxc", "Simulated-dxc")
# write.csv(figDXC,file = file.path(cwd,"figdxc.txt"),row.names = F)




# 
# ggsave(p5,filename = paste("ant",riv,"_ant",".png",sep = ""), # 保存的文件名称。通过后缀来决定生成什么格式的图片
#   width = 11.95,             # 宽
#   height = 3.03,            # 高
#   units = "in",          # 单位
#   dpi = 300,              # 分辨率DPI
#   path = file.path(cwd,"fig")
# )
# 
# 
# dev.off()






