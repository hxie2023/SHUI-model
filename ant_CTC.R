##5、7、15、17、20、23 CTC
cwd <- "/home/smq/code/1/tyh_20240406"
setwd(cwd)
SHUI <- file.path(cwd, "SHUI")
system2(SHUI, args = c("-n 80"))

sim_data7 <- read.table(file.path(cwd, "out/conc_riv7.txt"),header = T)#ng/L
# obs_data7 <- read.table(file.path(cwd, "dat/antriv7.txt")  ,header = T)
obs_data7 <- read.table(file.path(cwd, "out/obsprec_r7.txt")  ,header = T)

sim_data5 <- read.table(file.path(cwd, "out/conc_riv5.txt"),header = T)#ng/L
# obs_data5 <- read.table(file.path(cwd, "dat/antriv5.txt")  ,header = T)
obs_data5 <- read.table(file.path(cwd, "out/obsprec_r5.txt")  ,header = T)

sim_data15 <- read.table(file.path(cwd, "out/conc_riv15.txt"),header = T)#ng/L
# obs_data15 <- read.table(file.path(cwd, "dat/antriv15.txt")  ,header = T)
obs_data15 <- read.table(file.path(cwd, "out/obsprec_r15.txt")  ,header = T)

sim_data17 <- read.table(file.path(cwd, "out/conc_riv17.txt"),header = T)#ng/L
# obs_data17 <- read.table(file.path(cwd, "dat/antriv17.txt")  ,header = T)
obs_data17 <- read.table(file.path(cwd, "out/obsprec_r17.txt")  ,header = T)

sim_data20 <- read.table(file.path(cwd, "out/conc_riv20.txt"),header = T)#ng/L
# obs_data20 <- read.table(file.path(cwd, "dat/antriv20.txt")  ,header = T)
obs_data20 <- read.table(file.path(cwd, "out/obsprec_r20.txt")  ,header = T)

sim_data23 <- read.table(file.path(cwd, "out/conc_riv23.txt"),header = T)#ng/L
# obs_data23 <- read.table(file.path(cwd, "dat/antriv23.txt")  ,header = T)
obs_data23 <- read.table(file.path(cwd, "out/obsprec_r23.txt")  ,header = T)

d7<- merge(obs_data7,sim_data7,by="date")
d5<- merge(obs_data5,sim_data5,by="date")
d17<- merge(obs_data17,sim_data17,by="date")
d15<- merge(obs_data15,sim_data15,by="date")
d20<- merge(obs_data20,sim_data20,by="date")
d23<- merge(obs_data23,sim_data23,by="date")
# View(d)

D7 <- merge(obs_data7,sim_data7,by="date",all=TRUE)
D5 <- merge(obs_data5,sim_data5,by="date",all=TRUE)
D17<- merge(obs_data17,sim_data17,by="date",all=TRUE)
D15<- merge(obs_data15,sim_data15,by="date",all=TRUE)
D20<- merge(obs_data20,sim_data20,by="date",all=TRUE)
D23<- merge(obs_data23,sim_data23,by="date",all=TRUE)


cal7 <-subset(d7 ,select = c("date","CTC.x","CTC.y"))#X:OBS;Y:SIM
cal5 <-subset(d5 ,select = c("date","CTC.x","CTC.y"))
cal17<-subset(d17,select = c("date","CTC.x","CTC.y"))
cal15<-subset(d15,select = c("date","CTC.x","CTC.y"))
cal20<-subset(d20,select = c("date","CTC.x","CTC.y"))
cal23<-subset(d23,select = c("date","CTC.x","CTC.y"))
cal7  <- cal7[apply(cal7 != 0 , 1 , all),]
cal5  <- cal5[apply(cal5 != 0 , 1 , all),]
cal17 <- cal17[apply(cal17!= 0 , 1 , all),]
cal15 <- cal15[apply(cal15!= 0 , 1 , all),]
cal20 <- cal20[apply(cal20!= 0 , 1 , all),]
cal23 <- cal23[apply(cal23!= 0 , 1 , all),]

# View(cal7)
# View(cal5)
# View(cal15)
# View(cal17)
# View(cal20)
# View(cal23)
# qq<-rbind(cal5,cal15,cal20)
# qq<-rbind(cal7,cal17,cal23)
# colnames(qq) <- c("Date","Observed-tc", "Simulated-tc")
# write.csv(qq,file = file.path(cwd,"figtc.txt"),row.names = F)

stopifnot(length(cal7 $CTC.x) == length(cal7 $CTC.y))
stopifnot(length(cal5 $CTC.x) == length(cal5 $CTC.y))
stopifnot(length(cal17$CTC.x) == length(cal17$CTC.y))
stopifnot(length(cal15$CTC.x) == length(cal15$CTC.y))
stopifnot(length(cal20$CTC.x) == length(cal20$CTC.y))
stopifnot(length(cal23$CTC.x) == length(cal23$CTC.y))
NSE_ctc7     <- cal_NSE(cal7$CTC.x, cal7$CTC.y)
NSE_ctc5     <- cal_NSE(cal5$CTC.x, cal5$CTC.y)
NSE_ctc17    <- cal_NSE(cal17$CTC.x, cal17$CTC.y)
NSE_ctc15    <- cal_NSE(cal15$CTC.x, cal15$CTC.y)
NSE_ctc20    <- cal_NSE(cal20$CTC.x, cal20$CTC.y)
NSE_ctc23    <- cal_NSE(cal23$CTC.x, cal23$CTC.y)
PBAIS_ctc7   <- cal_PBIAS(cal7$CTC.x, cal7$CTC.y)
PBAIS_ctc5   <- cal_PBIAS(cal5$CTC.x, cal5$CTC.y)
PBAIS_ctc17  <- cal_PBIAS(cal17$CTC.x, cal17$CTC.y)
PBAIS_ctc15  <- cal_PBIAS(cal15$CTC.x, cal15$CTC.y)
PBAIS_ctc20  <- cal_PBIAS(cal20$CTC.x, cal20$CTC.y)
PBAIS_ctc23  <- cal_PBIAS(cal23$CTC.x, cal23$CTC.y)

RMSE_ctc7    <-rmse(cal7$CTC.x, cal7$CTC.y)
RMSE_ctc5    <-rmse(cal5$CTC.x, cal5$CTC.y)
RMSE_ctc17   <-rmse(cal17$CTC.x, cal17$CTC.y)
RMSE_ctc15   <-rmse(cal15$CTC.x, cal15$CTC.y)
RMSE_ctc20   <-rmse(cal20$CTC.x, cal20$CTC.y)
RMSE_ctc23   <-rmse(cal23$CTC.x, cal23$CTC.y)
Rsq_ctc7     <-cor(cal7$CTC.x, cal7$CTC.y)^2
Rsq_ctc5     <-cor(cal5$CTC.x, cal5$CTC.y)^2
Rsq_ctc17    <-cor(cal17$CTC.x, cal17$CTC.y)^2
Rsq_ctc15    <-cor(cal15$CTC.x, cal15$CTC.y)^2
Rsq_ctc20    <-cor(cal20$CTC.x, cal20$CTC.y)^2
Rsq_ctc23    <-cor(cal23$CTC.x, cal23$CTC.y)^2

total  <- rbind(cal5,cal7,cal15,cal17,cal20,cal23)
t_r2   <- cor(total$CTC.x, total$CTC.y)^2
t_pbais<- cal_PBIAS( total$CTC.x, total$CTC.y)
res<- rbind(t_r2,t_pbais)
write.table(res,path,append =T,row.names=F,col.names=F,quote=F)

# nse<- cbind(NSE_ctc5,NSE_ctc7,NSE_ctc15,NSE_ctc17,NSE_ctc20,NSE_ctc23)
# r2 <- cbind(Rsq_ctc5,Rsq_ctc7,Rsq_ctc15,Rsq_ctc17,Rsq_ctc20,Rsq_ctc23)  
# rms<- cbind(RMSE_ctc5,RMSE_ctc7,RMSE_ctc15,RMSE_ctc17,RMSE_ctc20,RMSE_ctc23)
# pbas<-cbind(PBAIS_ctc5,PBAIS_ctc7,PBAIS_ctc15,PBAIS_ctc17,PBAIS_ctc20,PBAIS_ctc23)
# res<- rbind(nse,r2,rms,pbas)
# 
# write.table(res,path,append =T,row.names=F,col.names=F,quote=F)


fig7 <-subset(D7 ,select = c("date","CTC.x","CTC.y"))#X:OBS;Y:SIM
fig5 <-subset(D5 ,select = c("date","CTC.x","CTC.y"))
fig17<-subset(D17,select = c("date","CTC.x","CTC.y"))
fig15<-subset(D15,select = c("date","CTC.x","CTC.y"))
fig20<-subset(D20,select = c("date","CTC.x","CTC.y"))
fig23<-subset(D23,select = c("date","CTC.x","CTC.y"))

fig7 [fig7  == 0 ] <- NA
fig5 [fig5  == 0 ] <- NA
fig17[fig17 == 0 ] <- NA
fig15[fig15 == 0 ] <- NA
fig20[fig20 == 0 ] <- NA
fig23[fig23 == 0 ] <- NA



fig7$date <-  as.Date(fig7$date)
fig5$date <-  as.Date(fig5$date)
fig17$date <- as.Date(fig17$date)
fig15$date <- as.Date(fig15$date)
fig20$date <- as.Date(fig20$date)
fig23$date <- as.Date(fig23$date)

y=c(0,50)
ynse=14
ypb=12
yrmse=16
yr2=15
yt=30
library(lubridate)
library(ggplot2)
library(cowplot)
# qq<-rbind(cal5,cal7,cal15,cal17,cal20,cal23)
# # View(qq)
# qq$logObserved <- log10(qq$CTC.x)
# qq$logSimulated <- log10(qq$CTC.y)
# # qq$logObserved <- qq$CTC.x
# # qq$logSimulated <- qq$CTC.y
# # 计算统计数据
# 
# cor.test(qq$logObserved,qq$logSimulated,alternative = "two.sided",method = "pearson",conf.level = 0.95)
# 
# R <- cor(qq$logObserved,qq$logSimulated)^2
# RMSE <- sqrt(mean((qq$logObserved -  qq$logSimulated)^2))
# Pbias <- 100 * sum(qq$logSimulated - qq$logObserved) / sum(qq$logObserved)
# 
# # 绘制散点图
# df_poly <- data.frame( x=c(0,0,2.5,3,3,0.5), y=c(0,0.5,3,3,2.5,0))
# ggplot(qq, aes(x = logSimulated, y = logObserved)) +
#   # geom_rect(aes(xmin=0,xmax=5,ymin=1,ymax=3),
#   #           fill="grey",alpha=0.3)+
#   # geom_abline(slope = seq(1, 0, 0.00011), color = "grey60"),intercept =(1, 0, 0.00011) +
#   # geom_abline(slope = 1,intercept =seq(-1, 1, 0.01),color = "grey",linewidth=4,alpha=0.02) +
#   geom_abline(intercept = 0, slope = 1,linewidth=0.3) +
#   geom_polygon(data=df_poly, aes(x, y), fill="blue", alpha=0.1)+
#   # geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "grey") +
#   # geom_abline(intercept = -1, slope = 1, linetype = "dashed", color = "grey") +
#   geom_point(shape = 3, stroke = 1) +  # Change the shape of the points
#   # annotate("text", x = 0.18, y = 2.3, label = sprintf("R = %.2f\nRMSE = %.2f\nPbias = %.0f%%", R, RMSE, Pbias),
#   #          hjust = 0, vjust = 0, size = 6,family = "serif") +
#   annotate("text", x = 0.1, y = 2, label = sprintf("R = %.2f\nRMSE = %.2f\nPbias = %.0f%%", R, RMSE, Pbias), hjust = 0, vjust = 0, size = 4)+
#   scale_x_continuous(expand = c(0, 0), limits = c(0, 3)) +  # Set x-axis range from 0 to 3
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 3)) +  # Set y-axis range from 0 to 3
#   # labs(x = "Simulated log (mg/L)", y = "Observed log(mg/L)") +
#   labs(x = "CTC Simulated (mg/L)", y = "Observed (mg/L)") +
#   theme_bw() #+

# testd7$date <- as.Date(testd7$date)
# p99<-ggplot(testd7,aes(x=date))+
#   geom_line(aes(y = VOL1), linetype = "solid", color = "#3E90BF")+
# p99
p1 <- ggplot(fig5 , aes(x = date)) +
  # 添加散点图层，指定y轴为obs_9，设置点的形状(shape)和颜色(color)
  geom_point(aes(y = CTC.x), shape = 1, color = "#ED8D5A",stroke = 1) +
  # 添加折线图层，指定y轴为sim_9，设置线的类型和颜色
  geom_line(aes(y = CTC.y), linetype = "solid", color = "#3E90BF") +
  theme_light() +
  ylab("riv5-ctc")+
  scale_y_continuous(expand = c(0,0),limits = y)+
  scale_x_date(limits = as.Date(c("2020-12-31", "2022-01-01")),
               date_breaks = "5 month",
               date_labels = "%Y-%m")+
  theme(axis.text = element_text(size = 11.5,colour="black",family = "Times New Roman"),
        text =element_text(size = 12,colour="black",family = "Times New Roman"))+
  annotate("text", x = as.Date("2021-9-1","%Y-%m-%d"), y = 20,
           label = sprintf("rmse = %.2f\nrsq = %.3f\npbais=%.2f%%\nnse=%.2f", RMSE_ctc5,Rsq_ctc5,PBAIS_ctc5,NSE_ctc5 ),
           hjust = 0, vjust = 0, size = 6,family = "Times New Roman") 

p2 <- ggplot(fig7 , aes(x = date)) +
  # 添加散点图层，指定y轴为obs_9，设置点的形状(shape)和颜色(color)
  geom_point(aes(y = CTC.x), shape = 1, color = "#ED8D5A",stroke = 1) +
  # 添加折线图层，指定y轴为sim_9，设置线的类型和颜色
  geom_line(aes(y = CTC.y), linetype = "solid", color = "#3E90BF") +
  theme_light() +
  ylab("riv7-ctc")+
  scale_y_continuous(expand = c(0,0),limits = y)+
  scale_x_date(limits = as.Date(c("2020-12-31", "2022-01-01")),
               date_breaks = "5 month",
               date_labels = "%Y-%m")+
  theme(axis.text = element_text(size = 11.5,colour="black",family = "Times New Roman"),
        text =element_text(size = 12,colour="black",family = "Times New Roman"))+
  annotate("text", x = as.Date("2021-9-1","%Y-%m-%d"), y = 20,
           label = sprintf("rmse = %.2f\nrsq = %.3f\npbais=%.2f%%\nnse=%.2f", RMSE_ctc7,Rsq_ctc7,PBAIS_ctc7,NSE_ctc7 ),
           hjust = 0, vjust = 0, size = 6,family = "Times New Roman") 

p3 <- ggplot(fig15 , aes(x = date)) +
  # 添加散点图层，指定y轴为obs_9，设置点的形状(shape)和颜色(color)
  geom_point(aes(y = CTC.x), shape = 1, color = "#ED8D5A",stroke = 1) +
  # 添加折线图层，指定y轴为sim_9，设置线的类型和颜色
  geom_line(aes(y = CTC.y), linetype = "solid", color = "#3E90BF") +
  theme_light() +
  ylab("riv15-ctc")+
  scale_y_continuous(expand = c(0,0),limits = y)+
  scale_x_date(limits = as.Date(c("2020-12-31", "2022-01-01")),
               date_breaks = "5 month",
               date_labels = "%Y-%m")+
  theme(axis.text = element_text(size = 11.5,colour="black",family = "Times New Roman"),
        text =element_text(size = 12,colour="black",family = "Times New Roman"))+
  annotate("text", x = as.Date("2021-9-1","%Y-%m-%d"), y = 20,
           label = sprintf("rmse = %.2f\nrsq = %.3f\npbais=%.2f%%\nnse=%.2f", RMSE_ctc15,Rsq_ctc15,PBAIS_ctc15,NSE_ctc15),
           hjust = 0, vjust = 0, size = 6,family = "Times New Roman") 

p4 <- ggplot(fig17 , aes(x = date)) +
  # 添加散点图层，指定y轴为obs_9，设置点的形状(shape)和颜色(color)
  geom_point(aes(y = CTC.x), shape = 1, color = "#ED8D5A",stroke = 1) +
  # 添加折线图层，指定y轴为sim_9，设置线的类型和颜色
  geom_line(aes(y = CTC.y), linetype = "solid", color = "#3E90BF") +
  theme_light() +
  ylab("riv17-ctc")+
  scale_y_continuous(expand = c(0,0),limits = y)+
  scale_x_date(limits = as.Date(c("2020-12-31", "2022-01-01")),
               date_breaks = "5 month",
               date_labels = "%Y-%m")+
  theme(axis.text = element_text(size = 11.5,colour="black",family = "Times New Roman"),
        text =element_text(size = 12,colour="black",family = "Times New Roman"))+
  annotate("text", x = as.Date("2021-9-1","%Y-%m-%d"), y = 20,
           label = sprintf("rmse = %.2f\nrsq = %.3f\npbais=%.2f%%\nnse=%.2f", RMSE_ctc17,Rsq_ctc17,PBAIS_ctc17,NSE_ctc17),
           hjust = 0, vjust = 0, size = 6,family = "Times New Roman")

p5 <- ggplot(fig20 , aes(x = date)) +
  # 添加散点图层，指定y轴为obs_9，设置点的形状(shape)和颜色(color)
  geom_point(aes(y = CTC.x), shape = 1, color = "#ED8D5A",stroke = 1) +
  # 添加折线图层，指定y轴为sim_9，设置线的类型和颜色
  geom_line(aes(y = CTC.y), linetype = "solid", color = "#3E90BF") +
  theme_light() +
  ylab("riv20-ctc")+
  scale_y_continuous(expand = c(0,0),limits = y)+
  scale_x_date(limits = as.Date(c("2020-12-31", "2022-01-01")),
               date_breaks = "5 month",
               date_labels = "%Y-%m")+
  theme(axis.text = element_text(size = 11.5,colour="black",family = "Times New Roman"),
        text =element_text(size = 12,colour="black",family = "Times New Roman"))+
  annotate("text", x = as.Date("2021-9-1","%Y-%m-%d"), y = 20,
           label = sprintf("rmse = %.2f\nrsq = %.3f\npbais=%.2f%%\nnse=%.2f", RMSE_ctc20,Rsq_ctc20,PBAIS_ctc20,NSE_ctc20),
           hjust = 0, vjust = 0, size = 6,family = "Times New Roman")

p6 <- ggplot(fig23 , aes(x = date)) +
  # 添加散点图层，指定y轴为obs_9，设置点的形状(shape)和颜色(color)
  geom_point(aes(y = CTC.x), shape = 1, color = "#ED8D5A",stroke = 1) +
  # 添加折线图层，指定y轴为sim_9，设置线的类型和颜色
  geom_line(aes(y = CTC.y), linetype = "solid", color = "#3E90BF") +
  theme_light() +
  ylab("riv23-ctc")+
  scale_y_continuous(expand = c(0,0),limits = y)+
  scale_x_date(limits = as.Date(c("2020-12-31", "2022-01-01")),
               date_breaks = "5 month",
               date_labels = "%Y-%m")+
  theme(axis.text = element_text(size = 11.5,colour="black",family = "Times New Roman"),
        text =element_text(size = 12,colour="black",family = "Times New Roman"))+
  annotate("text", x = as.Date("2021-9-1","%Y-%m-%d"), y = 20,
           label = sprintf("rmse = %.2f\nrsq = %.3f\npbais=%.2f%%\nnse=%.2f", RMSE_ctc23,Rsq_ctc23,PBAIS_ctc23,NSE_ctc23),
           hjust = 0, vjust = 0, size = 6,family = "Times New Roman")




f<- cowplot::plot_grid(p1,p2,p3,p4,p5,p6, nrow = 2)
f