cwd <- "/home/smq/code/1/tyh_20240406"
setwd(cwd)
SHUI <- file.path(cwd, "SHUI")
system2(SHUI, args = c("-n 80"))

sim_data7 <- read.table(file.path(cwd, "out/conc_riv7.txt"),header = T)#ng/L
obs_data7 <- read.table(file.path(cwd, "dat/antriv7.txt")  ,header = T)

sim_data5 <- read.table(file.path(cwd, "out/conc_riv5.txt"),header = T)#ng/L
obs_data5 <- read.table(file.path(cwd, "dat/antriv5.txt")  ,header = T)

d7<- merge(obs_data7,sim_data7,by="date")
d5<- merge(obs_data5,sim_data5,by="date")
# View(d)

D7<- merge(obs_data7,sim_data7,by="date",all=TRUE)
D5<- merge(obs_data5,sim_data5,by="date",all=TRUE)


cal5TC<- subset(d5,select = c("date","TC.x","TC.y"))#X:OBS;Y:SIM
cal5DXC<-subset(d5,select = c("date","DXC.x","DXC.y"))
cal5OTC<-subset(d5,select = c("date","OTC.x","OTC.y"))
cal5CTC<-subset(d5,select = c("date","CTC.x","CTC.y"))
cal5TC<- cal5TC[complete.cases(cal5TC),]
cal5DXC<-cal5DXC[complete.cases(cal5DXC),]
cal5OTC<-cal5OTC[complete.cases(cal5OTC),]
cal5CTC<-cal5CTC[complete.cases(cal5CTC),]


cal7TC<- subset(d7,select = c("date","TC.x","TC.y"))#X:OBS;Y:SIM
cal7DXC<-subset(d7,select = c("date","DXC.x","DXC.y"))
cal7OTC<-subset(d7,select = c("date","OTC.x","OTC.y"))
cal7CTC<-subset(d7,select = c("date","CTC.x","CTC.y"))
cal7TC<- cal7TC[complete.cases (cal7TC ),]
cal7DXC<-cal7DXC[complete.cases(cal7DXC),]
cal7OTC<-cal7OTC[complete.cases(cal7OTC),]
cal7CTC<-cal7CTC[complete.cases(cal7CTC),]


stopifnot(length(cal5TC$TC.x) == length(cal5TC$TC.y))
NSE_tc5    <- cal_NSE(cal5TC$TC.x, cal5TC$TC.y)
PBAIS_tc5  <- cal_PBIAS(cal7TC$TC.x, cal5TC$TC.y)
RMSE_tc5   <-rmse(cal5TC$TC.y, cal5TC$TC.x)
Rsq_tc5    <-cor(cal5TC$TC.x, cal5TC$TC.y)^2
stopifnot(length(cal7TC$TC.x) == length(cal7TC$TC.y))
NSE_tc7    <- cal_NSE(cal7TC$TC.x, cal7TC$TC.y)
PBAIS_tc7  <- cal_PBIAS(cal7TC$TC.x, cal7TC$TC.y)
RMSE_tc7   <-rmse(cal7TC$TC.y, cal7TC$TC.x)
Rsq_tc7    <-cor(cal7TC$TC.x, cal7TC$TC.y)^2


stopifnot(length(cal5DXC$DXC.x) == length(cal5DXC$DXC.y))
NSE_dxc5    <- cal_NSE(cal5DXC$DXC.x,cal5DXC$DXC.y)
PBAIS_dxc5 <- cal_PBIAS(cal5DXC$DXC.x,cal5DXC$DXC.y)
RMSE_dxc5   <-rmse(cal5DXC$DXC.x,cal5DXC$DXC.y)
Rsq_dxc5    <-cor(cal5DXC$DXC.x,cal5DXC$DXC.y)^2
stopifnot(length(cal7DXC$DXC.x) == length(cal7DXC$DXC.y))
NSE_dxc7    <- cal_NSE(cal7DXC$DXC.x,cal7DXC$DXC.y)
PBAIS_dxc7  <- cal_PBIAS(cal7DXC$DXC.x,cal7DXC$DXC.y)
RMSE_dxc7   <-rmse(cal7DXC$DXC.x,cal7DXC$DXC.y)
Rsq_dxc7    <-cor(cal7DXC$DXC.x,cal7DXC$DXC.y)^2



stopifnot(length(cal5OTC$OTC.x) == length(cal5OTC$OTC.y))
NSE_otc5    <- cal_NSE(cal5OTC$OTC.x,cal5OTC$OTC.y)
PBAIS_otc5  <- cal_PBIAS(cal5OTC$OTC.x,cal5OTC$OTC.y)
RMSE_otc5   <-rmse(cal5OTC$OTC.x,cal5OTC$OTC.y)
Rsq_otc5    <-cor(cal5OTC$OTC.x,cal5OTC$OTC.y)^2
stopifnot(length(cal7OTC$OTC.x) == length(cal7OTC$OTC.y))
NSE_otc7    <- cal_NSE(cal7OTC$OTC.x,cal7OTC$OTC.y)
PBAIS_otc7  <- cal_PBIAS(cal7OTC$OTC.x,cal7OTC$OTC.y)
RMSE_otc7   <-rmse(cal7OTC$OTC.x,cal7OTC$OTC.y)
Rsq_otc7    <-cor(cal7OTC$OTC.x,cal7OTC$OTC.y)^2

stopifnot(length(cal5CTC$CTC.x) == length(cal5CTC$CTC.y))
NSE_ctc5    <- cal_NSE(cal5CTC$CTC.x,cal5CTC$CTC.y)
PBAIS_ctc5  <- cal_PBIAS(cal5CTC$CTC.x,cal5CTC$CTC.y)
RMSE_ctc5   <-rmse(cal5CTC$CTC.x,cal5CTC$CTC.y)
Rsq_ctc5   <-cor(cal5CTC$CTC.x,cal5CTC$CTC.y)^2
stopifnot(length(cal7CTC$CTC.x) == length(cal7CTC$CTC.y))
NSE_ctc7    <- cal_NSE(cal7CTC$CTC.x,cal7CTC$CTC.y)
PBAIS_ctc7  <- cal_PBIAS(cal7CTC$CTC.x,cal7CTC$CTC.y)
RMSE_ctc7   <-rmse(cal7CTC$CTC.x,cal7CTC$CTC.y)
Rsq_ctc7    <-cor(cal7CTC$CTC.x,cal7CTC$CTC.y)^2


fig5TC <-subset(D5,select = c("date","TC.x","TC.y"))#X:OBS;Y:SIM
fig5DXC<-subset(D5,select = c("date","DXC.x","DXC.y"))
fig5OTC<-subset(D5,select = c("date","OTC.x","OTC.y"))
fig5CTC<-subset(D5,select = c("date","CTC.x","CTC.y"))
fig5TC[fig5TC == 0 ] <- NA
fig5DXC[fig5DXC == 0 ] <- NA
fig5OTC[fig5OTC == 0 ] <- NA
fig5CTC[fig5CTC == 0 ] <- NA


fig7TC <-subset(D7,select = c("date","TC.x","TC.y"))#X:OBS;Y:SIM
fig7DXC<-subset(D7,select = c("date","DXC.x","DXC.y"))
fig7OTC<-subset(D7,select = c("date","OTC.x","OTC.y"))
fig7CTC<-subset(D7,select = c("date","CTC.x","CTC.y"))
fig7TC[fig7TC == 0 ] <- NA
fig7DXC[fig7DXC == 0 ] <- NA
fig7OTC[fig7OTC == 0 ] <- NA
fig7CTC[fig7CTC == 0 ] <- NA

fig5TC$date <-  as.Date(fig5TC$date)
fig5DXC$date <- as.Date(fig5DXC$date)
fig5OTC$date <- as.Date(fig5OTC$date)
fig5CTC$date <- as.Date(fig5CTC$date)



fig7TC$date <-  as.Date(fig7TC$date)
fig7DXC$date <- as.Date(fig7DXC$date)
fig7OTC$date <- as.Date(fig7OTC$date)
fig7CTC$date <- as.Date(fig7CTC$date)

y=c(0,50)
ynse=14
ypb=12
yrmse=16
yr2=15
yt=30
library(lubridate)
library(ggplot2)
library(cowplot)


# testd7$date <- as.Date(testd7$date)
# p99<-ggplot(testd7,aes(x=date))+
#   geom_line(aes(y = VOL1), linetype = "solid", color = "#3E90BF")+
# p99
p1 <- ggplot(fig5TC , aes(x = date)) +
  # 添加散点图层，指定y轴为obs_9，设置点的形状(shape)和颜色(color)
  geom_point(aes(y = TC.x), shape = 1, color = "#ED8D5A",stroke = 1) +
  # 添加折线图层，指定y轴为sim_9，设置线的类型和颜色
  geom_line(aes(y = TC.y), linetype = "solid", color = "#3E90BF") +
  theme_light() +
  ylab("riv5-tc")+
  scale_y_continuous(expand = c(0,0),limits = y)+
  scale_x_date(limits = as.Date(c("2020-12-31", "2022-01-01")),
               date_breaks = "5 month",
               date_labels = "%Y-%m")+
  theme(axis.text = element_text(size = 11.5,colour="black",family = "Times New Roman"),
        text =element_text(size = 12,colour="black",family = "Times New Roman"))+
  annotate("text", x = as.Date("2021-9-1","%Y-%m-%d"), y = 20,
           label = sprintf("rmse = %.2f\nrsq = %.3f\npbais=%.2f%%\nnse=%.2f", RMSE_tc5,Rsq_tc5,PBAIS_tc5,NSE_tc5 ),
           hjust = 0, vjust = 0, size = 6,family = "Times New Roman") 

p2 <- ggplot(fig5DXC , aes(x = date)) +
  # 添加散点图层，指定y轴为obs_9，设置点的形状(shape)和颜色(color)
  geom_point(aes(y = DXC.x), shape = 1, color = "#ED8D5A",stroke = 1) +
  # 添加折线图层，指定y轴为sim_9，设置线的类型和颜色
  geom_line(aes(y = DXC.y), linetype = "solid", color = "#3E90BF") +
  theme_light() +
  ylab("riv5-dxc")+
  scale_y_continuous(expand = c(0,0),limits = y)+
  scale_x_date(limits = as.Date(c("2020-12-31", "2022-01-01")),
               date_breaks = "5 month",
               date_labels = "%Y-%m")+
  theme(axis.text = element_text(size = 11.5,colour="black",family = "Times New Roman"),
        text =element_text(size = 12,colour="black",family = "Times New Roman"))+
  annotate("text", x = as.Date("2021-9-1","%Y-%m-%d"), y = 20,
           label = sprintf("rmse = %.2f\nrsq = %.3f\npbais=%.2f%%\nnse=%.2f", RMSE_dxc5,Rsq_dxc5,PBAIS_dxc5,NSE_dxc5 ),
           hjust = 0, vjust = 0, size = 6,family = "Times New Roman") 

p3 <- ggplot(fig5OTC , aes(x = date)) +
  # 添加散点图层，指定y轴为obs_9，设置点的形状(shape)和颜色(color)
  geom_point(aes(y = OTC.x), shape = 1, color = "#ED8D5A",stroke = 1) +
  # 添加折线图层，指定y轴为sim_9，设置线的类型和颜色
  geom_line(aes(y = OTC.y), linetype = "solid", color = "#3E90BF") +
  theme_light() +
  ylab("riv5-otc")+
  scale_y_continuous(expand = c(0,0),limits = y)+
  scale_x_date(limits = as.Date(c("2020-12-31", "2022-01-01")),
               date_breaks = "5 month",
               date_labels = "%Y-%m")+
  theme(axis.text = element_text(size = 11.5,colour="black",family = "Times New Roman"),
        text =element_text(size = 12,colour="black",family = "Times New Roman"))+
  annotate("text", x = as.Date("2021-9-1","%Y-%m-%d"), y = 20,
           label = sprintf("rmse = %.2f\nrsq = %.3f\npbais=%.2f%%\nnse=%.2f", RMSE_otc5,Rsq_otc5,PBAIS_otc5,NSE_otc5 ),
           hjust = 0, vjust = 0, size = 6,family = "Times New Roman") 

p4 <- ggplot(fig5CTC , aes(x = date)) +
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

f5 <- cowplot::plot_grid(p1, p2, p3,p4,nrow = 2,
                         labels = LETTERS[1:4])

p5 <- ggplot(fig7TC , aes(x = date)) +
  # 添加散点图层，指定y轴为obs_9，设置点的形状(shape)和颜色(color)
  geom_point(aes(y = TC.x), shape = 1, color = "#ED8D5A",stroke = 1) +
  # 添加折线图层，指定y轴为sim_9，设置线的类型和颜色
  geom_line(aes(y = TC.y), linetype = "solid", color = "#3E90BF") +
  theme_light() +
  ylab("riv7-tc")+
  scale_y_continuous(expand = c(0,0),limits = y)+
  scale_x_date(limits = as.Date(c("2020-12-31", "2022-01-01")),
               date_breaks = "5 month",
               date_labels = "%Y-%m")+
  theme(axis.text = element_text(size = 11.5,colour="black",family = "Times New Roman"),
        text =element_text(size = 12,colour="black",family = "Times New Roman"))+
  annotate("text", x = as.Date("2021-9-1","%Y-%m-%d"), y = 20,
           label = sprintf("rmse = %.2f\nrsq = %.3f\npbais=%.2f%%\nnse=%.2f", RMSE_tc7,Rsq_tc7,PBAIS_tc7,NSE_tc7 ),
           hjust = 0, vjust = 0, size = 6,family = "Times New Roman") 

p6 <- ggplot(fig7DXC , aes(x = date)) +
  # 添加散点图层，指定y轴为obs_9，设置点的形状(shape)和颜色(color)
  geom_point(aes(y = DXC.x), shape = 1, color = "#ED8D5A",stroke = 1) +
  # 添加折线图层，指定y轴为sim_9，设置线的类型和颜色
  geom_line(aes(y = DXC.y), linetype = "solid", color = "#3E90BF") +
  theme_light() +
  ylab("riv7-dxc")+
  scale_y_continuous(expand = c(0,0),limits = y)+
  scale_x_date(limits = as.Date(c("2020-12-31", "2022-01-01")),
               date_breaks = "5 month",
               date_labels = "%Y-%m")+
  theme(axis.text = element_text(size = 11.5,colour="black",family = "Times New Roman"),
        text =element_text(size = 12,colour="black",family = "Times New Roman"))+
  annotate("text", x = as.Date("2021-9-1","%Y-%m-%d"), y = 20,
           label = sprintf("rmse = %.2f\nrsq = %.3f\npbais=%.2f%%\nnse=%.2f", RMSE_dxc7,Rsq_dxc7,PBAIS_dxc7,NSE_dxc7 ),
           hjust = 0, vjust = 0, size = 6,family = "Times New Roman") 

p7 <- ggplot(fig7OTC , aes(x = date)) +
  # 添加散点图层，指定y轴为obs_9，设置点的形状(shape)和颜色(color)
  geom_point(aes(y = OTC.x), shape = 1, color = "#ED8D5A",stroke = 1) +
  # 添加折线图层，指定y轴为sim_9，设置线的类型和颜色
  geom_line(aes(y = OTC.y), linetype = "solid", color = "#3E90BF") +
  theme_light() +
  ylab("riv7-otc")+
  scale_y_continuous(expand = c(0,0),limits = y)+
  scale_x_date(limits = as.Date(c("2020-12-31", "2022-01-01")),
               date_breaks = "5 month",
               date_labels = "%Y-%m")+
  theme(axis.text = element_text(size = 11.5,colour="black",family = "Times New Roman"),
        text =element_text(size = 12,colour="black",family = "Times New Roman"))+
  annotate("text", x = as.Date("2021-9-1","%Y-%m-%d"), y = 20,
           label = sprintf("rmse = %.2f\nrsq = %.3f\npbais=%.2f%%\nnse=%.2f", RMSE_otc7,Rsq_otc7,PBAIS_otc7,NSE_otc7 ),
           hjust = 0, vjust = 0, size = 6,family = "Times New Roman") 

p8 <- ggplot(fig7CTC , aes(x = date)) +
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

f7 <- cowplot::plot_grid(p5, p6, p7,p8,nrow = 2,
                         labels = LETTERS[1:4])

f<- cowplot::plot_grid(f5, f7, nrow = 1)
f
