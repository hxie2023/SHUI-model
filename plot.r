cwd <- "/home/smq/code/1/tyh_20240406"
setwd(cwd)
rm(list = ls())
install.packages("tidyverse") # 安装包
install.packages("networkD3") # 安装包
library(tidyverse) # 加载包 
library(networkD3) 

links <- read.table(file.path(cwd,"path.txt"),header = T)

links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1 
links %>% head(3) 