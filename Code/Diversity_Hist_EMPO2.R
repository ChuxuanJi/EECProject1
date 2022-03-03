rm(list=ls())
#加载所需要的包
library(vegan)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(splines)
library(gridExtra)
library(ggpmisc)
library(gcookbook)
#作图

#Chao1 Hist EMPO2
#1.画一个All Sample多样性的直方图
Chao_all <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_empo2.txt',header = T)
Chao_all_hist <- ggplot(data = Chao_all, aes(x=Chao1, fill=Location))+
  geom_histogram(bins = 150)+
  labs(x='Chao1 Index',y="Number")+
  scale_fill_manual(values=c( "#B28FCE", "#56B4E9", "#009E73", "#0072B2"))+
  theme_bw()+theme(panel.grid=element_blank())+
  #scale_x_continuous(breaks=seq(0, 7000, 100))+
  ggtitle("All Sample") + theme(plot.title = element_text(hjust = 0.5))+
  geom_rug()
Chao_all_hist

#2.画一个Non-Saline多样性的直方图
Chao_ns <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_ns.txt',header = T)
Chao_ns_hist <- ggplot(data = Chao_ns, aes(x=Chao1, fill=Location))+
  geom_histogram(bins = 150)+
  labs(x='Chao1 Index',y="Number")+
  scale_fill_manual(values=c( "#999999",  "#43341B","#E69F00", "#82663A", "#7DB9DE"))+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Chao_ns_hist

#3.画一个Saline多样性的直方图
Chao_s <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_s.txt',header = T)
Chao_s_hist <- ggplot(data = Chao_s, aes(x=Chao1, fill=Location))+
  geom_histogram(bins = 150)+
  labs(x='Chao1 Index',y="Number")+
  scale_fill_manual(values=c( "#999999", "#78552B", "#D7B98E", "#113285"))+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Chao_s_hist

#4.画一个Plant多样性的直方图
Chao_p <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_p.txt',header = T)
Chao_p_hist <- ggplot(data = Chao_p, aes(x=Chao1, fill=Location))+
  geom_histogram(bins = 150)+
  labs(x='Chao1 Index',y="Number")+
  scale_fill_manual(values=c("#B5CAA0", "#8D742A", "#4B4E2A"))+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Plant") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Chao_p_hist

#5.画一个Animal多样性的直方图
Chao_ani <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_ani.txt',header = T)
Chao_ani_hist <- ggplot(data = Chao_ani, aes(x=Chao1, fill=Location))+
  geom_histogram(bins = 150)+
  labs(x='Chao1 Index',y="Number")+
  scale_fill_manual(values=c("#3F2B36", "#9B6E23", "#FAD689","#CA7A2C", "#B28FCE"))+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Animal") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Chao_ani_hist

grid.arrange(arrangeGrob(Chao_all_hist, ncol=1),arrangeGrob(Chao_ns_hist, Chao_s_hist, ncol=2), arrangeGrob(Chao_p_hist, Chao_ani_hist, ncol = 2), nrow=3)


#Observed OTUs Hist
#1.画一个All Sample多样性的直方图
OO_all <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_empo2.txt',header = T)
OO_all_hist <- ggplot(data = OO_all, aes(x=OO, fill=Location))+
  geom_histogram(bins = 150)+
  labs(x='Observed OTUs',y="Number")+
  scale_fill_manual(values=c( "#B28FCE", "#56B4E9", "#009E73", "#0072B2"))+
  theme_bw()+theme(panel.grid=element_blank())+
  #scale_x_continuous(breaks=seq(0, 7000, 100))+
  ggtitle("All Sample") + theme(plot.title = element_text(hjust = 0.5))+
  geom_rug()
OO_all_hist

#2.画一个Non-Saline多样性的直方图
OO_ns <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_ns.txt',header = T)
OO_ns_hist <- ggplot(data = OO_ns, aes(x=OO, fill=Location))+
  geom_histogram(bins = 150)+
  labs(x='Observed OTUs',y="Number")+
  scale_fill_manual(values=c( "#999999",  "#43341B","#E69F00", "#82663A", "#7DB9DE"))+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
OO_ns_hist

#3.画一个Saline多样性的直方图
OO_s <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_s.txt',header = T)
OO_s_hist <- ggplot(data = OO_s, aes(x=OO, fill=Location))+
  geom_histogram(bins = 150)+
  labs(x='Observed OTUs',y="Number")+
  scale_fill_manual(values=c( "#999999", "#78552B", "#D7B98E", "#113285"))+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
OO_s_hist

#4.画一个Plant多样性的直方图
OO_p <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_p.txt',header = T)
OO_p_hist <- ggplot(data = OO_p, aes(x=OO, fill=Location))+
  geom_histogram(bins = 150)+
  labs(x='Observed OTUs',y="Number")+
  scale_fill_manual(values=c("#B5CAA0", "#8D742A", "#4B4E2A"))+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Plant") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
OO_p_hist

#5.画一个Animal多样性的直方图
OO_ani <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_ani.txt',header = T)
OO_ani_hist <- ggplot(data = OO_ani, aes(x=OO, fill=Location))+
  geom_histogram(bins = 150)+
  labs(x='Observed OTUs',y="Number")+
  scale_fill_manual(values=c("#3F2B36", "#9B6E23", "#FAD689","#CA7A2C", "#B28FCE"))+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Animal") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
OO_ani_hist

grid.arrange(arrangeGrob(OO_all_hist, ncol=1),arrangeGrob(OO_ns_hist, OO_s_hist, ncol=2), arrangeGrob(OO_p_hist, OO_ani_hist, ncol = 2), nrow=3)


#Shannon Index Hist
#1.画一个All Sample多样性的直方图
Shan_all <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_empo2.txt',header = T)
Shan_all_hist <- ggplot(data = Shan_all, aes(x=Shannon.Index, fill=Location))+
  geom_histogram(bins = 150)+
  labs(x='Shannon Index',y="Number")+
  scale_fill_manual(values=c( "#B28FCE", "#56B4E9", "#009E73", "#0072B2"))+
  theme_bw()+theme(panel.grid=element_blank())+
  #scale_x_continuous(breaks=seq(0, 7000, 100))+
  ggtitle("All Sample") + theme(plot.title = element_text(hjust = 0.5))+
  geom_rug()
Shan_all_hist

#2.画一个Non-Saline多样性的直方图
Shan_ns <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_ns.txt',header = T)
Shan_ns_hist <- ggplot(data = Shan_ns, aes(x=Shannon.Index, fill=Location))+
  geom_histogram(bins = 150)+
  labs(x='Shannon Index',y="Number")+
  scale_fill_manual(values=c( "#999999",  "#43341B","#E69F00", "#82663A", "#7DB9DE"))+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Shan_ns_hist

#3.画一个Saline多样性的直方图
Shan_s <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_s.txt',header = T)
Shan_s_hist <- ggplot(data = Shan_s, aes(x=Shannon.Index, fill=Location))+
  geom_histogram(bins = 150)+
  labs(x='Shannon Index',y="Number")+
  scale_fill_manual(values=c( "#999999", "#78552B", "#D7B98E", "#113285"))+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Shan_s_hist

#4.画一个Plant多样性的直方图
Shan_p <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_p.txt',header = T)
Shan_p_hist <- ggplot(data = Shan_p, aes(x=Shannon.Index, fill=Location))+
  geom_histogram(bins = 150)+
  labs(x='Shannon Index',y="Number")+
  scale_fill_manual(values=c("#B5CAA0", "#8D742A", "#4B4E2A"))+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Plant") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Shan_p_hist

#5.画一个Animal多样性的直方图
Shan_ani <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_ani.txt',header = T)
Shan_ani_hist <- ggplot(data = Shan_ani, aes(x=Shannon.Index, fill=Location))+
  geom_histogram(bins = 150)+
  labs(x='Shannon Index',y="Number")+
  scale_fill_manual(values=c("#3F2B36", "#9B6E23", "#FAD689","#CA7A2C", "#B28FCE"))+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Animal") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Shan_ani_hist

grid.arrange(arrangeGrob(Shan_all_hist, ncol=1),arrangeGrob(Shan_ns_hist, Shan_s_hist, ncol=2), arrangeGrob(Shan_p_hist, Shan_ani_hist, ncol = 2), nrow=3)
