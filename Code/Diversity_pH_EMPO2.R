rm(list=ls())
#利用Chao1 Index和环境因素做散点图
#加载所需要的包
library(vegan)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(splines)
library(gridExtra)
#作图

#Chao1 pH EMPO2
#pH和Chao1 Index的Non-Saline散点图
Chao_pH_ns <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_pH_ns.txt',header = T)
Chao_pH_ns_plot <- ggplot()+
  geom_point(data = Chao_pH_ns,aes(x=pH, y=Chao1,color = Location), size=1, shape=21)+
  labs(x='pH',y="Chao1 Index")+
  scale_color_manual(values=c( "#999999",  "#43341B","#E69F00", "#82663A", "#7DB9DE"))+
  geom_smooth(data = Chao_pH_ns,aes(x=pH, y=Chao1), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
Chao_pH_ns_plot

#pH和Chao1 Index的Saline散点图
Chao_pH_s <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_pH_s.txt',header = T)
Chao_pH_s_plot <- ggplot()+
  geom_point(data = Chao_pH_s,aes(x=pH, y=Chao1,color = Location), size=1, shape=21)+
  labs(x='pH',y="Chao1 Index")+
  scale_color_manual(values=c( "#999999", "#78552B", "#D7B98E", "#113285"))+
  geom_smooth(data = Chao_pH_s,aes(x=pH, y=Chao1), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
Chao_pH_s_plot

#pH和Chao1 Index的All散点图
Chao_pH_all <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_pH_all.txt',header = T)
Chao_pH_all_plot <- ggplot()+
  geom_point(data = Chao_pH_all,aes(x=pH, y=Chao1,color = Location), size=1, shape=21)+
  labs(x='pH',y="Chao1 Index")+
  scale_color_manual(values=c( "#B28FCE", "#56B4E9", "#009E73", "#0072B2"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = Chao_pH_all,aes(x=pH, y=Chao1), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("All Samples") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
Chao_pH_all_plot

grid.arrange(arrangeGrob(Chao_pH_all_plot), arrangeGrob(Chao_pH_ns_plot, Chao_pH_s_plot, ncol = 2), nrow=2)

#Observed OTUs pH EMPO2
#pH和Observed OTUs的Non-Saline散点图
OO_pH_ns <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_pH_ns.txt',header = T)
OO_pH_ns_plot <- ggplot()+
  geom_point(data = OO_pH_ns,aes(x=pH, y=OO,color = Location), size=1, shape=21)+
  labs(x='pH',y="Observed OTUs")+
  scale_color_manual(values=c( "#999999",  "#43341B","#E69F00", "#82663A", "#7DB9DE"))+
  geom_smooth(data = OO_pH_ns,aes(x=pH, y=OO), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
OO_pH_ns_plot

#pH和Observed OTUs的Saline散点图
OO_pH_s <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_pH_s.txt',header = T)
OO_pH_s_plot <- ggplot()+
  geom_point(data = OO_pH_s,aes(x=pH, y=OO,color = Location), size=1, shape=21)+
  labs(x='pH',y="Observed OTUs")+
  scale_color_manual(values=c( "#999999", "#78552B", "#D7B98E", "#113285"))+
  geom_smooth(data = OO_pH_s,aes(x=pH, y=OO), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
OO_pH_s_plot

#pH和Observed OTUs的All散点图
OO_pH_all <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_pH_all.txt',header = T)
OO_pH_all_plot <- ggplot()+
  geom_point(data = OO_pH_all,aes(x=pH, y=OO,color = Location), size=1, shape=21)+
  labs(x='pH',y="Observed OTUs")+
  scale_color_manual(values=c( "#B28FCE", "#56B4E9", "#009E73", "#0072B2"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = OO_pH_all,aes(x=pH, y=OO), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("All Samples") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
OO_pH_all_plot

grid.arrange(arrangeGrob(OO_pH_all_plot), arrangeGrob(OO_pH_ns_plot, OO_pH_s_plot, ncol = 2), nrow=2)


#Shannon Index pH EMPO2
#pH和Shannon Index的Non-Saline散点图
shan_pH_ns <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_pH_ns.txt',header = T)
shan_pH_ns_plot <- ggplot()+
  geom_point(data = shan_pH_ns,aes(x=pH, y=Shannon.Index,color = Location), size=1, shape=21)+
  labs(x='pH',y="Shannon Index")+
  scale_color_manual(values=c( "#999999",  "#43341B","#E69F00", "#82663A", "#7DB9DE"))+
  geom_smooth(data = shan_pH_ns,aes(x=pH, y=Shannon.Index), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
shan_pH_ns_plot

#pH和Shannon Index的Saline散点图
shan_pH_s <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_pH_s.txt',header = T)
shan_pH_s_plot <- ggplot()+
  geom_point(data = shan_pH_s,aes(x=pH, y=Shannon.Index,color = Location), size=1, shape=21)+
  labs(x='pH',y="Shannon Index")+
  scale_color_manual(values=c( "#999999", "#78552B", "#D7B98E", "#113285"))+
  geom_smooth(data = shan_pH_s,aes(x=pH, y=Shannon.Index), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
shan_pH_s_plot

#pH和Shannon Index的All散点图
shan_pH_all <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_pH_all.txt',header = T)
shan_pH_all_plot <- ggplot()+
  geom_point(data = shan_pH_all,aes(x=pH, y=Shannon.Index,color = Location), size=1, shape=21)+
  labs(x='pH',y="Shannon Index")+
  scale_color_manual(values=c( "#B28FCE", "#56B4E9", "#009E73", "#0072B2"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = shan_pH_all,aes(x=pH, y=Shannon.Index), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("All Samples") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
shan_pH_all_plot

grid.arrange(arrangeGrob(shan_pH_all_plot), arrangeGrob(shan_pH_ns_plot, shan_pH_s_plot, ncol = 2), nrow=2)
