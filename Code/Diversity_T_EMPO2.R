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

#Chao1 Temperature EMPO2
#Temperature和Chao1 Index的EMPO2散点图
Chao_T_empo2 <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_T_empo2.txt',header = T)
Chao_T_empo2_plot <- ggplot()+
  geom_point(data = Chao_T_empo2,aes(x=Temperature, y=Chao1,color = Location), size=1, shape=21)+
  labs(x='Temperature',y="Chao1 Index")+
  scale_color_manual(values=c( "#B28FCE", "#56B4E9", "#009E73", "#0072B2"))+
  geom_smooth(data = Chao_T_empo2,aes(x=Temperature, y=Chao1), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("All Samples") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
Chao_T_empo2_plot

#Temperature和Chao1 Index的Non-Saline散点图
Chao_T_ns <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_T_ns.txt',header = T)
Chao_T_ns_plot <- ggplot()+
  geom_point(data = Chao_T_ns,aes(x=Temperature, y=Chao1,color = Location), size=1, shape=21)+
  labs(x='Temperature',y="Chao1 Index")+
  scale_color_manual(values=c( "#999999",  "#43341B","#E69F00", "#82663A", "#7DB9DE"))+
  geom_smooth(data = Chao_T_ns,aes(x=Temperature, y=Chao1), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
Chao_T_ns_plot

#Temperature和Chao1 Index的Saline散点图
Chao_T_s <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_T_s.txt',header = T)
Chao_T_s_plot <- ggplot()+
  geom_point(data = Chao_T_s,aes(x=Temperature, y=Chao1,color = Location), size=1, shape=21)+
  labs(x='Temperature',y="Chao1 Index")+
  scale_color_manual(values=c( "#999999", "#78552B", "#D7B98E", "#113285"))+
  geom_smooth(data = Chao_T_s,aes(x=Temperature, y=Chao1), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
Chao_T_s_plot

#Temperature和Chao1 Index的Plant散点图
Chao_T_p <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_T_p.txt',header = T)
Chao_T_p_plot <- ggplot()+
  geom_point(data = Chao_T_p,aes(x=Temperature, y=Chao1,color = Location), size=1, shape=21)+
  labs(x='Temperature',y="Chao1 Index")+
  scale_color_manual(values=c("#B5CAA0", "#8D742A", "#4B4E2A"))+
  geom_smooth(data = Chao_T_p,aes(x=Temperature, y=Chao1), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Plant") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
Chao_T_p_plot

ggarrange(Chao_T_empo2_plot, Chao_T_ns_plot,Chao_T_s_plot, Chao_T_p_plot,ncol = 2, nrow = 2,
          labels = c("A","B","C","D"), # 添加标签
          font.label = list(size = 14))


#Observed OTUs Temperature EMPO2
#Temperature和Observed OTUs的EMPO2散点图
OO_T_empo2 <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_T_empo2.txt',header = T)
OO_T_empo2_plot <- ggplot()+
  geom_point(data = OO_T_empo2,aes(x=Temperature, y=OO,color = Location), size=0.8, shape=21)+
  labs(x='Temperature',y="Observed OTUs")+
  scale_color_manual(values=c( "#B28FCE", "#56B4E9", "#009E73", "#0072B2"))+
  geom_smooth(data = OO_T_empo2,aes(x=Temperature, y=OO), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("All Samples") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
OO_T_empo2_plot

#Temperature和Observed OTUs的Non-Saline散点图
OO_T_ns <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_T_ns.txt',header = T)
OO_T_ns_plot <- ggplot()+
  geom_point(data = OO_T_ns,aes(x=Temperature, y=OO,color = Location), size=0.8, shape=21)+
  labs(x='Temperature',y="Observed OTUs")+
  scale_color_manual(values=c( "#999999",  "#43341B","#E69F00", "#82663A", "#7DB9DE"))+
  geom_smooth(data = OO_T_ns,aes(x=Temperature, y=OO), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
OO_T_ns_plot

#Temperature和Observed OTUs的Saline散点图
OO_T_s <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_T_s.txt',header = T)
OO_T_s_plot <- ggplot()+
  geom_point(data = OO_T_s,aes(x=Temperature, y=OO,color = Location), size=0.8, shape=21)+
  labs(x='Temperature',y="Observed OTUs")+
  scale_color_manual(values=c( "#999999", "#78552B", "#D7B98E", "#113285"))+
  geom_smooth(data = OO_T_s,aes(x=Temperature, y=OO), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
OO_T_s_plot

#Temperature和Observed OTUs的Plant散点图
OO_T_p <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_T_p.txt',header = T)
OO_T_p_plot <- ggplot()+
  geom_point(data = OO_T_p,aes(x=Temperature, y=OO,color = Location), size=0.8, shape=21)+
  labs(x='Temperature',y="Observed OTUs")+
  scale_color_manual(values=c("#B5CAA0", "#8D742A", "#4B4E2A"))+
  geom_smooth(data = OO_T_p,aes(x=Temperature, y=OO), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Plant") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
OO_T_p_plot

ggarrange(OO_T_empo2_plot, OO_T_ns_plot,OO_T_s_plot, OO_T_p_plot,ncol = 2, nrow = 2,
          labels = c("A","B","C","D"), # 添加标签
          font.label = list(size = 14))


#Shannon Index Temperature EMPO2
#Temperature和Shannon Index的EMPO2散点图
shan_T_empo2 <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_T_empo2.txt',header = T)
shan_T_empo2_plot <- ggplot()+
  geom_point(data = shan_T_empo2,aes(x=Temperature, y=Shannon.Index,color = Location), size=0.8, shape=21)+
  labs(x='Temperature',y="Shannon Index")+
  scale_color_manual(values=c( "#B28FCE", "#56B4E9", "#009E73", "#0072B2"))+
  geom_smooth(data = shan_T_empo2,aes(x=Temperature, y=Shannon.Index), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("All Samples") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
shan_T_empo2_plot

#Temperature和Shannon Index的Non-Saline散点图
shan_T_ns <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_T_ns.txt',header = T)
shan_T_ns_plot <- ggplot()+
  geom_point(data = shan_T_ns,aes(x=Temperature, y=Shannon.Index,color = Location), size=0.8, shape=21)+
  labs(x='Temperature',y="Shannon Index")+
  scale_color_manual(values=c( "#999999",  "#43341B","#E69F00", "#82663A", "#7DB9DE"))+
  geom_smooth(data = shan_T_ns,aes(x=Temperature, y=Shannon.Index), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
shan_T_ns_plot

#Temperature和Shannon Index的Saline散点图
shan_T_s <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_T_s.txt',header = T)
shan_T_s_plot <- ggplot()+
  geom_point(data = shan_T_s,aes(x=Temperature, y=Shannon.Index,color = Location), size=0.8, shape=21)+
  labs(x='Temperature',y="Shannon Index")+
  scale_color_manual(values=c( "#999999", "#78552B", "#D7B98E", "#113285"))+
  geom_smooth(data = shan_T_s,aes(x=Temperature, y=Shannon.Index), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
shan_T_s_plot

#Temperature和Shannon Index的Plant散点图
shan_T_p <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_T_p.txt',header = T)
shan_T_p_plot <- ggplot()+
  geom_point(data = shan_T_p,aes(x=Temperature, y=Shannon.Index,color = Location), size=0.8, shape=21)+
  labs(x='Temperature',y="Shannon Index")+
  scale_color_manual(values=c("#B5CAA0", "#8D742A", "#4B4E2A"))+
  geom_smooth(data = shan_T_p,aes(x=Temperature, y=Shannon.Index), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Plant") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
shan_T_p_plot

ggarrange(shan_T_empo2_plot, shan_T_ns_plot,shan_T_s_plot, shan_T_p_plot,ncol = 2, nrow = 2,
          labels = c("A","B","C","D"), # 添加标签
          font.label = list(size = 14))
