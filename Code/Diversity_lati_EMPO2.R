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

#Chao1 Latitude EMPO2
#latitude和Chao1 Index的Non-Saline散点图
Chao_lati_all <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_all.txt',header = T)
Chao_lati_all_plot <- ggplot()+
  geom_point(data = Chao_lati_all,aes(x=Latitude, y=Chao1,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Chao1 Index")+
  scale_color_manual(values=c( "#B28FCE", "#56B4E9", "#009E73", "#0072B2"))+
  geom_smooth(data = Chao_lati_all,aes(x=Latitude, y=Chao1), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("All Samples") + theme(plot.title = element_text(hjust = 0.5))
#+theme(legend.position="none")
Chao_lati_all_plot

#latitude和Chao1 Index的Non-Saline散点图
Chao_lati_ns <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_ns.txt',header = T)
Chao_lati_ns_plot <- ggplot()+
  geom_point(data = Chao_lati_ns,aes(x=Latitude, y=Chao1,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Chao1 Index")+
  scale_color_manual(values=c( "#999999",  "#43341B","#E69F00", "#82663A", "#7DB9DE"))+
  geom_smooth(data = Chao_lati_ns,aes(x=Latitude, y=Chao1), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
Chao_lati_ns_plot

#latitude和Chao1 Index的Saline散点图
Chao_lati_s <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_s.txt',header = T)
Chao_lati_s_plot <- ggplot()+
  geom_point(data = Chao_lati_s,aes(x=Latitude, y=Chao1,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Chao1 Index")+
  scale_color_manual(values=c( "#999999", "#78552B", "#D7B98E", "#113285"))+
  geom_smooth(data = Chao_lati_s,aes(x=Latitude, y=Chao1), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
Chao_lati_s_plot

#latitude和Chao1 Index的Plant散点图
Chao_lati_p <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_p.txt',header = T)
Chao_lati_p_plot <- ggplot()+
  geom_point(data = Chao_lati_p,aes(x=Latitude, y=Chao1,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Chao1 Index")+
  scale_color_manual(values=c("#B5CAA0", "#8D742A", "#4B4E2A"))+
  geom_smooth(data = Chao_lati_p,aes(x=Latitude, y=Chao1), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Plant") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
Chao_lati_p_plot

#latitude和Chao1 Index的Animal散点图
Chao_lati_ani <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_ani.txt',header = T)
Chao_lati_ani_plot <- ggplot()+
  geom_point(data = Chao_lati_ani,aes(x=Latitude, y=Chao1,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Chao1 Index")+
  scale_color_manual(values=c("#3F2B36", "#9B6E23", "#FAD689","#CA7A2C", "#B28FCE"))+
  geom_smooth(data = Chao_lati_ani,aes(x=Latitude, y=Chao1), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Animal") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
Chao_lati_ani_plot

grid.arrange(arrangeGrob(Chao_lati_all_plot, ncol=1), arrangeGrob(Chao_lati_ns_plot, Chao_lati_s_plot, ncol=2), arrangeGrob(Chao_lati_p_plot, Chao_lati_ani_plot, ncol = 2), nrow=3)


#Observed OTUs Latitude EMPO2
#latitude和Observed OTUs的All散点图
OO_lati_all <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_all.txt',header = T)
OO_lati_all_plot <- ggplot()+
  geom_point(data = OO_lati_all,aes(x=Latitude, y=OO,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Observed OTUs")+
  scale_color_manual(values=c( "#B28FCE", "#56B4E9", "#009E73", "#0072B2"))+
  geom_smooth(data = OO_lati_all,aes(x=Latitude, y=OO), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("All Samples") + theme(plot.title = element_text(hjust = 0.5))
#+theme(legend.position="none")
OO_lati_all_plot

#latitude和Observed OTUs的Non-Saline散点图
OO_lati_ns <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_ns.txt',header = T)
OO_lati_ns_plot <- ggplot()+
  geom_point(data = OO_lati_ns,aes(x=Latitude, y=OO,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Observed OTUs")+
  scale_color_manual(values=c( "#999999",  "#43341B","#E69F00", "#82663A", "#7DB9DE"))+
  geom_smooth(data = OO_lati_ns,aes(x=Latitude, y=OO), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
OO_lati_ns_plot

#latitude和Observed OTUs的Saline散点图
OO_lati_s <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_s.txt',header = T)
OO_lati_s_plot <- ggplot()+
  geom_point(data = OO_lati_s,aes(x=Latitude, y=OO,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Observed OTUs")+
  scale_color_manual(values=c( "#999999", "#78552B", "#D7B98E", "#113285"))+
  geom_smooth(data = OO_lati_s,aes(x=Latitude, y=OO), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
OO_lati_s_plot

#latitude和Observed OTUs的Plant散点图
OO_lati_p <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_p.txt',header = T)
OO_lati_p_plot <- ggplot()+
  geom_point(data = OO_lati_p,aes(x=Latitude, y=OO,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Observed OTUs")+
  scale_color_manual(values=c("#B5CAA0", "#8D742A", "#4B4E2A"))+
  geom_smooth(data = OO_lati_p,aes(x=Latitude, y=OO), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Plant") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
OO_lati_p_plot

#latitude和Observed OTUs的Animal散点图
OO_lati_ani <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_ani.txt',header = T)
OO_lati_ani_plot <- ggplot()+
  geom_point(data = OO_lati_ani,aes(x=Latitude, y=OO,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Observed OTUs")+
  scale_color_manual(values=c("#3F2B36", "#9B6E23", "#FAD689","#CA7A2C", "#B28FCE"))+
  geom_smooth(data = OO_lati_ani,aes(x=Latitude, y=OO), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Animal") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
OO_lati_ani_plot

grid.arrange(arrangeGrob(OO_lati_all_plot, ncol=1),arrangeGrob(OO_lati_ns_plot, OO_lati_s_plot, ncol=2), arrangeGrob(OO_lati_p_plot, OO_lati_ani_plot, ncol = 2), nrow=3)


#Shannon Latitude EMPO2
#latitude和Shannon Index的All散点图
Shan_lati_all <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_all.txt',header = T)
Shan_lati_all_plot <- ggplot()+
  geom_point(data = Shan_lati_all,aes(x=Latitude, y=Shannon,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Shannon Index")+
  scale_color_manual(values=c( "#B28FCE", "#56B4E9", "#009E73", "#0072B2"))+
  geom_smooth(data = Shan_lati_all,aes(x=Latitude, y=Shannon), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("All Samples") + theme(plot.title = element_text(hjust = 0.5))
#+theme(legend.position="none")
Shan_lati_all_plot

#latitude和Shannon Index的Non-Saline散点图
shan_lati_ns <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_ns.txt',header = T)
shan_lati_ns_plot <- ggplot()+
  geom_point(data = shan_lati_ns,aes(x=Latitude, y=Shannon.Index,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Shannon Index")+
  scale_color_manual(values=c( "#999999",  "#43341B","#E69F00", "#82663A", "#7DB9DE"))+
  geom_smooth(data = shan_lati_ns,aes(x=Latitude, y=Shannon.Index), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
shan_lati_ns_plot

#latitude和Shannon Index的Saline散点图
shan_lati_s <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_s.txt',header = T)
shan_lati_s_plot <- ggplot()+
  geom_point(data = shan_lati_s,aes(x=Latitude, y=Shannon.Index,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Shannon Index")+
  scale_color_manual(values=c( "#999999", "#78552B", "#D7B98E", "#113285"))+
  geom_smooth(data = shan_lati_s,aes(x=Latitude, y=Shannon.Index), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
shan_lati_s_plot

#latitude和Shannon Index的Plant散点图
shan_lati_p <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_p.txt',header = T)
shan_lati_p_plot <- ggplot()+
  geom_point(data = shan_lati_p,aes(x=Latitude, y=Shannon.Index,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Shannon Index")+
  scale_color_manual(values=c("#B5CAA0", "#8D742A", "#4B4E2A"))+
  geom_smooth(data = shan_lati_p,aes(x=Latitude, y=Shannon.Index), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Plant") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
shan_lati_p_plot

#latitude和Shannon Index的Animal散点图
shan_lati_ani <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_ani.txt',header = T)
shan_lati_ani_plot <- ggplot()+
  geom_point(data = shan_lati_ani,aes(x=Latitude, y=Shannon.Index,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Shannon Index")+
  scale_color_manual(values=c("#3F2B36", "#9B6E23", "#FAD689","#CA7A2C", "#B28FCE"))+
  geom_smooth(data = shan_lati_ani,aes(x=Latitude, y=Shannon.Index), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Animal") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")
#+theme(legend.position="none")
shan_lati_ani_plot

grid.arrange(arrangeGrob(Shan_lati_all_plot, ncol=1),arrangeGrob(shan_lati_ns_plot, shan_lati_s_plot, ncol=2), arrangeGrob(shan_lati_p_plot, shan_lati_ani_plot, ncol = 2), nrow=3)
