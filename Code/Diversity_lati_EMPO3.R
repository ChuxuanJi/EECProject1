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

mycol<-c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Chao1 Latitude EMPO3
#Non-Saline Samples

#1.latitude和Chao1 Index的Soil散点图
Chao_lati_soil <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_soil.txt',header = T)
Chao_lati_soil_plot <- ggplot()+
  geom_point(data = Chao_lati_soil,aes(x=Latitude, y=Chao1,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Chao1 Index")+
  scale_color_manual(values=c("#E69F00"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = Chao_lati_soil,aes(x=Latitude, y=Chao1), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Soil") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
Chao_lati_soil_plot


#2.latitude和Chao1 Index的Non-Saline-sedi散点图(不好看)
Chao_lati_ns_sedi <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_ns_sedi.txt',header = T)
Chao_lati_ns_sedi_plot <- ggplot()+
  geom_point(data = Chao_lati_ns_sedi,aes(x=Latitude, y=Chao1,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Chao1 Index")+
  scale_color_manual(values=c("#999999"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = Chao_lati_ns_sedi,aes(x=Latitude, y=Chao1), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline Sediment") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
Chao_lati_ns_sedi_plot

#3.latitude和Chao1 Index的Non-Saline-surf散点图(不好看)
Chao_lati_ns_surf <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_ns_surf.txt',header = T)
Chao_lati_ns_surf_plot <- ggplot()+
  geom_point(data = Chao_lati_ns_surf,aes(x=Latitude, y=Chao1,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Chao1 Index")+
  scale_color_manual(values=c("#999999"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = Chao_lati_ns_surf,aes(x=Latitude, y=Chao1), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline Surface") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
Chao_lati_ns_surf_plot

#4.latitude和Chao1 Index的Non-Saline-water散点图(不好看)
Chao_lati_ns_water <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_ns_water.txt',header = T)
Chao_lati_ns_water_plot <- ggplot()+
  geom_point(data = Chao_lati_ns_water,aes(x=Latitude, y=Chao1,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Chao1 Index")+
  scale_color_manual(values=c("#999999"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = Chao_lati_ns_water,aes(x=Latitude, y=Chao1), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline Water") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
Chao_lati_ns_water_plot

#5.latitude和Chao1 Index的Non-Saline-Water(Water and Sediment)散点图(还可以)
Chao_lati_ns_ws <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_ns_ws.txt',header = T)
Chao_lati_ns_ws_plot <- ggplot()+
  geom_point(data = Chao_lati_ns_ws,aes(x=Latitude, y=Chao1,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Chao1 Index")+
  scale_color_manual(values=c("#56B4E9", "#009E73"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = Chao_lati_ns_ws,aes(x=Latitude, y=Chao1), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline Water (Water and Sediment)") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
Chao_lati_ns_ws_plot


#Saline Samples

#6.latitude和Chao1 Index的Saline-water散点图(不错)
Chao_lati_s_water <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_s_water.txt',header = T)
Chao_lati_s_water_plot <- ggplot()+
  geom_point(data = Chao_lati_s_water,aes(x=Latitude, y=Chao1,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Chao1 Index")+
  scale_color_manual(values=c("#0072B2"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = Chao_lati_s_water,aes(x=Latitude, y=Chao1), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline Water") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
Chao_lati_s_water_plot

#7.latitude和Chao1 Index的Saline-sedi散点图(一般)
Chao_lati_s_sedi <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_s_sedi.txt',header = T)
Chao_lati_s_sedi_plot <- ggplot()+
  geom_point(data = Chao_lati_s_sedi,aes(x=Latitude, y=Chao1,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Chao1 Index")+
  scale_color_manual(values=c("#D55E00"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = Chao_lati_s_sedi,aes(x=Latitude, y=Chao1), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline Sediment") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
Chao_lati_s_sedi_plot

#8.latitude和Chao1 Index的Saline-Water(Water and Sediment)散点图(还可以)
Chao_lati_s_ws <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_s_ws.txt',header = T)
Chao_lati_s_ws_plot <- ggplot()+
  geom_point(data = Chao_lati_s_ws,aes(x=Latitude, y=Chao1,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Chao1 Index")+
  scale_color_manual(values=c("#0072B2","#D55E00"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = Chao_lati_s_ws,aes(x=Latitude, y=Chao1), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline Water (Water and Sediment)") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
Chao_lati_s_ws_plot

#9.latitude和Chao1 Index的Saline-surf散点图(不好看)
Chao_lati_s_surf <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_s_surf.txt',header = T)
Chao_lati_s_surf_plot <- ggplot()+
  geom_point(data = Chao_lati_s_surf,aes(x=Latitude, y=Chao1,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Chao1 Index")+
  scale_color_manual(values=c("#999999"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = Chao_lati_s_surf,aes(x=Latitude, y=Chao1), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline Surface") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
Chao_lati_s_surf_plot


#Plant Samples

#10.latitude和Chao1 Index的Plant-Rhizosphere散点图(不好看)
Chao_lati_p_rhi <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_p_rhi.txt',header = T)
Chao_lati_p_rhi_plot <- ggplot()+
  geom_point(data = Chao_lati_p_rhi,aes(x=Latitude, y=Chao1,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Chao1 Index")+
  scale_color_manual(values=c("#999999"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = Chao_lati_p_rhi,aes(x=Latitude, y=Chao1), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Plant Rhizosphere") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
Chao_lati_p_rhi_plot

#11.latitude和Chao1 Index的Plant-surf散点图(还可以)
Chao_lati_p_surf <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_p_surf.txt',header = T)
Chao_lati_p_surf_plot <- ggplot()+
  geom_point(data = Chao_lati_p_surf,aes(x=Latitude, y=Chao1,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Chao1 Index")+
  scale_color_manual(values=c("#CC79A7"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = Chao_lati_p_surf,aes(x=Latitude, y=Chao1), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Plant Surface") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
Chao_lati_p_surf_plot


#Animal Samples

#12.latitude和Chao1 Index的Animal-surf散点图(不是很好)
Chao_lati_a_surf <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_a_surf.txt',header = T)
Chao_lati_a_surf_plot <- ggplot()+
  geom_point(data = Chao_lati_a_surf,aes(x=Latitude, y=Chao1,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Chao1 Index")+
  scale_color_manual(values=c("#999999"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = Chao_lati_a_surf,aes(x=Latitude, y=Chao1), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Animal Surface") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
Chao_lati_a_surf_plot

ggarrange(Chao_lati_soil_plot, Chao_lati_ns_ws_plot, Chao_lati_s_ws_plot, Chao_lati_ns_sedi_plot, Chao_lati_ns_surf_plot,Chao_lati_ns_water_plot, Chao_lati_s_sedi_plot, Chao_lati_s_surf_plot,Chao_lati_s_water_plot, Chao_lati_a_surf_plot,Chao_lati_p_surf_plot, Chao_lati_p_rhi_plot, ncol = 3, nrow = 4,
          labels = c("A","B","C","D",'E','F','G','H','I',"J","K","L"), # 添加标签
          font.label = list(size = 14))

#Observed OTUs Latitude EMPO3
#Non-Saline Samples

#1.latitude和Observed OTUs的Soil散点图
OO_lati_soil <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_soil.txt',header = T)
OO_lati_soil_plot <- ggplot()+
  geom_point(data = OO_lati_soil,aes(x=Latitude, y=OO,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Observed OTUs")+
  scale_color_manual(values=c("#E69F00"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = OO_lati_soil,aes(x=Latitude, y=OO), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Soil") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
OO_lati_soil_plot


#2.latitude和Observed OTUs的Non-Saline-sedi散点图(不好看)
OO_lati_ns_sedi <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_ns_sedi.txt',header = T)
OO_lati_ns_sedi_plot <- ggplot()+
  geom_point(data = OO_lati_ns_sedi,aes(x=Latitude, y=OO,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Observed OTUs")+
  scale_color_manual(values=c("#999999"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = OO_lati_ns_sedi,aes(x=Latitude, y=OO), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline Sediment") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
OO_lati_ns_sedi_plot

#3.latitude和Observed OTUs的Non-Saline-surf散点图(不好看)
OO_lati_ns_surf <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_ns_surf.txt',header = T)
OO_lati_ns_surf_plot <- ggplot()+
  geom_point(data = OO_lati_ns_surf,aes(x=Latitude, y=OO,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Observed OTUs")+
  scale_color_manual(values=c("#999999"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = OO_lati_ns_surf,aes(x=Latitude, y=OO), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline Surface") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
OO_lati_ns_surf_plot

#4.latitude和Observed OTUs的Non-Saline-water散点图(不好看)
OO_lati_ns_water <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_ns_water.txt',header = T)
OO_lati_ns_water_plot <- ggplot()+
  geom_point(data = OO_lati_ns_water,aes(x=Latitude, y=OO,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Observed OTUs")+
  scale_color_manual(values=c("#999999"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = OO_lati_ns_water,aes(x=Latitude, y=OO), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline Water") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
OO_lati_ns_water_plot

#5.latitude和Observed OTUs的Non-Saline-Water(Water and Sediment)散点图(还可以)
OO_lati_ns_ws <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_ns_ws.txt',header = T)
OO_lati_ns_ws_plot <- ggplot()+
  geom_point(data = OO_lati_ns_ws,aes(x=Latitude, y=OO,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Observed OTUs")+
  scale_color_manual(values=c("#56B4E9", "#009E73"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = OO_lati_ns_ws,aes(x=Latitude, y=OO), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline Water (Water and Sediment)") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
OO_lati_ns_ws_plot


#Saline Samples

#6.latitude和Observed OTUs的Saline-water散点图(不错)
OO_lati_s_water <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_s_water.txt',header = T)
OO_lati_s_water_plot <- ggplot()+
  geom_point(data = OO_lati_s_water,aes(x=Latitude, y=OO,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Observed OTUs")+
  scale_color_manual(values=c("#0072B2"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = OO_lati_s_water,aes(x=Latitude, y=OO), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline Water") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
OO_lati_s_water_plot

#7.latitude和Observed OTUs的Saline-sedi散点图(一般)
OO_lati_s_sedi <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_s_sedi.txt',header = T)
OO_lati_s_sedi_plot <- ggplot()+
  geom_point(data = OO_lati_s_sedi,aes(x=Latitude, y=OO,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Observed OTUs")+
  scale_color_manual(values=c("#D55E00"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = OO_lati_s_sedi,aes(x=Latitude, y=OO), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline Sediment") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
OO_lati_s_sedi_plot

#8.latitude和Observed OTUs的Saline-Water(Water and Sediment)散点图(还可以)
OO_lati_s_ws <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_s_ws.txt',header = T)
OO_lati_s_ws_plot <- ggplot()+
  geom_point(data = OO_lati_s_ws,aes(x=Latitude, y=OO,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Observed OTUs")+
  scale_color_manual(values=c("#0072B2","#D55E00"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = OO_lati_s_ws,aes(x=Latitude, y=OO), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline Water (Water and Sediment)") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
OO_lati_s_ws_plot

#9.latitude和Observed OTUs的Saline-surf散点图(不好看)
OO_lati_s_surf <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_s_surf.txt',header = T)
OO_lati_s_surf_plot <- ggplot()+
  geom_point(data = OO_lati_s_surf,aes(x=Latitude, y=OO,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Observed OTUs")+
  scale_color_manual(values=c("#999999"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = OO_lati_s_surf,aes(x=Latitude, y=OO), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline Surface") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
OO_lati_s_surf_plot


#Plant Samples

#10.latitude和Observed OTUs的Plant-Rhizosphere散点图(不好看)
OO_lati_p_rhi <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_p_rhi.txt',header = T)
OO_lati_p_rhi_plot <- ggplot()+
  geom_point(data = OO_lati_p_rhi,aes(x=Latitude, y=OO,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Observed OTUs")+
  scale_color_manual(values=c("#999999"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = OO_lati_p_rhi,aes(x=Latitude, y=OO), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Plant Rhizosphere") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
OO_lati_p_rhi_plot

#11.latitude和Observed OTUs的Plant-surf散点图(还可以)
OO_lati_p_surf <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_p_surf.txt',header = T)
OO_lati_p_surf_plot <- ggplot()+
  geom_point(data = OO_lati_p_surf,aes(x=Latitude, y=OO,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Observed OTUs")+
  scale_color_manual(values=c("#CC79A7"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = OO_lati_p_surf,aes(x=Latitude, y=OO), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Plant Surface") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
OO_lati_p_surf_plot


#Animal Samples

#12.latitude和Observed OTUs的Animal-surf散点图(不是很好)
OO_lati_a_surf <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_a_surf.txt',header = T)
OO_lati_a_surf_plot <- ggplot()+
  geom_point(data = OO_lati_a_surf,aes(x=Latitude, y=OO,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Observed OTUs")+
  scale_color_manual(values=c("#999999"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = OO_lati_a_surf,aes(x=Latitude, y=OO), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Animal Surface") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
OO_lati_a_surf_plot

ggarrange(OO_lati_soil_plot, OO_lati_ns_ws_plot, OO_lati_s_ws_plot, OO_lati_ns_sedi_plot, OO_lati_ns_surf_plot,OO_lati_ns_water_plot, OO_lati_s_sedi_plot, OO_lati_s_surf_plot,OO_lati_s_water_plot, OO_lati_a_surf_plot,OO_lati_p_surf_plot, OO_lati_p_rhi_plot, ncol = 3, nrow = 4,
          labels = c("A","B","C","D",'E','F','G','H','I',"J","K","L"), # 添加标签
          font.label = list(size = 14))


#Shannon Index Latitude EMPO3
#Non-Saline Samples

#1.latitude和Shannon Index的Soil散点图
shan_lati_soil <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_soil.txt',header = T)
shan_lati_soil_plot <- ggplot()+
  geom_point(data = shan_lati_soil,aes(x=Latitude, y=Shannon.Index,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Shannon Index")+
  scale_color_manual(values=c("#E69F00"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = shan_lati_soil,aes(x=Latitude, y=Shannon.Index), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Soil") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
shan_lati_soil_plot


#2.latitude和Shannon Index的Non-Saline-sedi散点图(不好看)
shan_lati_ns_sedi <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_ns_sedi.txt',header = T)
shan_lati_ns_sedi_plot <- ggplot()+
  geom_point(data = shan_lati_ns_sedi,aes(x=Latitude, y=Shannon.Index,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Shannon Index")+
  scale_color_manual(values=c("#999999"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = shan_lati_ns_sedi,aes(x=Latitude, y=Shannon.Index), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline Sediment") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
shan_lati_ns_sedi_plot

#3.latitude和Shannon Index的Non-Saline-surf散点图(不好看)
shan_lati_ns_surf <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_ns_surf.txt',header = T)
shan_lati_ns_surf_plot <- ggplot()+
  geom_point(data = shan_lati_ns_surf,aes(x=Latitude, y=Shannon.Index,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Shannon Index")+
  scale_color_manual(values=c("#999999"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = shan_lati_ns_surf,aes(x=Latitude, y=Shannon.Index), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline Surface") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
shan_lati_ns_surf_plot

#4.latitude和Shannon Index的Non-Saline-water散点图(不好看)
shan_lati_ns_water <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_ns_water.txt',header = T)
shan_lati_ns_water_plot <- ggplot()+
  geom_point(data = shan_lati_ns_water,aes(x=Latitude, y=Shannon.Index,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Shannon Index")+
  scale_color_manual(values=c("#999999"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = shan_lati_ns_water,aes(x=Latitude, y=Shannon.Index), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline Water") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
shan_lati_ns_water_plot

#5.latitude和Shannon Index的Non-Saline-Water(Water and Sediment)散点图(还可以)
shan_lati_ns_ws <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_ns_ws.txt',header = T)
shan_lati_ns_ws_plot <- ggplot()+
  geom_point(data = shan_lati_ns_ws,aes(x=Latitude, y=Shannon.Index,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Shannon Index")+
  scale_color_manual(values=c("#56B4E9", "#009E73"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = shan_lati_ns_ws,aes(x=Latitude, y=Shannon.Index), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline Water (Water and Sediment)") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
shan_lati_ns_ws_plot


#Saline Samples

#6.latitude和Shannon Index的Saline-water散点图(不错)
shan_lati_s_water <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_s_water.txt',header = T)
shan_lati_s_water_plot <- ggplot()+
  geom_point(data = shan_lati_s_water,aes(x=Latitude, y=Shannon.Index,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Shannon Index")+
  scale_color_manual(values=c("#0072B2"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = shan_lati_s_water,aes(x=Latitude, y=Shannon.Index), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline Water") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
shan_lati_s_water_plot

#7.latitude和Shannon Index的Saline-sedi散点图(一般)
shan_lati_s_sedi <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_s_sedi.txt',header = T)
shan_lati_s_sedi_plot <- ggplot()+
  geom_point(data = shan_lati_s_sedi,aes(x=Latitude, y=Shannon.Index,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Shannon Index")+
  scale_color_manual(values=c("#D55E00"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = shan_lati_s_sedi,aes(x=Latitude, y=Shannon.Index), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline Sediment") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
shan_lati_s_sedi_plot

#8.latitude和Shannon Index的Saline-Water(Water and Sediment)散点图(还可以)
shan_lati_s_ws <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_s_ws.txt',header = T)
shan_lati_s_ws_plot <- ggplot()+
  geom_point(data = shan_lati_s_ws,aes(x=Latitude, y=Shannon.Index,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Shannon Index")+
  scale_color_manual(values=c("#0072B2","#D55E00"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = shan_lati_s_ws,aes(x=Latitude, y=Shannon.Index), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline Water (Water and Sediment)") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
shan_lati_s_ws_plot

#9.latitude和Shannon Index的Saline-surf散点图(不好看)
shan_lati_s_surf <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_s_surf.txt',header = T)
shan_lati_s_surf_plot <- ggplot()+
  geom_point(data = shan_lati_s_surf,aes(x=Latitude, y=Shannon.Index,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Shannon Index")+
  scale_color_manual(values=c("#999999"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = shan_lati_s_surf,aes(x=Latitude, y=Shannon.Index), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline Surface") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
shan_lati_s_surf_plot


#Plant Samples

#10.latitude和Shannon Index的Plant-Rhizosphere散点图(不好看)
shan_lati_p_rhi <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_p_rhi.txt',header = T)
shan_lati_p_rhi_plot <- ggplot()+
  geom_point(data = shan_lati_p_rhi,aes(x=Latitude, y=Shannon.Index,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Shannon Index")+
  scale_color_manual(values=c("#999999"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = shan_lati_p_rhi,aes(x=Latitude, y=Shannon.Index), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Plant Rhizosphere") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
shan_lati_p_rhi_plot

#11.latitude和Shannon Index的Plant-surf散点图(还可以)
shan_lati_p_surf <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_p_surf.txt',header = T)
shan_lati_p_surf_plot <- ggplot()+
  geom_point(data = shan_lati_p_surf,aes(x=Latitude, y=Shannon.Index,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Shannon Index")+
  scale_color_manual(values=c("#CC79A7"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = shan_lati_p_surf,aes(x=Latitude, y=Shannon.Index), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Plant Surface") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
shan_lati_p_surf_plot


#Animal Samples

#12.latitude和Shannon Index的Animal-surf散点图(不是很好)
shan_lati_a_surf <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_a_surf.txt',header = T)
shan_lati_a_surf_plot <- ggplot()+
  geom_point(data = shan_lati_a_surf,aes(x=Latitude, y=Shannon.Index,color = Location), size=1, shape=21)+
  labs(x='Latitude',y="Shannon Index")+
  scale_color_manual(values=c("#999999"))+
  #scale_fill_brewer(palette="Set2")+
  geom_smooth(data = shan_lati_a_surf,aes(x=Latitude, y=Shannon.Index), method = "lm", formula = y ~ splines::bs(x,3), se = T)+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Animal Surface") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")
#+theme(legend.position="none")
shan_lati_a_surf_plot

ggarrange(shan_lati_soil_plot, shan_lati_ns_ws_plot, shan_lati_s_ws_plot, shan_lati_ns_sedi_plot, shan_lati_ns_surf_plot,shan_lati_ns_water_plot, shan_lati_s_sedi_plot, shan_lati_s_surf_plot,shan_lati_s_water_plot, shan_lati_a_surf_plot,shan_lati_p_surf_plot, shan_lati_p_rhi_plot, ncol = 3, nrow = 4,
          labels = c("A","B","C","D",'E','F','G','H','I',"J","K","L"), # 添加标签
          font.label = list(size = 14))
