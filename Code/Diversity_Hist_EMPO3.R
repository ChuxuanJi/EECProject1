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

#Chao1 Hist
#1.画一个Non-Saline Soil多样性的直方图
Chao_soil <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_soil.txt',header = T)
Chao_soil_hist <- ggplot(data = Chao_soil, aes(x=Chao1))+
  geom_histogram(bins = 100, color=c("#E69F00"))+
  labs(x='Chao1 Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Soil") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Chao_soil_hist

#2.画一个Non-Saline-sedi多样性的直方图
Chao_ns_sedi <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_ns_sedi.txt',header = T)
Chao_ns_sedi_hist <- ggplot(data = Chao_ns_sedi, aes(x=Chao1))+
  geom_histogram(bins = 100, color=c("#43341B"))+
  labs(x='Chao1 Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline Sediment") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Chao_ns_sedi_hist

#3.画一个Non-Saline-surf多样性的直方图
Chao_ns_surf <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_ns_surf.txt',header = T)
Chao_ns_surf_hist <- ggplot(data = Chao_ns_surf, aes(x=Chao1))+
  geom_histogram(bins = 100, color=c("#82663A"))+
  labs(x='Chao1 Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline Surface") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Chao_ns_surf_hist

#4.画一个Non-Saline-water多样性的直方图
Chao_ns_water <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_ns_water.txt',header = T)
Chao_ns_water_hist <- ggplot(data = Chao_ns_water, aes(x=Chao1))+
  geom_histogram(bins = 100, color=c("#7DB9DE"))+
  labs(x='Chao1 Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline Water") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Chao_ns_water_hist

#5.画一个Saline-sedi多样性的直方图
Chao_s_sedi <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_s_sedi.txt',header = T)
Chao_s_sedi_hist <- ggplot(data = Chao_s_sedi, aes(x=Chao1))+
  geom_histogram(bins = 100, color=c("#78552B"))+
  labs(x='Chao1 Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline Sediment") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Chao_s_sedi_hist

#6.画一个Saline-surf多样性的直方图
Chao_s_surf <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_s_surf.txt',header = T)
Chao_s_surf_hist <- ggplot(data = Chao_s_surf, aes(x=Chao1))+
  geom_histogram(bins = 100, color=c("#D7B98E"))+
  labs(x='Chao1 Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline Surface") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Chao_s_surf_hist

#7.画一个Saline-water多样性的直方图
Chao_s_water <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_s_water.txt',header = T)
Chao_s_water_hist <- ggplot(data = Chao_s_water, aes(x=Chao1))+
  geom_histogram(bins = 100, color=c("#113285"))+
  labs(x='Chao1 Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline Water") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Chao_s_water_hist

#8.画一个Plant-rhi多样性的直方图
Chao_p_rhi <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_p_rhi.txt',header = T)
Chao_p_rhi_hist <- ggplot(data = Chao_p_rhi, aes(x=Chao1))+
  geom_histogram(bins = 100, color=c("#4A593D"))+
  labs(x='Chao1 Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Plant Rhizosphere") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Chao_p_rhi_hist

#9.画一个Plant-surf多样性的直方图
Chao_p_surf <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_p_surf.txt',header = T)
Chao_p_surf_hist <- ggplot(data = Chao_p_surf, aes(x=Chao1))+
  geom_histogram(bins = 100, color=c("#A8D8B9"))+
  labs(x='Chao1 Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Plant Surface") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Chao_p_surf_hist

#10.画一个Plant-cor多样性的直方图
Chao_p_cor <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_p_cor.txt',header = T)
Chao_p_cor_hist <- ggplot(data = Chao_p_cor, aes(x=Chao1))+
  geom_histogram(bins = 100, color=c("#42602D"))+
  labs(x='Chao1 Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Plant Corpus") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Chao_p_cor_hist

#11.画一个Animal-sec多样性的直方图
Chao_a_sec <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_a_sec.txt',header = T)
Chao_a_sec_hist <- ggplot(data = Chao_a_sec, aes(x=Chao1))+
  geom_histogram(bins = 100, color=c("#3F2B36"))+
  labs(x='Chao1 Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Animal Secretion") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Chao_a_sec_hist

#12.画一个Animal-surf多样性的直方图
Chao_a_surf <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_a_surf.txt',header = T)
Chao_a_surf_hist <- ggplot(data = Chao_a_surf, aes(x=Chao1))+
  geom_histogram(bins = 100, color=c("#9B6E23"))+
  labs(x='Chao1 Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Animal Surface") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Chao_a_surf_hist

#13.画一个Animal-cor多样性的直方图
Chao_a_cor <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_a_cor.txt',header = T)
Chao_a_cor_hist <- ggplot(data = Chao_a_cor, aes(x=Chao1))+
  geom_histogram(bins = 100, color=c("#CA7A2C"))+
  labs(x='Chao1 Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Animal Corpus") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Chao_a_cor_hist

#14.画一个Animal-dis多样性的直方图
Chao_a_dis <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_a_dis.txt',header = T)
Chao_a_dis_hist <- ggplot(data = Chao_a_dis, aes(x=Chao1))+
  geom_histogram(bins = 100, color=c("#3C2F41"))+
  labs(x='Chao1 Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Animal Distal gut") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Chao_a_dis_hist

#15.画一个Animal-pro多样性的直方图
Chao_a_pro <- read.delim('C:/Users/lenovo/Desktop/Chao_envfactors/Chao_lati_a_pro.txt',header = T)
Chao_a_pro_hist <- ggplot(data = Chao_a_pro, aes(x=Chao1))+
  geom_histogram(bins = 100, color=c("#B28FCE"))+
  labs(x='Chao1 Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Animal Proximal gut") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Chao_a_pro_hist

ggarrange(Chao_soil_hist, Chao_ns_sedi_hist, Chao_ns_surf_hist, Chao_ns_water_hist, Chao_s_sedi_hist, Chao_s_surf_hist, Chao_s_water_hist, Chao_p_rhi_hist,Chao_p_surf_hist, Chao_p_cor_hist,Chao_a_sec_hist, Chao_a_surf_hist, Chao_a_cor_hist,Chao_a_dis_hist, Chao_a_pro_hist,ncol = 3, nrow = 5,
          labels = c("A","B","C","D",'E','F','G','H','I',"J","K","L","M","N","O"), # 添加标签
          font.label = list(size = 14))

#Observed OTUs Hist
#1.画一个Non-Saline Soil多样性的直方图
OO_soil <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_soil.txt',header = T)
OO_soil_hist <- ggplot(data = OO_soil, aes(x=OO))+
  geom_histogram(bins = 100, color=c("#E69F00"))+
  labs(x='Observed OTUs',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Soil") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
OO_soil_hist

#2.画一个Non-Saline-sedi多样性的直方图
OO_ns_sedi <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_ns_sedi.txt',header = T)
OO_ns_sedi_hist <- ggplot(data = OO_ns_sedi, aes(x=OO))+
  geom_histogram(bins = 100, color=c("#43341B"))+
  labs(x='Observed OTUs',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline Sediment") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
OO_ns_sedi_hist

#3.画一个Non-Saline-surf多样性的直方图
OO_ns_surf <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_ns_surf.txt',header = T)
OO_ns_surf_hist <- ggplot(data = OO_ns_surf, aes(x=OO))+
  geom_histogram(bins = 100, color=c("#82663A"))+
  labs(x='Observed OTUs',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline Surface") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
OO_ns_surf_hist

#4.画一个Non-Saline-water多样性的直方图
OO_ns_water <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_ns_water.txt',header = T)
OO_ns_water_hist <- ggplot(data = OO_ns_water, aes(x=OO))+
  geom_histogram(bins = 100, color=c("#7DB9DE"))+
  labs(x='Observed OTUs',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline Water") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
OO_ns_water_hist

#5.画一个Saline-sedi多样性的直方图
OO_s_sedi <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_s_sedi.txt',header = T)
OO_s_sedi_hist <- ggplot(data = OO_s_sedi, aes(x=OO))+
  geom_histogram(bins = 100, color=c("#78552B"))+
  labs(x='Observed OTUs',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline Sediment") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
OO_s_sedi_hist

#6.画一个Saline-surf多样性的直方图
OO_s_surf <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_s_surf.txt',header = T)
OO_s_surf_hist <- ggplot(data = OO_s_surf, aes(x=OO))+
  geom_histogram(bins = 100, color=c("#D7B98E"))+
  labs(x='Observed OTUs',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline Surface") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
OO_s_surf_hist

#7.画一个Saline-water多样性的直方图
OO_s_water <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_s_water.txt',header = T)
OO_s_water_hist <- ggplot(data = OO_s_water, aes(x=OO))+
  geom_histogram(bins = 100, color=c("#113285"))+
  labs(x='Observed OTUs',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline Water") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
OO_s_water_hist

#8.画一个Plant-rhi多样性的直方图
OO_p_rhi <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_p_rhi.txt',header = T)
OO_p_rhi_hist <- ggplot(data = OO_p_rhi, aes(x=OO))+
  geom_histogram(bins = 100, color=c("#4A593D"))+
  labs(x='Observed OTUs',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Plant Rhizosphere") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
OO_p_rhi_hist

#9.画一个Plant-surf多样性的直方图
OO_p_surf <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_p_surf.txt',header = T)
OO_p_surf_hist <- ggplot(data = OO_p_surf, aes(x=OO))+
  geom_histogram(bins = 100, color=c("#A8D8B9"))+
  labs(x='Observed OTUs',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Plant Surface") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
OO_p_surf_hist

#10.画一个Plant-cor多样性的直方图
OO_p_cor <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_p_cor.txt',header = T)
OO_p_cor_hist <- ggplot(data = OO_p_cor, aes(x=OO))+
  geom_histogram(bins = 100, color=c("#42602D"))+
  labs(x='Observed OTUs',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Plant Corpus") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
OO_p_cor_hist

#11.画一个Animal-sec多样性的直方图
OO_a_sec <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_a_sec.txt',header = T)
OO_a_sec_hist <- ggplot(data = OO_a_sec, aes(x=OO))+
  geom_histogram(bins = 100, color=c("#3F2B36"))+
  labs(x='Observed OTUs',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Animal Secretion") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
OO_a_sec_hist

#12.画一个Animal-surf多样性的直方图
OO_a_surf <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_a_surf.txt',header = T)
OO_a_surf_hist <- ggplot(data = OO_a_surf, aes(x=OO))+
  geom_histogram(bins = 100, color=c("#9B6E23"))+
  labs(x='Observed OTUs',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Animal Surface") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
OO_a_surf_hist

#13.画一个Animal-cor多样性的直方图
OO_a_cor <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_a_cor.txt',header = T)
OO_a_cor_hist <- ggplot(data = OO_a_cor, aes(x=OO))+
  geom_histogram(bins = 100, color=c("#CA7A2C"))+
  labs(x='Observed OTUs',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Animal Corpus") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
OO_a_cor_hist

#14.画一个Animal-dis多样性的直方图
OO_a_dis <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_a_dis.txt',header = T)
OO_a_dis_hist <- ggplot(data = OO_a_dis, aes(x=OO))+
  geom_histogram(bins = 100, color=c("#3C2F41"))+
  labs(x='Observed OTUs',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Animal Distal gut") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
OO_a_dis_hist

#15.画一个Animal-pro多样性的直方图
OO_a_pro <- read.delim('C:/Users/lenovo/Desktop/OO_envfactors/OO_lati_a_pro.txt',header = T)
OO_a_pro_hist <- ggplot(data = OO_a_pro, aes(x=OO))+
  geom_histogram(bins = 100, color=c("#B28FCE"))+
  labs(x='Observed OTUs',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Animal Proximal gut") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
OO_a_pro_hist

ggarrange(OO_soil_hist, OO_ns_sedi_hist, OO_ns_surf_hist, OO_ns_water_hist, OO_s_sedi_hist, OO_s_surf_hist, OO_s_water_hist, OO_p_rhi_hist,OO_p_surf_hist, OO_p_cor_hist,OO_a_sec_hist, OO_a_surf_hist, OO_a_cor_hist,OO_a_dis_hist, OO_a_pro_hist,ncol = 3, nrow = 5,
          labels = c("A","B","C","D",'E','F','G','H','I',"J","K","L","M","N","O"), # 添加标签
          font.label = list(size = 14))


#Shannon Index Hist
#1.画一个Non-Saline Soil多样性的直方图
Shan_soil <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_soil.txt',header = T)
Shan_soil_hist <- ggplot(data = Shan_soil, aes(x=Shannon.Index))+
  geom_histogram(bins = 100, color=c("#E69F00"))+
  labs(x='Shannon Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Soil") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Shan_soil_hist

#2.画一个Non-Saline-sedi多样性的直方图
Shan_ns_sedi <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_ns_sedi.txt',header = T)
Shan_ns_sedi_hist <- ggplot(data = Shan_ns_sedi, aes(x=Shannon.Index))+
  geom_histogram(bins = 100, color=c("#43341B"))+
  labs(x='Shannon Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline Sediment") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Shan_ns_sedi_hist

#3.画一个Non-Saline-surf多样性的直方图
Shan_ns_surf <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_ns_surf.txt',header = T)
Shan_ns_surf_hist <- ggplot(data = Shan_ns_surf, aes(x=Shannon.Index))+
  geom_histogram(bins = 100, color=c("#82663A"))+
  labs(x='Shannon Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline Surface") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Shan_ns_surf_hist

#4.画一个Non-Saline-water多样性的直方图
Shan_ns_water <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_ns_water.txt',header = T)
Shan_ns_water_hist <- ggplot(data = Shan_ns_water, aes(x=Shannon.Index))+
  geom_histogram(bins = 100, color=c("#7DB9DE"))+
  labs(x='Shannon Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Non-Saline Water") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Shan_ns_water_hist

#5.画一个Saline-sedi多样性的直方图
Shan_s_sedi <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_s_sedi.txt',header = T)
Shan_s_sedi_hist <- ggplot(data = Shan_s_sedi, aes(x=Shannon.Index))+
  geom_histogram(bins = 100, color=c("#78552B"))+
  labs(x='Shannon Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline Sediment") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Shan_s_sedi_hist

#6.画一个Saline-surf多样性的直方图
Shan_s_surf <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_s_surf.txt',header = T)
Shan_s_surf_hist <- ggplot(data = Shan_s_surf, aes(x=Shannon.Index))+
  geom_histogram(bins = 100, color=c("#D7B98E"))+
  labs(x='Shannon Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline Surface") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Shan_s_surf_hist

#7.画一个Saline-water多样性的直方图
Shan_s_water <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_s_water.txt',header = T)
Shan_s_water_hist <- ggplot(data = Shan_s_water, aes(x=Shannon.Index))+
  geom_histogram(bins = 100, color=c("#113285"))+
  labs(x='Shannon Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Saline Water") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Shan_s_water_hist

#8.画一个Plant-rhi多样性的直方图
Shan_p_rhi <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_p_rhi.txt',header = T)
Shan_p_rhi_hist <- ggplot(data = Shan_p_rhi, aes(x=Shannon.Index))+
  geom_histogram(bins = 100, color=c("#4A593D"))+
  labs(x='Shannon Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Plant Rhizosphere") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Shan_p_rhi_hist

#9.画一个Plant-surf多样性的直方图
Shan_p_surf <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_p_surf.txt',header = T)
Shan_p_surf_hist <- ggplot(data = Shan_p_surf, aes(x=Shannon.Index))+
  geom_histogram(bins = 100, color=c("#A8D8B9"))+
  labs(x='Shannon Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Plant Surface") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Shan_p_surf_hist

#10.画一个Plant-cor多样性的直方图
Shan_p_cor <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_p_cor.txt',header = T)
Shan_p_cor_hist <- ggplot(data = Shan_p_cor, aes(x=Shannon.Index))+
  geom_histogram(bins = 100, color=c("#42602D"))+
  labs(x='Shannon Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Plant Corpus") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Shan_p_cor_hist

#11.画一个Animal-sec多样性的直方图
Shan_a_sec <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_a_sec.txt',header = T)
Shan_a_sec_hist <- ggplot(data = Shan_a_sec, aes(x=Shannon.Index))+
  geom_histogram(bins = 100, color=c("#3F2B36"))+
  labs(x='Shannon Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Animal Secretion") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Shan_a_sec_hist

#12.画一个Animal-surf多样性的直方图
Shan_a_surf <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_a_surf.txt',header = T)
Shan_a_surf_hist <- ggplot(data = Shan_a_surf, aes(x=Shannon.Index))+
  geom_histogram(bins = 100, color=c("#9B6E23"))+
  labs(x='Shannon Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Animal Surface") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Shan_a_surf_hist

#13.画一个Animal-cor多样性的直方图
Shan_a_cor <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_a_cor.txt',header = T)
Shan_a_cor_hist <- ggplot(data = Shan_a_cor, aes(x=Shannon.Index))+
  geom_histogram(bins = 100, color=c("#CA7A2C"))+
  labs(x='Shannon Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Animal Corpus") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Shan_a_cor_hist

#14.画一个Animal-dis多样性的直方图
Shan_a_dis <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_a_dis.txt',header = T)
Shan_a_dis_hist <- ggplot(data = Shan_a_dis, aes(x=Shannon.Index))+
  geom_histogram(bins = 100, color=c("#3C2F41"))+
  labs(x='Shannon Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Animal Distal gut") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Shan_a_dis_hist

#15.画一个Animal-pro多样性的直方图
Shan_a_pro <- read.delim('C:/Users/lenovo/Desktop/Shan_envfactors/Shan_lati_a_pro.txt',header = T)
Shan_a_pro_hist <- ggplot(data = Shan_a_pro, aes(x=Shannon.Index))+
  geom_histogram(bins = 100, color=c("#B28FCE"))+
  labs(x='Shannon Index',y="Number")+
  theme_bw()+theme(panel.grid=element_blank())+
  ggtitle("Animal Proximal gut") + theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_rug()
Shan_a_pro_hist

ggarrange(Shan_soil_hist, Shan_ns_sedi_hist, Shan_ns_surf_hist, Shan_ns_water_hist, Shan_s_sedi_hist, Shan_s_surf_hist, Shan_s_water_hist, Shan_p_rhi_hist,Shan_p_surf_hist, Shan_p_cor_hist,Shan_a_sec_hist, Shan_a_surf_hist, Shan_a_cor_hist,Shan_a_dis_hist, Shan_a_pro_hist,ncol = 3, nrow = 5,
          labels = c("A","B","C","D",'E','F','G','H','I',"J","K","L","M","N","O"), # 添加标签
          font.label = list(size = 14))
