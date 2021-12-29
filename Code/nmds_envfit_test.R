rm(list=ls())
#下载所需要的包和安装
install.packages("dplyr",repos="http://cran.us.r-project.org")
library(dplyr)
install.packages("vegan",repos="http://cran.us.r-project.org")
library(vegan)
install.packages("ggpubr",repos="http://cran.us.r-project.org")
library(ggpubr)
install.packages("RColorBrewer",repos="http://cran.us.r-project.org")
library(RColorBrewer)
install.packages("maptools",repos="http://cran.us.r-project.org")
library(maptools)
install.packages("ggplot2",repos="http://cran.us.r-project.org")
library(ggplot2)
install.packages("ggrepel",repos="http://cran.us.r-project.org")
library(ggrepel)
options(stringsAsFactors=F)
#读取文件
genus_10 <- read.delim('filter_otu_10.txt',header = T, row.names = 1,check.names = F)
genus_10 <- na.omit(genus_10)
genus_10 <- as.data.frame(t(genus_10))
env <- read.delim('env.txt',row.names = 1,header = T)
#计算出Bray-Curtis距离之后进行nmds分析
dist_10 <- vegdist(genus_10, method="bray",na.rm = T)
dist_10 <- as.matrix(dist_10)
nmds_result_10 <-  metaMDS(dist_10,k=2)
stress <- paste0("Stress=",round(nmds_result_10$stress,3))

nmds12_10 <- as.data.frame(nmds_result_10$points)

env<-env[rownames(nmds12_10),]
fit<-envfit(nmds_result_10,env,permutations = 999,na.rm = TRUE)

summary(fit)
fit
sink('envfit1.txt',append=FALSE)
envfit(nmds_result_10,env,permutations = 999,na.rm = TRUE)
sink(file=NULL)
fit1<-as.data.frame(fit$vectors$arrows)
nmds12_10$samples<-rownames(nmds12_10)
groups_10_T<-read.delim('group1_10_T.txt',header = T)
colnames(groups_10_T)[1]<-'samples'





nmds12_10<-merge(nmds12_10,groups_10_T,by='samples')

fit1<-fit1/5
mycol<-c('#8DD3C7','#FFFFB3','#BEBADA','#FB8072','#80B1D3','#FDB462','#B3DE69','#FCCDE5')
ggplot() +
  #geom_text_repel(data = st,aes(RDA1,RDA2,label=row.names(st)),size=4)+#Show a Square
  geom_point(data = nmds12_10,aes(MDS1,MDS2,color=nmds12_10$group),size=4)+
  #scale_color_manual(values=rep())+
  geom_segment(data = fit1,aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                             type = "closed"),linetype=1, size=0.6,colour = "red")+
  scale_color_manual(values = mycol)+
  geom_text_repel(data = fit1,aes(NMDS1,NMDS2,label=row.names(fit1)))+
  guides(fill = guide_legend(override.aes = list(shape = 21)))+
  geom_hline(yintercept=0,linetype=2) + 
  geom_vline(xintercept=0,linetype=2)+
  labs(x='NMDS1',y="NMDS2",title = stress)+
  theme_bw()+theme(panel.grid=element_blank())

ggsave('nmds_envfit_10_T.pdf',width = 8,height = 6)





ggplot() +
  #geom_text_repel(data = st,aes(RDA1,RDA2,label=row.names(st)),size=4)+#Show a Square
  geom_point(data = nmds12_10,aes(MDS1,MDS2,color=nmds12_10$group),size=4)+
  stat_ellipse(data = nmds12_10,aes(MDS1,MDS2,color=nmds12_10$group), level = 0.95)+
  scale_color_manual(values=mycol)+
  geom_hline(yintercept=0,linetype=2) + 
  geom_vline(xintercept=0,linetype=2)+
  labs(x='NMDS1',y="NMDS2",title = stress)+
  theme_bw()+theme(panel.grid=element_blank())

ggsave('nmds_without_envfit_10_T.pdf',width = 8,height = 6)
