########################################################################################################
#准备工作
#加载所需函数
library(dplyr)
library(data.table)
library(dplyr)
library(vegan)
library(ggpubr)
library(RColorBrewer)
library(maptools)
library(ggplot2)
library(ggrepel)
options(stringsAsFactors=F)
library(ggsci)
options(stringsAsFactors=F)

#先去掉环境因素的NA项
df <- read.delim('/rds/general/user/cj421/home/Filtered_data_TpL.csv',header = T,sep = ',',row.names = 1)
df1 <- na.omit(df)

otu1 <- fread('/rds/general/user/cj421/home/otu_table/otu_table.txt', header=T, sep="\t", skip=1,check.names = F)
#otu <- read.delim('F:/otu_table/otu_table.txt',skip = 1,row.names = 1)#(也是可行的)
#选取前十名
colnames(otu1)[1:10]

otu1<-as.data.frame(otu1)
myfilter<-intersect(colnames(otu1),rownames(df1))
#筛选环境数据
df1<-df1[myfilter,]

#筛选otu数量大于0的
otu2<-otu1[,c("#OTU ID",myfilter)]
colnames(otu2)[1]<-'OTU'
otu2<-otu2[rowSums(otu2[,-1]>0),]
#拟合
otu3<-otu2 %>%group_by(OTU) %>% summarise_all(sum)
otu4<-otu3[,c(T,colSums(otu3[,-1])>0)]
write.table(otu4,'filter_otu_0.txt',row.names = F,sep = '\t',quote = F)

#读取文件
genus_1 <- read.delim('filter_otu_0.txt',header = T, row.names = 1,check.names = F)
genus_1 <- na.omit(genus_1)
genus_1 <- as.data.frame(t(genus_1))
env <- read.delim('env.txt',row.names = 1,header = T)
#计算出Bray-Curtis距离之后进行nmds分析
dist_1 <- vegdist(genus_1, method="bray",na.rm = T)
dist_1 <- as.matrix(dist_1)
nmds_result_1 <-  metaMDS(dist_1,k=2)
stress <- paste0("Stress=",round(nmds_result_1$stress,3))

nmds12_1 <- as.data.frame(nmds_result_1$points)

env<-env[rownames(nmds12_1),]
fit<-envfit(nmds_result_1,env,permutations = 999,na.rm = TRUE)

summary(fit)
fit
sink('envfit1.txt',append=FALSE)
envfit(nmds_result_1,env,permutations = 999,na.rm = TRUE)
sink(file=NULL)
fit1<-as.data.frame(fit$vectors$arrows)
nmds12_1$samples<-rownames(nmds12_1)
groups_1_emp_7<-read.delim('group1_1_emp3.txt',header = T)
colnames(groups_1_emp_7)[1]<-'samples'





nmds12_1<-merge(nmds12_1,groups_1_emp_7,by='samples')

fit1<-fit1/5
mycol<-c('#8DD3C7','#FFFFB3','#BEBADA','#FB8072','#80B1D3','#FDB462','#B3DE69','#FCCDE5')
ggplot() +
  #geom_text_repel(data = st,aes(RDA1,RDA2,label=row.names(st)),size=4)+#Show a Square
  geom_point(data = nmds12_1,aes(MDS1,MDS2,color=group),size=4)+
  stat_ellipse(data = nmds12_1,aes(MDS1,MDS2,color=group), level = 0.95)+
  #scale_color_manual(values=rep())+
  geom_segment(data = fit1,aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                             type = "closed"),linetype=1, size=0.6,colour = "red")+
  scale_color_npg()+
  geom_text_repel(data = fit1,aes(NMDS1,NMDS2,label=row.names(fit1)))+
  guides(fill = guide_legend(override.aes = list(shape = 21)))+
  geom_hline(yintercept=0,linetype=2) + 
  geom_vline(xintercept=0,linetype=2)+
  labs(x='NMDS1',y="NMDS2",title = stress)+
  theme_bw()+theme(panel.grid=element_blank())

ggsave('nmds_envfit_empo3.pdf',width = 6,height = 4)

