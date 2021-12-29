#test
rm(list=ls())
setwd("C:/Users/lenovo/Desktop/nmds_envft/Unifrac/")
df<-read.delim('emp_90_gg_1k_weighted_unifrac.txt.pc.first_ten.txt',header = F)
colnames(df)[1:3]<-c('samples','PC1','PC2')
mygroups<-read.delim('group_emp.txt',header = T)
colnames(mygroups)[1]<-'samples'
nmds12<-merge(df,mygroups,by='samples')
ggplot() +
  #geom_text_repel(data = st,aes(RDA1,RDA2,label=row.names(st)),size=4)+#Show a Square
  geom_point(data = nmds12,aes(PC1,PC2,color=group),size=0.5,shape=21)+
  stat_ellipse(data = nmds12,aes(PC1,PC2,color=group), level = 0.95)+
#  scale_color_manual(values=mycol)+
  geom_hline(yintercept=0,linetype=2) + 
  geom_vline(xintercept=0,linetype=2)+
  labs(x='PC1',y="PC2")+
  theme_bw()+theme(panel.grid=element_blank())

ggsave('PCA_Uni.pdf',width = 8,height = 6)
