#计算Alpha多样性，Chao1，Shannon，Simpson指数
#加载包
library(vegan)
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)
Raw_qc <- read.csv("C:/Users/lenovo/Desktop/raw_data_qcft.csv")
head(Raw_qc)
getwd()
#同理画一个Shannon的箱线图
Shannon_bin5 <- read.csv("C:/Users/lenovo/Desktop/Shannon_bin_5.csv")
Shannon_bin5
Shannon_box_bin5 <- ggplot(Shannon_bin5, aes(x=Tem, y=adiv_shannon),color=Tem) + 
  geom_boxplot(aes(fill=factor(Tem)))
Shannon_box_bin5
#画一个bin为10的Shannon箱线图
Shannon_box_bin10 <- ggplot(Shannon_bin5, aes(x=Tem_10, y=adiv_shannon),color=Tem_10) + 
  geom_boxplot(aes(fill=factor(Tem_10)))
Shannon_box_bin10

#用ggpubr试一试
Shannon_box_bin10_ggpubr <- ggboxplot(Shannon_bin5,x="Tem_10",y="adiv_shannon",color = "Tem_10",ylab = "Shannon Index",xlab = "Temperature_10")+
  stat_compare_means(comparisons = list(c("-10","0"),c("0","10"),c("10","20"),c("20","30"),c("30","40"),c("40","50"),c("50","60"),c("60","70"),c("70","80"),c("80","90")),method = "t.test",label = "p.signif") 
Shannon_box_bin10_ggpubr

ggsave('box_shan.pdf',width = 8,height = 6)
################################################################################################
#做Chao1中位数的
Chao1_T_Mid <- aggregate(Raw_qc$adiv_chao1,by = list(Temperature = Raw_qc$temperature_deg_c),median)
Chao1_T_Mid
Chao1_T_Mid$Mid <- Chao1_T_Mid$x
#做一个中位数的散点图
Chao1_MidPoint <- ggplot(Chao1_T_Mid, aes(x = Chao1_T_Mid$Temperature, y = Chao1_T_Mid$Mid)) + 
  geom_point(size = (1.5),color="red") + theme_bw() + 
  labs(y="Chao1", x = "Temperature (degree C)")
Chao1_MidPoint
