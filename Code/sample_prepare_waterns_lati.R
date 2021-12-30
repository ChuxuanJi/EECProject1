########################################################################################################
#准备工作
#加载所需函数
install.packages("dplyr", repos="http://cran.us.r-project.org")
library(dplyr)
install.packages("data.table", repos="http://cran.us.r-project.org")
#先去掉环境因素的NA项
df <- read.delim('/rds/general/user/cj421/home/otu_table/raw_data_qcft_env.csv',header = T,sep = ',',row.names = 1)
df1 <- na.omit(df)
#加载
library(data.table)
otu1 <- fread('/rds/general/user/cj421/home/otu_table/otu_table.txt', header=T, sep="\t", skip=1,check.names = F)
#otu <- read.delim('F:/otu_table/otu_table.txt',skip = 1,row.names = 1)#(也是可行的)
#选取前十名
colnames(otu1)[1:10]

otu1<-as.data.frame(otu1)
myfilter<-intersect(colnames(otu1),rownames(df1))
#筛选环境数据
df1<-df1[myfilter,]
write.table(cbind(sample=rownames(df1),df1),'env.txt',row.names = F,sep = '\t',quote = F)
#筛选otu数量大于1的
otu2<-otu1[,c("#OTU ID",myfilter)]
colnames(otu2)[1]<-'OTU'
otu2<-otu2[rowSums(otu2[,-1]>1),]
#拟合
otu3<-otu2 %>%group_by(OTU) %>% summarise_all(sum)
otu4<-otu3[,c(T,colSums(otu3[,-1])>0)]
write.table(otu4,'filter_otu_1.txt',row.names = F,sep = '\t',quote = F)

df2<-df1[colnames(otu4)[-1],]
df2<-df2%>% select(-altitude_m)
write.table(cbind(sample=rownames(df1),df1),'env.txt',row.names = F,sep = '\t',quote = F)
#分组
mygroup<-read.delim('/rds/general/user/cj421/home/group_lati_waterns.txt',header = T)
mygroup<-mygroup[mygroup$ID %in% colnames(otu4),]
write.table(mygroup,'group1_1_waterns_lati.txt',row.names = F,sep = '\t',quote = F)