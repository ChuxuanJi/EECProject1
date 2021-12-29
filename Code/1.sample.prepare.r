########################################################################################################
#׼������
#�������躯��
install.packages("dplyr", repos="http://cran.us.r-project.org")
library(dplyr)
install.packages("data.table", repos="http://cran.us.r-project.org")
#��ȥ���������ص�NA��
df <- read.delim('/rds/general/user/cj421/home/otu_table/raw_data_qcft_env.csv',header = T,sep = ',',row.names = 1)
df1 <- na.omit(df)
#����
library(data.table)
otu1 <- fread('/rds/general/user/cj421/home/otu_table/otu_table.txt', header=T, sep="\t", skip=1,check.names = F)
#otu <- read.delim('F:/otu_table/otu_table.txt',skip = 1,row.names = 1)#(Ҳ�ǿ��е�)
#ѡȡǰʮ��
colnames(otu1)[1:10]

otu1<-as.data.frame(otu1)
myfilter<-intersect(colnames(otu1),rownames(df1))
#ɸѡ��������
df1<-df1[myfilter,]
write.table(cbind(sample=rownames(df1),df1),'env.txt',row.names = F,sep = '\t',quote = F)
#ɸѡotu��������10��
otu2<-otu1[,c("#OTU ID",myfilter)]
colnames(otu2)[1]<-'OTU'
otu2<-otu2[rowSums(otu2[,-1]>10),]
#���
otu3<-otu2 %>%group_by(OTU) %>% summarise_all(sum)
otu4<-otu3[,c(T,colSums(otu3[,-1])>0)]
write.table(otu4,'filter_otu_10.txt',row.names = F,sep = '\t',quote = F)

df2<-df1[colnames(otu4)[-1],]
df2<-df2%>% select(-altitude_m)
write.table(cbind(sample=rownames(df1),df1),'env.txt',row.names = F,sep = '\t',quote = F)
#����
mygroup<-read.delim('/rds/general/user/cj421/home/group_T10.txt',header = T)
mygroup<-mygroup[mygroup$ID %in% colnames(otu4),]
write.table(mygroup,'group1_10_T.txt',row.names = F,sep = '\t',quote = F)