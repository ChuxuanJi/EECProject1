rm(list = ls())
#loading package
library(rfPermute)
library(randomForest)
library(car)
library(vegan)
library(ape)
library(picante)

input='/rds/general/user/cj421/home/otu_table/otu_table.txt'
data<-read.table(input,header = T,row.names = 1,check.names=F)
groups <- read.delim('/rds/general/user/cj421/home/Filtered_data_TpL.txt',header = T)
colnames(groups)[1] <- 'samples'
mysample <- intersect(groups$samples,colnames(data))
data <- data[,mysample]
tdata<-t(data)
#tdata<-ceiling(as.data.frame(t(data)))

tree <- read.tree('/rds/general/user/cj421/home/emp150.5000_1000_rxbl_placement_pruned75.tog.tre')
PD_whole_tree <- pd(tdata, tree, include.root = FALSE)[1]
names(PD_whole_tree) <- 'PD_whole_tree'
index <- as.data.frame(cbind(PD_whole_tree=PD_whole_tree$PD_whole_tree))
write.table(cbind(sample=colnames(data),index),'/rds/general/user/cj421/home/pd_whole_tree_ft.txt',  row.names = F,sep = '\t',quote = F)

pdfunc <- read.csv("C:/Users/lenovo/Desktop/Filtered_data_TpL.csv", stringsAsFactors = T)

#1.All samples' Polynomial Regression model
#Polynomial Regression model without location(***: , .: )
#R2=0.2069, VIF(All > 10), AIC=, MSE=
pm_pd_all_TpL <- lm(PD~ poly(Temperature , 3) + poly(pH, 3) + poly(Latitude, degree=2) + Temperature:pH + Temperature:Latitude + pH:Latitude + Temperature:pH:Latitude, data = pdfunc)
summary(pm_pd_all_TpL)
vif(pm_pd_all_TpL)
pm_pd_all_TpL.step <- step(pm_pd_all_TpL, direction = "backward")
mean(pm_pd_all_TpL$residuals^2)

#Polynomial Regression without latitude(***: )
#R2=0.04833, VIF(All > 10), AIC=, MSE=
pm_pd_all_Tp <- lm(PD~ poly(Temperature, 3) + poly(pH, 2)+ Temperature:pH, data = pdfunc)
summary(pm_pd_all_Tp)
pm_pd_all_Tp.step <- step(pm_pd_all_Tp, direction = "backward")
vif(pm_pd_all_Tp, merge_coef=fause)
mean(pm_pd_all_Tp$residuals^2)

#Polynomial Regression without pH(***: )
#R2=0.1275, VIF(All > 10), AIC=, MSE=
pm_pd_all_TL <- lm(PD~ poly(Temperature, 3) + poly(Latitude, 3) , data = pdfunc)
summary(pm_pd_all_TL)
pm_pd_all_TL.step <- step(pm_pd_all_TL, direction = "backward")
vif(pm_pd_all_TL, merge_coef=fause)
mean(pm_pd_all_TL$residuals^2)

#Polynomial Regression without Temperature(*: )
#R2=0.1387, VIF(All > 10), AIC=, MSE=
pm_pd_all_pL <- lm(PD~ poly(pH, 2) + poly(Latitude, 2)+pH:Latitude, data = pdfunc)
summary(pm_pd_all_pL)
pm_pd_all_pL.step <- step(pm_pd_all_pL, direction = "backward")
vif(pm_pd_all_pL, merge_coef=fause)
mean(pm_pd_all_pL$residuals^2)


#Polynomial Regression only Temperature(***: All)
#R2=0.01761, VIF(All > 10), AIC=, MSE=
pm_pd_all_T <- lm(PD~ poly(Temperature, 3), data = pdfunc)
summary(pm_pd_all_T)
pm_pd_all_T_1.step <- step(pm_pd_all_T, direction = "backward")
vif(pm_pd_all_T, merge_coef=fause)
mean(pm_pd_all_T$residuals^2)

#Polynomial Regression model only pH(***: Intercept, **: pH)
#R2=0.01704, VIF(All > 10), AIC=, MSE=
pm_pd_all_p <- lm(PD~ poly(pH, 3), data = pdfunc)
summary(pm_pd_all_p)
pm_pd_all_p.step <- step(pm_pd_all_p, direction = "backward")
vif(pm_pd_all_p, merge_coef=fause)
mean(pm_pd_all_p$residuals^2)

#Polynomial Regression model only Latitude(***: All)
#R2=0.09241, VIF(All > 10), AIC=, MSE=
pm_pd_all_L <- lm(PD~ poly(Latitude, 2), data = pdfunc)
summary(pm_pd_all_L)
pm_pd_all_L.step <- step(pm_pd_all_L, direction = "backward")
vif(pm_pd_all_L, merge_coef=fause)
mean(pm_pd_all_L$residuals^2)

#0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#1.All samples' Random Forest
#Random Forest without location, (**: T, p, L)
#R2=0.4223,  MSE=0.4767329
set.seed(123)
rf_pd_all_TpL <- randomForest(PD~ Temperature*pH*Latitude, data = pdfunc, importance = TRUE, ntree = 1000)
rf_pd_all_TpL

set.seed(123)
rfp_pd_all_TpL <- rfPermute(PD~ Temperature*pH*Latitude, data = pdfunc, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_pd_all_TpL
importance_rfp_pd_all_TpL.scale <- data.frame(importance(rfp_pd_all_TpL, scale = TRUE), check.names = FALSE)
importance_rfp_pd_all_TpL.scale

#Random Forest without latitude(**: T, p)
#R2=0.2354,  MSE=0.6309802
set.seed(123)
rf_pd_all_Tp <- randomForest(PD ~ Temperature*pH, data = pdfunc, importance = TRUE, ntree = 1000)
rf_pd_all_Tp

set.seed(123)
rfp_pd_all_Tp <- rfPermute(PD~ Temperature*pH, data = pdfunc, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_pd_all_Tp
importance_rfp_pd_all_Tp.scale <- data.frame(importance(rfp_pd_all_Tp, scale = TRUE), check.names = FALSE)
importance_rfp_pd_all_Tp.scale

#Random Forest without pH(***: )
#R2= 0.4363,  MSE=0.4652249
set.seed(123)
rf_pd_all_TL <- randomForest(PD~ Temperature*Latitude, data = pdfunc, importance = TRUE, ntree = 1000)
rf_pd_all_TL

set.seed(123)
rfp_pd_all_TL <- rfPermute(PD~ Temperature*Latitude, data = pdfunc, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_pd_all_TL
importance_rfp_pd_all_TL.scale <- data.frame(importance(rfp_pd_all_TL, scale = TRUE), check.names = FALSE)
importance_rfp_pd_all_TL.scale

#Random Forest without Temperature(*: Latitude)
#R2=0.4187,  MSE=0.4797418
set.seed(123)
rf_pd_all_pL <- randomForest(PD ~ pH*Latitude, data = pdfunc, importance = TRUE, ntree = 1000)
rf_pd_all_pL

set.seed(123)
rfp_pd_all_pL <- rfPermute(PD~ pH*Latitude, data = pdfunc, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_pd_all_pL
importance_rfp_pd_all_pL.scale <- data.frame(importance(rfp_pd_all_pL, scale = TRUE), check.names = FALSE)
importance_rfp_pd_all_pL.scale

#Random Forest only Temperature(***: All)
#R2=0.0179,  MSE=0.8104523
set.seed(123)
rf_pd_all_T <- randomForest(PD~ Temperature, data = pdfunc, importance = TRUE, ntree = 1000)
rf_pd_all_T



#Random Forest only pH(***: )
#R2=0.1271,  MSE=0.7203333
set.seed(123)
rf_pd_all_p <- randomForest(PD ~ pH, data = pdfunc, importance = TRUE, ntree = 1000)
rf_pd_all_p



#Random Forest only Latitude(***: )
#R2=0.4617,  MSE=0.444203
set.seed(123)
rf_pd_all_L <- randomForest(PD ~ Latitude, data = pdfunc, importance = TRUE, ntree = 1000)
rf_pd_all_L
