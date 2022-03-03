rm(list = ls())
#loading package
library(rfPermute)
library(randomForest)

#Chao1 Index Random Forest models
Chaofunc <- read.csv("C:/Users/lenovo/Desktop/Chao_envfactors/Func_Chao_T_pH_lati.csv", stringsAsFactors = T)
#0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#1.All samples' Random Forest
#Random Forest without location, (**: T, p, L)
#R2=0.4231,  MSE=684972.3
set.seed(123)
rf_chao_all_TpL <- randomForest(Chao1~ Temperature*pH*Latitude, data = Chaofunc, importance = TRUE, ntree = 1000)
rf_chao_all_TpL

set.seed(123)
rfp_chao_all_TpL <- rfPermute(Chao1~ Temperature*pH*Latitude, data = Chaofunc, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_chao_all_TpL
importance_rfp_chao_all_TpL.scale <- data.frame(importance(rfp_chao_all_TpL, scale = TRUE), check.names = FALSE)
importance_rfp_chao_all_TpL.scale

#Random Forest without latitude(**: T, p)
#R2=0.1709, MSE=984365.4
set.seed(123)
rf_chao_all_Tp <- randomForest(Chao1 ~ Temperature*pH, data = Chaofunc, importance = TRUE, ntree = 1000)
rf_chao_all_Tp

set.seed(123)
rfp_chao_all_Tp <- rfPermute(Chao1~ Temperature*pH, data = Chaofunc, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_chao_all_Tp
importance_rfp_chao_all_Tp.scale <- data.frame(importance(rfp_chao_all_Tp, scale = TRUE), check.names = FALSE)
importance_rfp_chao_all_Tp.scale

#Random Forest without pH(***: )
#R2=0.4563, MSE=645466.6
set.seed(123)
rf_chao_all_TL <- randomForest(Chao1~ Temperature*Latitude, data = Chaofunc, importance = TRUE, ntree = 1000)
rf_chao_all_TL

set.seed(123)
rfp_chao_all_TL <- rfPermute(Chao1~ Temperature*Latitude, data = Chaofunc, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_chao_all_TL
importance_rfp_chao_all_TL.scale <- data.frame(importance(rfp_chao_all_TL, scale = TRUE), check.names = FALSE)
importance_rfp_chao_all_TL.scale

#Random Forest without Temperature(*: Latitude)
#R2=0.4536, MSE=648776.5
set.seed(123)
rf_chao_all_pL <- randomForest(Chao1 ~ pH*Latitude, data = Chaofunc, importance = TRUE, ntree = 1000)
rf_chao_all_pL

set.seed(123)
rfp_chao_all_pL <- rfPermute(Chao1~ pH*Latitude, data = Chaofunc, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_chao_all_pL
importance_rfp_chao_all_pL.scale <- data.frame(importance(rfp_chao_all_pL, scale = TRUE), check.names = FALSE)
importance_rfp_chao_all_pL.scale

#Random Forest only Temperature(***: All)
#R2=0.078, MSE=1094731
set.seed(123)
rf_chao_all_T <- randomForest(Chao1~ Temperature, data = Chaofunc, importance = TRUE, ntree = 1000)
rf_chao_all_T



#Random Forest only pH(***: )
#R2=0.1183, MSE=1046770
set.seed(123)
rf_chao_all_p <- randomForest(Chao1 ~ pH, data = Chaofunc, importance = TRUE, ntree = 1000)
rf_chao_all_p



#Random Forest only Latitude(***: )
#R2=0.4897, MSE=605910
set.seed(123)
rf_chao_all_L <- randomForest(Chao1 ~ Latitude, data = Chaofunc, importance = TRUE, ntree = 1000)
rf_chao_all_L

#Observed OTUs Random Forest models
OOfunc <- read.csv("C:/Users/lenovo/Desktop/OO_envfactors/Func_OO_T_pH_lati.csv", stringsAsFactors = T)
#0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#1.All samples' Random Forest
#Random Forest without location, (**: T, p, L)
#R2=0.4374,  MSE=138432.2
set.seed(123)
rf_OO_all_TpL <- randomForest(OO~ Temperature*pH*Latitude, data = OOfunc, importance = TRUE, ntree = 1000)
rf_OO_all_TpL

set.seed(123)
rfp_OO_all_TpL <- rfPermute(OO~ Temperature*pH*Latitude, data = OOfunc, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_OO_all_TpL
importance_rfp_OO_all_TpL.scale <- data.frame(importance(rfp_OO_all_TpL, scale = TRUE), check.names = FALSE)
importance_rfp_OO_all_TpL.scale

#Random Forest without latitude(**: T, p)
#R2=0.1794, MSE=201920.4
set.seed(123)
rf_OO_all_Tp <- randomForest(OO ~ Temperature*pH, data = OOfunc, importance = TRUE, ntree = 1000)
rf_OO_all_Tp

set.seed(123)
rfp_OO_all_Tp <- rfPermute(OO~ Temperature*pH, data = OOfunc, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_OO_all_Tp
importance_rfp_OO_all_Tp.scale <- data.frame(importance(rfp_OO_all_Tp, scale = TRUE), check.names = FALSE)
importance_rfp_OO_all_Tp.scale

#Random Forest without pH(**: L, *: T)
#R2=0.4699, MSE=130430.6
set.seed(123)
rf_OO_all_TL <- randomForest(OO~ Temperature*Latitude, data = OOfunc, importance = TRUE, ntree = 1000)
rf_OO_all_TL

set.seed(123)
rfp_OO_all_TL <- rfPermute(OO~ Temperature*Latitude, data = OOfunc, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_OO_all_TL
importance_rfp_OO_all_TL.scale <- data.frame(importance(rfp_OO_all_TL, scale = TRUE), check.names = FALSE)
importance_rfp_OO_all_TL.scale

#Random Forest without Temperature(*: Latitude)
#R2=0.4561, MSE=133833.8
set.seed(123)
rf_OO_all_pL <- randomForest(OO ~ pH*Latitude, data = OOfunc, importance = TRUE, ntree = 1000)
rf_OO_all_pL

set.seed(123)
rfp_OO_all_pL <- rfPermute(OO~ pH*Latitude, data = OOfunc, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_OO_all_pL
importance_rfp_OO_all_pL.scale <- data.frame(importance(rfp_OO_all_pL, scale = TRUE), check.names = FALSE)
importance_rfp_OO_all_pL.scale

#Random Forest only Temperature(***: All)
#R2=0.0832, MSE=225580.7
set.seed(123)
rf_OO_all_T <- randomForest(OO~ Temperature, data = OOfunc, importance = TRUE, ntree = 1000)
rf_OO_all_T



#Random Forest only pH(***: )
#R2=0.1155, MSE=217648.5
set.seed(123)
rf_OO_all_p <- randomForest(OO ~ pH, data = OOfunc, importance = TRUE, ntree = 1000)
rf_OO_all_p



#Random Forest only Latitude(***: )
#R2=0.4963, MSE=123943.2
set.seed(123)
rf_OO_all_L <- randomForest(OO ~ Latitude, data = OOfunc, importance = TRUE, ntree = 1000)
rf_OO_all_L


#Shannon Index Random Forest models
shanfunc <- read.csv("C:/Users/lenovo/Desktop/Shan_envfactors/Func_Shan_T_pH_lati.csv", stringsAsFactors = T)
#0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#1.All samples' Random Forest
#Random Forest without location, (**: T, p, L)
#R2=0.3859,  MSE=1.858015
set.seed(123)
rf_shan_all_TpL <- randomForest(Shannon~ Temperature*pH*Latitude, data = shanfunc, importance = TRUE, ntree = 1000)
rf_shan_all_TpL

set.seed(123)
rfp_shan_all_TpL <- rfPermute(Shannon~ Temperature*pH*Latitude, data = shanfunc, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 1)
rfp_shan_all_TpL
importance_rfp_shan_all_TpL.scale <- data.frame(importance(rfp_shan_all_TpL, scale = TRUE), check.names = FALSE)
importance_rfp_shan_all_TpL.scale

#Random Forest without latitude(**: T, p)
#R2=0.2352, MSE=2.31416
set.seed(123)
rf_shan_all_Tp <- randomForest(Shannon ~ Temperature*pH, data = shanfunc, importance = TRUE, ntree = 1000)
rf_shan_all_Tp

set.seed(123)
rfp_shan_all_Tp <- rfPermute(Shannon~ Temperature*pH, data = shanfunc, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_shan_all_Tp
importance_rfp_shan_all_Tp.scale <- data.frame(importance(rfp_shan_all_Tp, scale = TRUE), check.names = FALSE)
importance_rfp_shan_all_Tp.scale

#Random Forest without pH(**: L, *: T)
#R2=0.3891, MSE=1.8484
set.seed(123)
rf_shan_all_TL <- randomForest(Shannon~ Temperature*Latitude, data = shanfunc, importance = TRUE, ntree = 1000)
rf_shan_all_TL

set.seed(123)
rfp_shan_all_TL <- rfPermute(Shannon~ Temperature*Latitude, data = shanfunc, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_shan_all_TL
importance_rfp_shan_all_TL.scale <- data.frame(importance(rfp_shan_all_TL, scale = TRUE), check.names = FALSE)
importance_rfp_shan_all_TL.scale

#Random Forest without Temperature(*: Latitude)
#R2=0.3834, MSE=1.865562
set.seed(123)
rf_shan_all_pL <- randomForest(Shannon ~ pH*Latitude, data = shanfunc, importance = TRUE, ntree = 1000)
rf_shan_all_pL

set.seed(123)
rfp_shan_all_pL <- rfPermute(Shannon~ pH*Latitude, data = shanfunc, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_shan_all_pL
importance_rfp_shan_all_pL.scale <- data.frame(importance(rfp_shan_all_pL, scale = TRUE), check.names = FALSE)
importance_rfp_shan_all_pL.scale

#Random Forest only Temperature(***: All)
#R2=0.0835, MSE=2.773064
set.seed(123)
rf_shan_all_T <- randomForest(Shannon~ Temperature, data = shanfunc, importance = TRUE, ntree = 1000)
rf_shan_all_T



#Random Forest only pH(***: )
#R2=0.1591, MSE=2.544187
set.seed(123)
rf_shan_all_p <- randomForest(Shannon ~ pH, data = shanfunc, importance = TRUE, ntree = 1000)
rf_shan_all_p



#Random Forest only Latitude(***: )
#R2=0.4049, MSE=1.800532
set.seed(123)
rf_shan_all_L <- randomForest(Shannon ~ Latitude, data = shanfunc, importance = TRUE, ntree = 1000)
rf_shan_all_L
