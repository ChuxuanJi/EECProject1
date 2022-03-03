rm(list = ls())

#loading package
library(rfPermute)
library(randomForest)

Chaofunc <- read.csv("C:/Users/lenovo/Desktop/Chao_envfactors/Func_Chao_T_pH_lati.csv", stringsAsFactors = T)
OOfunc <- read.csv("C:/Users/lenovo/Desktop/OO_envfactors/Func_OO_T_pH_lati.csv", stringsAsFactors = T)
Shanfunc <- read.csv("C:/Users/lenovo/Desktop/Shan_envfactors/Func_Shan_T_pH_lati.csv", stringsAsFactors = T)

#Chao1 Index Random Forest models of non-saline water samples
Chaofunc_ns_water <- Chaofunc[which(Chaofunc$Location == 'Water (ns)'), ]
#0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#1.Non-Saline Water samples' Random Forest
#Random Forest without location, (**: T, p, L/L)
#R2=0.3979,  MSE=786219.2
set.seed(123)
rf_Chao_ns_water_TpL <- randomForest(Chao1~ Temperature*pH*Latitude, data = Chaofunc_ns_water, importance = TRUE, ntree = 1000)
rf_Chao_ns_water_TpL

set.seed(123)
rfp_Chao_ns_water_TpL <- rfPermute(Chao1~ Temperature*pH*Latitude, data = Chaofunc_ns_water, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_Chao_ns_water_TpL
importance_rfp_Chao_ns_water_TpL.scale <- data.frame(importance(rfp_Chao_ns_water_TpL, scale = TRUE), check.names = FALSE)
importance_rfp_Chao_ns_water_TpL.scale

#Random Forest without latitude(**T,p/**:T, *p)
#R2=0.14, MSE=1123095
set.seed(123)
rf_Chao_ns_water_Tp <- randomForest(Chao1 ~ Temperature*pH, data = Chaofunc_ns_water, importance = TRUE, ntree = 1000)
rf_Chao_ns_water_Tp

set.seed(123)
rfp_Chao_ns_water_Tp <- rfPermute(Chao1~ Temperature*pH, data = Chaofunc_ns_water, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_Chao_ns_water_Tp
importance_rfp_Chao_ns_water_Tp.scale <- data.frame(importance(rfp_Chao_ns_water_Tp, scale = TRUE), check.names = FALSE)
importance_rfp_Chao_ns_water_Tp.scale

#Random Forest without pH(**:T,L/**:L)
#R2=0.4269, MSE=748353.8
set.seed(123)
rf_Chao_ns_water_TL <- randomForest(Chao1~ Temperature*Latitude, data = Chaofunc_ns_water, importance = TRUE, ntree = 1000)
rf_Chao_ns_water_TL

set.seed(123)
rfp_Chao_ns_water_TL <- rfPermute(Chao1~ Temperature*Latitude, data = Chaofunc_ns_water, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_Chao_ns_water_TL
importance_rfp_Chao_ns_water_TL.scale <- data.frame(importance(rfp_Chao_ns_water_TL, scale = TRUE), check.names = FALSE)
importance_rfp_Chao_ns_water_TL.scale

#Random Forest without Temperature(**: p, L/ **:L)
#R2=0.4308, MSE=743306
set.seed(123)
rf_Chao_ns_water_pL <- randomForest(Chao1 ~ pH*Latitude, data = Chaofunc_ns_water, importance = TRUE, ntree = 1000)
rf_Chao_ns_water_pL

set.seed(123)
rfp_Chao_ns_water_pL <- rfPermute(Chao1~ pH*Latitude, data = Chaofunc_ns_water, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_Chao_ns_water_pL
importance_rfp_Chao_ns_water_pL.scale <- data.frame(importance(rfp_Chao_ns_water_pL, scale = TRUE), check.names = FALSE)
importance_rfp_Chao_ns_water_pL.scale

#Random Forest only Temperature(***: All)
#R2=0.0186, MSE=1281626
set.seed(123)
rf_Chao_ns_water_T <- randomForest(Chao1~ Temperature, data = Chaofunc_ns_water, importance = TRUE, ntree = 1000)
rf_Chao_ns_water_T



#Random Forest only pH(***: )
#R2=0.0994, MSE=1176058
set.seed(123)
rf_Chao_ns_water_p <- randomForest(Chao1 ~ pH, data = Chaofunc_ns_water, importance = TRUE, ntree = 1000)
rf_Chao_ns_water_p



#Random Forest only Latitude(***: )
#R2=0.4653, MSE=698290.9
set.seed(123)
rf_Chao_ns_water_L <- randomForest(Chao1 ~ Latitude, data = Chaofunc_ns_water, importance = TRUE, ntree = 1000)
rf_Chao_ns_water_L


#Observed OTUs Random Forest models of non-saline water samples
OOfunc_ns_water <- OOfunc[which(OOfunc$Location == 'Water (ns)'), ]
#0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#1.Non-Saline Water samples' Random Forest
#Random Forest without location, (**: T, p, L/**:T, L)
#R2=0.3909,  MSE=159553.6
set.seed(123)
rf_OO_ns_water_TpL <- randomForest(OO~ Temperature*pH*Latitude, data = OOfunc_ns_water, importance = TRUE, ntree = 1000)
rf_OO_ns_water_TpL

set.seed(123)
rfp_OO_ns_water_TpL <- rfPermute(OO~ Temperature*pH*Latitude, data = OOfunc_ns_water, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_OO_ns_water_TpL
importance_rfp_OO_ns_water_TpL.scale <- data.frame(importance(rfp_OO_ns_water_TpL, scale = TRUE), check.names = FALSE)
importance_rfp_OO_ns_water_TpL.scale

#Random Forest without latitude(**: T, p/**:T,*:p)
#R2=0.1284, MSE=228316.5
set.seed(123)
rf_OO_ns_water_Tp <- randomForest(OO ~ Temperature*pH, data = OOfunc_ns_water, importance = TRUE, ntree = 1000)
rf_OO_ns_water_Tp

set.seed(123)
rfp_OO_ns_water_Tp <- rfPermute(OO~ Temperature*pH, data = OOfunc_ns_water, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_OO_ns_water_Tp
importance_rfp_OO_ns_water_Tp.scale <- data.frame(importance(rfp_OO_ns_water_Tp, scale = TRUE), check.names = FALSE)
importance_rfp_OO_ns_water_Tp.scale

#Random Forest without pH(**: L,T /**: L)
#R2=0.4304, MSE=149206.3
set.seed(123)
rf_OO_ns_water_TL <- randomForest(OO~ Temperature*Latitude, data = OOfunc_ns_water, importance = TRUE, ntree = 1000)
rf_OO_ns_water_TL

set.seed(123)
rfp_OO_ns_water_TL <- rfPermute(OO~ Temperature*Latitude, data = OOfunc_ns_water, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_OO_ns_water_TL
importance_rfp_OO_ns_water_TL.scale <- data.frame(importance(rfp_OO_ns_water_TL, scale = TRUE), check.names = FALSE)
importance_rfp_OO_ns_water_TL.scale

#Random Forest without Temperature(**: p,L/**:L)
#R2=0.4149, MSE=153262.6
set.seed(123)
rf_OO_ns_water_pL <- randomForest(OO ~ pH*Latitude, data = OOfunc_ns_water, importance = TRUE, ntree = 1000)
rf_OO_ns_water_pL

set.seed(123)
rfp_OO_ns_water_pL <- rfPermute(OO~ pH*Latitude, data = OOfunc_ns_water, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_OO_ns_water_pL
importance_rfp_OO_ns_water_pL.scale <- data.frame(importance(rfp_OO_ns_water_pL, scale = TRUE), check.names = FALSE)
importance_rfp_OO_ns_water_pL.scale

#Random Forest only Temperature(***: All)
#R2=0.0116, MSE=258903.3
set.seed(123)
rf_OO_ns_water_T <- randomForest(OO~ Temperature, data = OOfunc_ns_water, importance = TRUE, ntree = 1000)
rf_OO_ns_water_T



#Random Forest only pH(***: )
#R2=0.0837, MSE=240022.4
set.seed(123)
rf_OO_ns_water_p <- randomForest(OO ~ pH, data = OOfunc_ns_water, importance = TRUE, ntree = 1000)
rf_OO_ns_water_p



#Random Forest only Latitude(***: )
#R2=0.4609, MSE=141211.7
set.seed(123)
rf_OO_ns_water_L <- randomForest(OO ~ Latitude, data = OOfunc_ns_water, importance = TRUE, ntree = 1000)
rf_OO_ns_water_L


#Shannon Index Random Forest models of non-saline water samples
Shanfunc_ns_water <- Shanfunc[which(Shanfunc$Location == 'Water (ns)'), ]
#0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#1.Non-Saline Water samples' Random Forest
#Random Forest without location, (**: T, p, L/**:L)
#R2=0.322,  MSE=1.891411
set.seed(123)
rf_Shan_ns_water_TpL <- randomForest(Shannon~ Temperature*pH*Latitude, data = Shanfunc_ns_water, importance = TRUE, ntree = 1000)
rf_Shan_ns_water_TpL

set.seed(123)
rfp_Shan_ns_water_TpL <- rfPermute(Shannon~ Temperature*pH*Latitude, data = Shanfunc_ns_water, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_Shan_ns_water_TpL
importance_rfp_Shan_ns_water_TpL.scale <- data.frame(importance(rfp_Shan_ns_water_TpL, scale = TRUE), check.names = FALSE)
importance_rfp_Shan_ns_water_TpL.scale

#Random Forest without latitude(**: T, p/**:p, *:T)
#R2=0.1534, MSE=2.361727
set.seed(123)
rf_Shan_ns_water_Tp <- randomForest(Shannon ~ Temperature*pH, data = Shanfunc_ns_water, importance = TRUE, ntree = 1000)
rf_Shan_ns_water_Tp

set.seed(123)
rfp_Shan_ns_water_Tp <- rfPermute(Shannon~ Temperature*pH, data = Shanfunc_ns_water, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_Shan_ns_water_Tp
importance_rfp_Shan_ns_water_Tp.scale <- data.frame(importance(rfp_Shan_ns_water_Tp, scale = TRUE), check.names = FALSE)
importance_rfp_Shan_ns_water_Tp.scale

#Random Forest without pH(**: L,T/ **: L)
#R2=0.3233, MSE=1.887682
set.seed(123)
rf_Shan_ns_water_TL <- randomForest(Shannon~ Temperature*Latitude, data = Shanfunc_ns_water, importance = TRUE, ntree = 1000)
rf_Shan_ns_water_TL

set.seed(123)
rfp_Shan_ns_water_TL <- rfPermute(Shannon~ Temperature*Latitude, data = Shanfunc_ns_water, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_Shan_ns_water_TL
importance_rfp_Shan_ns_water_TL.scale <- data.frame(importance(rfp_Shan_ns_water_TL, scale = TRUE), check.names = FALSE)
importance_rfp_Shan_ns_water_TL.scale

#Random Forest without Temperature(**:p, L/**: L)
#R2=0.3186, MSE=1.901046
set.seed(123)
rf_Shan_ns_water_pL <- randomForest(Shannon ~ pH*Latitude, data = Shanfunc_ns_water, importance = TRUE, ntree = 1000)
rf_Shan_ns_water_pL

set.seed(123)
rfp_Shan_ns_water_pL <- rfPermute(Shannon~ pH*Latitude, data = Shanfunc_ns_water, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_Shan_ns_water_pL
importance_rfp_Shan_ns_water_pL.scale <- data.frame(importance(rfp_Shan_ns_water_pL, scale = TRUE), check.names = FALSE)
importance_rfp_Shan_ns_water_pL.scale

#Random Forest only Temperature(***: All)
#R2=-0.0224, MSE=2.852306
set.seed(123)
rf_Shan_ns_water_T <- randomForest(Shannon~ Temperature, data = Shanfunc_ns_water, importance = TRUE, ntree = 1000)
rf_Shan_ns_water_T



#Random Forest only pH(***: )
#R2=0.1128, MSE=2.474937
set.seed(123)
rf_Shan_ns_water_p <- randomForest(Shannon ~ pH, data = Shanfunc_ns_water, importance = TRUE, ntree = 1000)
rf_Shan_ns_water_p



#Random Forest only Latitude(***: )
#R2=0.3463, MSE=1.823674
set.seed(123)
rf_Shan_ns_water_L <- randomForest(Shannon ~ Latitude, data = Shanfunc_ns_water, importance = TRUE, ntree = 1000)
rf_Shan_ns_water_L


#Chao1 Index Random Forest models of non-saline surface samples
Chaofunc_ns_surf <- Chaofunc[which(Chaofunc$Location == 'Surface (ns)'), ]
#0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#1.Non-Saline Surface samples' Random Forest
#Random Forest without location, (**: T, p, L)
#R2=0.6567  MSE=4997.753
set.seed(123)
rf_Chao_ns_surf_TpL <- randomForest(Chao1~ Temperature*pH*Latitude, data = Chaofunc_ns_surf, importance = TRUE, ntree = 1000)
rf_Chao_ns_surf_TpL

set.seed(123)
rfp_Chao_ns_surf_TpL <- rfPermute(Chao1~ Temperature*pH*Latitude, data = Chaofunc_ns_surf, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_Chao_ns_surf_TpL
importance_rfp_Chao_ns_surf_TpL.scale <- data.frame(importance(rfp_Chao_ns_surf_TpL, scale = TRUE), check.names = FALSE)
importance_rfp_Chao_ns_surf_TpL.scale

#Random Forest without latitude(**: T, p/**:p, *:T)
#R2=0.6621, MSE=4919.851
set.seed(123)
rf_Chao_ns_surf_Tp <- randomForest(Chao1 ~ Temperature*pH, data = Chaofunc_ns_surf, importance = TRUE, ntree = 1000)
rf_Chao_ns_surf_Tp

set.seed(123)
rfp_Chao_ns_surf_Tp <- rfPermute(Chao1~ Temperature*pH, data = Chaofunc_ns_surf, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_Chao_ns_surf_Tp
importance_rfp_Chao_ns_surf_Tp.scale <- data.frame(importance(rfp_Chao_ns_surf_Tp, scale = TRUE), check.names = FALSE)
importance_rfp_Chao_ns_surf_Tp.scale

#Random Forest without pH(**: L,T)
#R2=0.6373, MSE=5279.894
set.seed(123)
rf_Chao_ns_surf_TL <- randomForest(Chao1~ Temperature*Latitude, data = Chaofunc_ns_surf, importance = TRUE, ntree = 1000)
rf_Chao_ns_surf_TL

set.seed(123)
rfp_Chao_ns_surf_TL <- rfPermute(Chao1~ Temperature*Latitude, data = Chaofunc_ns_surf, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_Chao_ns_surf_TL
importance_rfp_Chao_ns_surf_TL.scale <- data.frame(importance(rfp_Chao_ns_surf_TL, scale = TRUE), check.names = FALSE)
importance_rfp_Chao_ns_surf_TL.scale

#Random Forest without Temperature(**:p, L/**: L)
#R2=0.642, MSE=5211.863
set.seed(123)
rf_Chao_ns_surf_pL <- randomForest(Chao1 ~ pH*Latitude, data = Chaofunc_ns_surf, importance = TRUE, ntree = 1000)
rf_Chao_ns_surf_pL

set.seed(123)
rfp_Chao_ns_surf_pL <- rfPermute(Chao1~ pH*Latitude, data = Chaofunc_ns_surf, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_Chao_ns_surf_pL
importance_rfp_Chao_ns_surf_pL.scale <- data.frame(importance(rfp_Chao_ns_surf_pL, scale = TRUE), check.names = FALSE)
importance_rfp_Chao_ns_surf_pL.scale

#Random Forest only Temperature(***: All)
#R2=0.6525, MSE=5059.603
set.seed(123)
rf_Chao_ns_surf_T <- randomForest(Chao1~ Temperature, data = Chaofunc_ns_surf, importance = TRUE, ntree = 1000)
rf_Chao_ns_surf_T



#Random Forest only pH(***: )
#R2=0.6354, MSE=5308.431
set.seed(123)
rf_Chao_ns_surf_p <- randomForest(Chao1 ~ pH, data = Chaofunc_ns_surf, importance = TRUE, ntree = 1000)
rf_Chao_ns_surf_p



#Random Forest only Latitude(***: )
#R2=0.2741, MSE=10567.48
set.seed(123)
rf_Chao_ns_surf_L <- randomForest(Chao1 ~ Latitude, data = Chaofunc_ns_surf, importance = TRUE, ntree = 1000)
rf_Chao_ns_surf_L


#Observed OTUs Random Forest models of non-saline surface samples
OOfunc_ns_surf <- OOfunc[which(OOfunc$Location == 'Surface (ns)'), ]
#0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#1.Non-Saline Surface samples' Random Forest
#Random Forest without location, (**: T, p, L)
#R2=0.6213  MSE=3815.44
set.seed(123)
rf_OO_ns_surf_TpL <- randomForest(OO~ Temperature*pH*Latitude, data = OOfunc_ns_surf, importance = TRUE, ntree = 1000)
rf_OO_ns_surf_TpL

set.seed(123)
rfp_OO_ns_surf_TpL <- rfPermute(OO~ Temperature*pH*Latitude, data = OOfunc_ns_surf, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_OO_ns_surf_TpL
importance_rfp_OO_ns_surf_TpL.scale <- data.frame(importance(rfp_OO_ns_surf_TpL, scale = TRUE), check.names = FALSE)
importance_rfp_OO_ns_surf_TpL.scale

#Random Forest without latitude(**: T, p)
#R2=0.6243, MSE=3784.621
set.seed(123)
rf_OO_ns_surf_Tp <- randomForest(OO ~ Temperature*pH, data = OOfunc_ns_surf, importance = TRUE, ntree = 1000)
rf_OO_ns_surf_Tp

set.seed(123)
rfp_OO_ns_surf_Tp <- rfPermute(OO~ Temperature*pH, data = OOfunc_ns_surf, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_OO_ns_surf_Tp
importance_rfp_OO_ns_surf_Tp.scale <- data.frame(importance(rfp_OO_ns_surf_Tp, scale = TRUE), check.names = FALSE)
importance_rfp_OO_ns_surf_Tp.scale

#Random Forest without pH(**: L,T/ **: L)
#R2=0.5965, MSE=4065.11
set.seed(123)
rf_OO_ns_surf_TL <- randomForest(OO~ Temperature*Latitude, data = OOfunc_ns_surf, importance = TRUE, ntree = 1000)
rf_OO_ns_surf_TL

set.seed(123)
rfp_OO_ns_surf_TL <- rfPermute(OO~ Temperature*Latitude, data = OOfunc_ns_surf, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_OO_ns_surf_TL
importance_rfp_OO_ns_surf_TL.scale <- data.frame(importance(rfp_OO_ns_surf_TL, scale = TRUE), check.names = FALSE)
importance_rfp_OO_ns_surf_TL.scale

#Random Forest without Temperature(**:p, L/**: L)
#R2=0.6008, MSE=4021.709
set.seed(123)
rf_OO_ns_surf_pL <- randomForest(OO ~ pH*Latitude, data = OOfunc_ns_surf, importance = TRUE, ntree = 1000)
rf_OO_ns_surf_pL

set.seed(123)
rfp_OO_ns_surf_pL <- rfPermute(OO~ pH*Latitude, data = OOfunc_ns_surf, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_OO_ns_surf_pL
importance_rfp_OO_ns_surf_pL.scale <- data.frame(importance(rfp_OO_ns_surf_pL, scale = TRUE), check.names = FALSE)
importance_rfp_OO_ns_surf_pL.scale

#Random Forest only Temperature(***: All)
#R2=0.6164, MSE=3865.028
set.seed(123)
rf_OO_ns_surf_T <- randomForest(OO~ Temperature, data = OOfunc_ns_surf, importance = TRUE, ntree = 1000)
rf_OO_ns_surf_T



#Random Forest only pH(***: )
#R2=0.5914, MSE=4116.284
set.seed(123)
rf_OO_ns_surf_p <- randomForest(OO ~ pH, data = OOfunc_ns_surf, importance = TRUE, ntree = 1000)
rf_OO_ns_surf_p

#Random Forest only Latitude(***: )
#R2=0.2656, MSE=7398.691
set.seed(123)
rf_OO_ns_surf_L <- randomForest(OO ~ Latitude, data = OOfunc_ns_surf, importance = TRUE, ntree = 1000)
rf_OO_ns_surf_L


#Shannon Index Random Forest models of non-saline surface samples
Shanfunc_ns_surf <- Shanfunc[which(Shanfunc$Location == 'Surface (ns)'), ]
#0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#1.Non-Saline Surface samples' Random Forest
#Random Forest without location, (**: T, p, L/**:L)
#R2=0.4803  MSE=0.5292739
set.seed(123)
rf_Shan_ns_surf_TpL <- randomForest(Shannon~ Temperature*pH*Latitude, data = Shanfunc_ns_surf, importance = TRUE, ntree = 1000)
rf_Shan_ns_surf_TpL

set.seed(123)
rfp_Shan_ns_surf_TpL <- rfPermute(Shannon~ Temperature*pH*Latitude, data = Shanfunc_ns_surf, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_Shan_ns_surf_TpL
importance_rfp_Shan_ns_surf_TpL.scale <- data.frame(importance(rfp_Shan_ns_surf_TpL, scale = TRUE), check.names = FALSE)
importance_rfp_Shan_ns_surf_TpL.scale

#Random Forest without latitude(**: T, p/**:p, *:T)
#R2=0.4764, MSE=0.5333261
set.seed(123)
rf_Shan_ns_surf_Tp <- randomForest(Shannon ~ Temperature*pH, data = Shanfunc_ns_surf, importance = TRUE, ntree = 1000)
rf_Shan_ns_surf_Tp

set.seed(123)
rfp_Shan_ns_surf_Tp <- rfPermute(Shannon~ Temperature*pH, data = Shanfunc_ns_surf, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_Shan_ns_surf_Tp
importance_rfp_Shan_ns_surf_Tp.scale <- data.frame(importance(rfp_Shan_ns_surf_Tp, scale = TRUE), check.names = FALSE)
importance_rfp_Shan_ns_surf_Tp.scale

#Random Forest without pH(**: L,T/ **: L)
#R2=0.463, MSE=0.5469563
set.seed(123)
rf_Shan_ns_surf_TL <- randomForest(Shannon~ Temperature*Latitude, data = Shanfunc_ns_surf, importance = TRUE, ntree = 1000)
rf_Shan_ns_surf_TL

set.seed(123)
rfp_Shan_ns_surf_TL <- rfPermute(Shannon~ Temperature*Latitude, data = Shanfunc_ns_surf, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_Shan_ns_surf_TL
importance_rfp_Shan_ns_surf_TL.scale <- data.frame(importance(rfp_Shan_ns_surf_TL, scale = TRUE), check.names = FALSE)
importance_rfp_Shan_ns_surf_TL.scale

#Random Forest without Temperature(**:p, L/**: L)
#R2=0.4681, MSE=0.5417001
set.seed(123)
rf_Shan_ns_surf_pL <- randomForest(Shannon ~ pH*Latitude, data = Shanfunc_ns_surf, importance = TRUE, ntree = 1000)
rf_Shan_ns_surf_pL

set.seed(123)
rfp_Shan_ns_surf_pL <- rfPermute(Shannon~ pH*Latitude, data = Shanfunc_ns_surf, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_Shan_ns_surf_pL
importance_rfp_Shan_ns_surf_pL.scale <- data.frame(importance(rfp_Shan_ns_surf_pL, scale = TRUE), check.names = FALSE)
importance_rfp_Shan_ns_surf_pL.scale

#Random Forest only Temperature(***: All)
#R2=0.4696, MSE=0.5402067
set.seed(123)
rf_Shan_ns_surf_T <- randomForest(Shannon~ Temperature, data = Shanfunc_ns_surf, importance = TRUE, ntree = 1000)
rf_Shan_ns_surf_T



#Random Forest only pH(***: )
#R2=0.4225, MSE=0.5881959
set.seed(123)
rf_Shan_ns_surf_p <- randomForest(Shannon ~ pH, data = Shanfunc_ns_surf, importance = TRUE, ntree = 1000)
rf_Shan_ns_surf_p



#Random Forest only Latitude(***: )
#R2=0.2542, MSE=0.7595831
set.seed(123)
rf_Shan_ns_surf_L <- randomForest(Shannon ~ Latitude, data = Shanfunc_ns_surf, importance = TRUE, ntree = 1000)
rf_Shan_ns_surf_L


#Chao1 Index Random Forest models of saline sediment samples
Chaofunc_s_sedi <- Chaofunc[which(Chaofunc$Location == 'Sediment (s)'), ]
#0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#1.Non-Saline Surface samples' Random Forest
#Random Forest without location, (**: T, p, L)
#R2=0.4741  MSE=310671.3
set.seed(123)
rf_Chao_s_sedi_TpL <- randomForest(Chao1~ Temperature*pH*Latitude, data = Chaofunc_s_sedi, importance = TRUE, ntree = 100)
rf_Chao_s_sedi_TpL

set.seed(123)
rfp_Chao_s_sedi_TpL <- rfPermute(Chao1~ Temperature*pH*Latitude, data = Chaofunc_s_sedi, importance = TRUE, ntree = 100, nrep = 1000, num.cores = 8)
rfp_Chao_s_sedi_TpL
importance_rfp_Chao_s_sedi_TpL.scale <- data.frame(importance(rfp_Chao_s_sedi_TpL, scale = TRUE), check.names = FALSE)
importance_rfp_Chao_s_sedi_TpL.scale

#Random Forest without latitude(**: T, p)
#R2=0.4342, MSE=334245.2
set.seed(123)
rf_Chao_s_sedi_Tp <- randomForest(Chao1 ~ Temperature*pH, data = Chaofunc_s_sedi, importance = TRUE, ntree = 100)
rf_Chao_s_sedi_Tp

set.seed(123)
rfp_Chao_s_sedi_Tp <- rfPermute(Chao1~ Temperature*pH, data = Chaofunc_s_sedi, importance = TRUE, ntree = 100, nrep = 1000, num.cores = 8)
rfp_Chao_s_sedi_Tp
importance_rfp_Chao_s_sedi_Tp.scale <- data.frame(importance(rfp_Chao_s_sedi_Tp, scale = TRUE), check.names = FALSE)
importance_rfp_Chao_s_sedi_Tp.scale

#Random Forest without pH(**: L,T/ **: L)
#R2=0.4672, MSE=314716.9
set.seed(123)
rf_Chao_s_sedi_TL <- randomForest(Chao1~ Temperature*Latitude, data = Chaofunc_s_sedi, importance = TRUE, ntree = 100)
rf_Chao_s_sedi_TL

set.seed(123)
rfp_Chao_s_sedi_TL <- rfPermute(Chao1~ Temperature*Latitude, data = Chaofunc_s_sedi, importance = TRUE, ntree = 100, nrep = 1000, num.cores = 8)
rfp_Chao_s_sedi_TL
importance_rfp_Chao_s_sedi_TL.scale <- data.frame(importance(rfp_Chao_s_sedi_TL, scale = TRUE), check.names = FALSE)
importance_rfp_Chao_s_sedi_TL.scale

#Random Forest without Temperature(**:p, L)
#R2=0.4709, MSE=312537.1
set.seed(123)
rf_Chao_s_sedi_pL <- randomForest(Chao1 ~ pH*Latitude, data = Chaofunc_s_sedi, importance = TRUE, ntree = 100)
rf_Chao_s_sedi_pL

set.seed(123)
rfp_Chao_s_sedi_pL <- rfPermute(Chao1~ pH*Latitude, data = Chaofunc_s_sedi, importance = TRUE, ntree = 100, nrep = 1000, num.cores = 8)
rfp_Chao_s_sedi_pL
importance_rfp_Chao_s_sedi_pL.scale <- data.frame(importance(rfp_Chao_s_sedi_pL, scale = TRUE), check.names = FALSE)
importance_rfp_Chao_s_sedi_pL.scale

#Random Forest only Temperature(***: All)
#R2=0.4677, MSE=314453
set.seed(123)
rf_Chao_s_sedi_T <- randomForest(Chao1~ Temperature, data = Chaofunc_s_sedi, importance = TRUE, ntree = 100)
rf_Chao_s_sedi_T
summary(rf_Chao_s_sedi_T)


#Random Forest only pH(***: )
#R2=0.4275, MSE=338161
set.seed(123)
rf_Chao_s_sedi_p <- randomForest(Chao1 ~ pH, data = Chaofunc_s_sedi, importance = TRUE, ntree = 100)
rf_Chao_s_sedi_p



#Random Forest only Latitude(***: )
#R2=0.447, MSE=326634.5
set.seed(123)
rf_Chao_s_sedi_L <- randomForest(Chao1 ~ Latitude, data = Chaofunc_s_sedi, importance = TRUE, ntree = 100)
rf_Chao_s_sedi_L


#Observed OTUs Random Forest models of saline sediment samples
OOfunc_s_sedi <- OOfunc[which(OOfunc$Location == 'Sediment (s)'), ]
#0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#1.Non-Saline Surface samples' Random Forest
#Random Forest without location, (**: T, p, L)
#R2=0.5295  MSE=65094.01
set.seed(123)
rf_OO_s_sedi_TpL <- randomForest(OO~ Temperature*pH*Latitude, data = OOfunc_s_sedi, importance = TRUE, ntree = 100)
rf_OO_s_sedi_TpL

set.seed(123)
rfp_OO_s_sedi_TpL <- rfPermute(OO~ Temperature*pH*Latitude, data = OOfunc_s_sedi, importance = TRUE, ntree = 100, nrep = 1000, num.cores = 8)
rfp_OO_s_sedi_TpL
importance_rfp_OO_s_sedi_TpL.scale <- data.frame(importance(rfp_OO_s_sedi_TpL, scale = TRUE), check.names = FALSE)
importance_rfp_OO_s_sedi_TpL.scale

#Random Forest without latitude(**: T, p)
#R2=0.5121, MSE=67502.25
set.seed(123)
rf_OO_s_sedi_Tp <- randomForest(OO ~ Temperature*pH, data = OOfunc_s_sedi, importance = TRUE, ntree = 100)
rf_OO_s_sedi_Tp

set.seed(123)
rfp_OO_s_sedi_Tp <- rfPermute(OO~ Temperature*pH, data = OOfunc_s_sedi, importance = TRUE, ntree = 100, nrep = 1000, num.cores = 8)
rfp_OO_s_sedi_Tp
importance_rfp_OO_s_sedi_Tp.scale <- data.frame(importance(rfp_OO_s_sedi_Tp, scale = TRUE), check.names = FALSE)
importance_rfp_OO_s_sedi_Tp.scale

#Random Forest without pH(**: L,T/ **: L)
#R2=0.5355, MSE=64263.1
set.seed(123)
rf_OO_s_sedi_TL <- randomForest(OO~ Temperature*Latitude, data = OOfunc_s_sedi, importance = TRUE, ntree = 100)
rf_OO_s_sedi_TL

set.seed(123)
rfp_OO_s_sedi_TL <- rfPermute(OO~ Temperature*Latitude, data = OOfunc_s_sedi, importance = TRUE, ntree = 100, nrep = 1000, num.cores = 8)
rfp_OO_s_sedi_TL
importance_rfp_OO_s_sedi_TL.scale <- data.frame(importance(rfp_OO_s_sedi_TL, scale = TRUE), check.names = FALSE)
importance_rfp_OO_s_sedi_TL.scale

#Random Forest without Temperature(**:p, L)
#R2=0.5416, MSE=63416.05
set.seed(123)
rf_OO_s_sedi_pL <- randomForest(OO ~ pH*Latitude, data = OOfunc_s_sedi, importance = TRUE, ntree = 100)
rf_OO_s_sedi_pL

set.seed(123)
rfp_OO_s_sedi_pL <- rfPermute(OO~ pH*Latitude, data = OOfunc_s_sedi, importance = TRUE, ntree = 100, nrep = 1000, num.cores = 8)
rfp_OO_s_sedi_pL
importance_rfp_OO_s_sedi_pL.scale <- data.frame(importance(rfp_OO_s_sedi_pL, scale = TRUE), check.names = FALSE)
importance_rfp_OO_s_sedi_pL.scale

#Random Forest only Temperature(***: All)
#R2=0.5303, MSE=64976.56
set.seed(123)
rf_OO_s_sedi_T <- randomForest(OO~ Temperature, data = OOfunc_s_sedi, importance = TRUE, ntree = 100)
rf_OO_s_sedi_T



#Random Forest only pH(***: )
#R2=0.4941, MSE=69982.99
set.seed(123)
rf_OO_s_sedi_p <- randomForest(OO ~ pH, data = OOfunc_s_sedi, importance = TRUE, ntree = 100)
rf_OO_s_sedi_p



#Random Forest only Latitude(***: )
#R2=0.5301, MSE=65010.36
set.seed(123)
rf_OO_s_sedi_L <- randomForest(OO ~ Latitude, data = OOfunc_s_sedi, importance = TRUE, ntree = 100)
rf_OO_s_sedi_L


#Shannon Index Random Forest models of saline sediment samples
Shanfunc_s_sedi <- Shanfunc[which(Shanfunc$Location == 'Sediment (s)'), ]
#0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#1.Non-Saline Surface samples' Random Forest
#Random Forest without location, (**: T, p, L)
#R2=0.4035  MSE=2.924771
set.seed(123)
rf_Shan_s_sedi_TpL <- randomForest(Shannon~ Temperature*pH*Latitude, data = Shanfunc_s_sedi, importance = TRUE, ntree = 100)
rf_Shan_s_sedi_TpL

set.seed(123)
rfp_Shan_s_sedi_TpL <- rfPermute(Shannon~ Temperature*pH*Latitude, data = Shanfunc_s_sedi, importance = TRUE, ntree = 100, nrep = 1000, num.cores = 8)
rfp_Shan_s_sedi_TpL
importance_rfp_Shan_s_sedi_TpL.scale <- data.frame(importance(rfp_Shan_s_sedi_TpL, scale = TRUE), check.names = FALSE)
importance_rfp_Shan_s_sedi_TpL.scale

#Random Forest without latitude(**: T, p)
#R2=0.3925, MSE=2.978634
set.seed(123)
rf_Shan_s_sedi_Tp <- randomForest(Shannon ~ Temperature*pH, data = Shanfunc_s_sedi, importance = TRUE, ntree = 100)
rf_Shan_s_sedi_Tp

set.seed(123)
rfp_Shan_s_sedi_Tp <- rfPermute(Shannon~ Temperature*pH, data = Shanfunc_s_sedi, importance = TRUE, ntree = 100, nrep = 1000, num.cores = 8)
rfp_Shan_s_sedi_Tp
importance_rfp_Shan_s_sedi_Tp.scale <- data.frame(importance(rfp_Shan_s_sedi_Tp, scale = TRUE), check.names = FALSE)
importance_rfp_Shan_s_sedi_Tp.scale

#Random Forest without pH(**: L,T/ **: L)
#R2=0.3988, MSE=2.94746
set.seed(123)
rf_Shan_s_sedi_TL <- randomForest(Shannon~ Temperature*Latitude, data = Shanfunc_s_sedi, importance = TRUE, ntree = 100)
rf_Shan_s_sedi_TL

set.seed(123)
rfp_Shan_s_sedi_TL <- rfPermute(Shannon~ Temperature*Latitude, data = Shanfunc_s_sedi, importance = TRUE, ntree = 100, nrep = 1000, num.cores = 8)
rfp_Shan_s_sedi_TL
importance_rfp_Shan_s_sedi_TL.scale <- data.frame(importance(rfp_Shan_s_sedi_TL, scale = TRUE), check.names = FALSE)
importance_rfp_Shan_s_sedi_TL.scale

#Random Forest without Temperature(**:p, L)
#R2=0.3988, MSE=2.947435
set.seed(123)
rf_Shan_s_sedi_pL <- randomForest(Shannon ~ pH*Latitude, data = Shanfunc_s_sedi, importance = TRUE, ntree = 100)
rf_Shan_s_sedi_pL

set.seed(123)
rfp_Shan_s_sedi_pL <- rfPermute(Shannon~ pH*Latitude, data = Shanfunc_s_sedi, importance = TRUE, ntree = 1000, nrep = 100, num.cores = 8)
rfp_Shan_s_sedi_pL
importance_rfp_Shan_s_sedi_pL.scale <- data.frame(importance(rfp_Shan_s_sedi_pL, scale = TRUE), check.names = FALSE)
importance_rfp_Shan_s_sedi_pL.scale

#Random Forest only Temperature(***: All)
#R2=0.3899, MSE=2.991322
set.seed(123)
rf_Shan_s_sedi_T <- randomForest(Shannon~ Temperature, data = Shanfunc_s_sedi, importance = TRUE, ntree = 100)
rf_Shan_s_sedi_T



#Random Forest only pH(***: )
#R2=0.3854, MSE=3.013444
set.seed(123)
rf_Shan_s_sedi_p <- randomForest(Shannon ~ pH, data = Shanfunc_s_sedi, importance = TRUE, ntree = 100)
rf_Shan_s_sedi_p



#Random Forest only Latitude(***: )
#R2=0.4007, MSE=2.938273
set.seed(123)
rf_Shan_s_sedi_L <- randomForest(Shannon ~ Latitude, data = Shanfunc_s_sedi, importance = TRUE, ntree = 100)
rf_Shan_s_sedi_L
