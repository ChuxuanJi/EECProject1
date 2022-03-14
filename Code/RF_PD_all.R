rm(list = ls())
#loading package
library(rfPermute)
library(randomForest)
pdfunc <- read.csv("C:/Users/lenovo/Desktop/Filtered_data_TpL.csv", stringsAsFactors = T)
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
