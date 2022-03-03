rm(list = ls())
#loading package
library(rfPermute)
library(randomForest)

#loading files
local_1_all <- read.delim('C:/Users/lenovo/Desktop/Linear Model/local_925_all.txt',header = T)
local_2_all <- read.delim('C:/Users/lenovo/Desktop/Linear Model/local_945_all.txt',header = T)
local_3_all <- read.delim('C:/Users/lenovo/Desktop/Linear Model/local_1884_all.txt',header = T)


#Chao1 Index Random Forest models of local samples
#1.All local samples' Random Forest model
#local 1 Random Forest model T*P(***: )
#R2=0.6588, MSE=5011.602
set.seed(123)
rf_chao_local_1_all_Tp <- randomForest(Chao1 ~ Temperature*pH, data = local_1_all, importance = TRUE, ntree = 150)
rf_chao_local_1_all_Tp
summary(rf_chao_local_1_all_Tp)

set.seed(123)
rfp_chao_local_1_all_Tp <- rfPermute(Chao1~ Temperature*pH, data = local_1_all, importance = TRUE, ntree = 150, nrep = 1000, num.cores = 8)
rfp_chao_local_1_all_Tp
importance_rfp_chao_local_1_all_Tp.scale <- data.frame(importance(rfp_chao_local_1_all_Tp, scale = TRUE), check.names = FALSE)
importance_rfp_chao_local_1_all_Tp.scale

#local 2 Random Forest model T*P(***: )
#R2=, MSE=
set.seed(123)
rf_chao_local_2_all_Tp <- randomForest(Chao1 ~ , data = local_2_all, importance = TRUE, ntree = 750)
rf_chao_local_2_all_Tp
summary(rf_chao_local_2_all_Tp)

set.seed(123)
rfp_chao_local_2_all_Tp <- rfPermute(Chao1~ , data = local_2_all, importance = TRUE, ntree = 750, nrep = 1000, num.cores = 8)
rfp_chao_local_2_all_Tp
importance_rfp_chao_local_2_all_Tp.scale <- data.frame(importance(rfp_chao_local_2_all_Tp, scale = TRUE), check.names = FALSE)
importance_rfp_chao_local_2_all_Tp.scale

#local 3 Random Forest model T*P(***: )
#R2=0.0461, MSE=1826134
set.seed(123)
rf_chao_local_3_all_Tp <- randomForest(Chao1 ~ Temperature*pH, data = local_3_all, importance = TRUE, ntree = 1000)
rf_chao_local_3_all_Tp
summary(rf_chao_local_3_all_Tp)

set.seed(123)
rfp_chao_local_3_all_Tp <- rfPermute(Chao1~ Temperature*pH, data = local_3_all, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_chao_local_3_all_Tp
importance_rfp_chao_local_3_all_Tp.scale <- data.frame(importance(rfp_chao_local_3_all_Tp, scale = TRUE), check.names = FALSE)
importance_rfp_chao_local_3_all_Tp.scale

#local 1 Random Forest model T(***: )
#R2=0.6529, MSE=5097.858
set.seed(123)
rf_chao_local_1_all_T <- randomForest(Chao1 ~ Temperature, data = local_1_all, importance = TRUE, ntree = 150)
rf_chao_local_1_all_T
summary(rf_chao_local_1_all_T)


#local 2 Random Forest model T(***: )
#R2=, MSE=
set.seed(123)
rf_chao_local_2_all_T <- randomForest(Chao1 ~, data = local_2_all, importance = TRUE, ntree = 750)
rf_chao_local_2_all_T
summary(rf_chao_local_2_all_T)


#local 3 Random Forest model T(***: )
#R2=0.0185, MSE=1878924
set.seed(123)
rf_chao_local_3_all_T <- randomForest(Chao1 ~ Temperature, data = local_3_all, importance = TRUE, ntree = 1000)
rf_chao_local_3_all_T
summary(rf_chao_local_3_all_T)

#local 1 Random Forest model P(***: )
#R2=0.6343, MSE=5371.667
set.seed(123)
rf_chao_local_1_all_p <- randomForest(Chao1 ~ pH, data = local_1_all, importance = TRUE, ntree = 150)
rf_chao_local_1_all_p
summary(rf_chao_local_1_all_p)


#local 2 Random Forest model P(***: )
#R2=, MSE=
set.seed(123)
rf_chao_local_2_all_p <- randomForest(Chao1 ~ , data = local_2_all, importance = TRUE, ntree = 750)
rf_chao_local_2_all_p
summary(rf_chao_local_2_all_p)


#local 3 Random Forest model P(***: )
#R2=, MSE=
set.seed(123)
rf_chao_local_3_all_p <- randomForest(Chao1 ~ , data = local_3_all, importance = TRUE, ntree = 1000)
rf_chao_local_3_all_p
summary(rf_chao_local_3_all_p)


#Observed OTUs Random Forest models of local samples
#1.All local samples' Random Forest model
#local 1 Random Forest model T*P(***: )
#R2=0.6205, MSE=3853.018
set.seed(123)
rf_OO_local_1_all_Tp <- randomForest(OO ~ Temperature*pH, data = local_1_all, importance = TRUE, ntree = 150)
rf_OO_local_1_all_Tp
summary(rf_OO_local_1_all_Tp)

set.seed(123)
rfp_OO_local_1_all_Tp <- rfPermute(OO~ Temperature*pH, data = local_1_all, importance = TRUE, ntree = 150, nrep = 1000, num.cores = 8)
rfp_OO_local_1_all_Tp
importance_rfp_OO_local_1_all_Tp.scale <- data.frame(importance(rfp_OO_local_1_all_Tp, scale = TRUE), check.names = FALSE)
importance_rfp_OO_local_1_all_Tp.scale

#local 2 Random Forest model T*P(***: )
#R2=, MSE=
set.seed(123)
rf_OO_local_2_all_Tp <- randomForest(OO ~ , data = local_2_all, importance = TRUE, ntree = 750)
rf_OO_local_2_all_Tp
summary(rf_OO_local_2_all_Tp)

set.seed(123)
rfp_OO_local_2_all_Tp <- rfPermute(OO~ , data = local_2_all, importance = TRUE, ntree = 750, nrep = 1000, num.cores = 8)
rfp_OO_local_2_all_Tp
importance_rfp_OO_local_2_all_Tp.scale <- data.frame(importance(rfp_OO_local_2_all_Tp, scale = TRUE), check.names = FALSE)
importance_rfp_OO_local_2_all_Tp.scale

#local 3 Random Forest model T*P(***: )
#R2=0.0555, MSE=350292.9
set.seed(123)
rf_OO_local_3_all_Tp <- randomForest(OO ~ Temperature*pH, data = local_3_all, importance = TRUE, ntree = 1000)
rf_OO_local_3_all_Tp
summary(rf_OO_local_3_all_Tp)

set.seed(123)
rfp_OO_local_3_all_Tp <- rfPermute(OO~ Temperature*pH, data = local_3_all, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_OO_local_3_all_Tp
importance_rfp_OO_local_3_all_Tp.scale <- data.frame(importance(rfp_OO_local_3_all_Tp, scale = TRUE), check.names = FALSE)
importance_rfp_OO_local_3_all_Tp.scale

#local 1 Random Forest model T(***: )
#R2=0.6159, MSE=3899.422
set.seed(123)
rf_OO_local_1_all_T <- randomForest(OO ~ Temperature, data = local_1_all, importance = TRUE, ntree = 150)
rf_OO_local_1_all_T
summary(rf_OO_local_1_all_T)


#local 2 Random Forest model T(***: )
#R2=, MSE=
set.seed(123)
rf_OO_local_2_all_T <- randomForest(OO ~ , data = local_2_all, importance = TRUE, ntree = 750)
rf_OO_local_2_all_T
summary(rf_OO_local_2_all_T)


#local 3 Random Forest model T(***: )
#R2=0.0272, MSE=360783.4
set.seed(123)
rf_OO_local_3_all_T <- randomForest(OO ~ Temperature, data = local_3_all, importance = TRUE, ntree = 1000)
rf_OO_local_3_all_T
summary(rf_OO_local_3_all_T)

#local 1 Random Forest model P(***: )
#R2=0.5872, MSE=4191.072
set.seed(123)
rf_OO_local_1_all_p <- randomForest(OO ~ pH, data = local_1_all, importance = TRUE, ntree = 150)
rf_OO_local_1_all_p
summary(rf_OO_local_1_all_p)


#local 2 Random Forest model P(***: )
#R2=, MSE=
set.seed(123)
rf_OO_local_2_all_p <- randomForest(OO ~ , data = local_2_all, importance = TRUE, ntree = 750)
rf_OO_local_2_all_p
summary(rf_OO_local_2_all_p)


#local 3 Random Forest model P(***: )
#R2=, MSE=
set.seed(123)
rf_OO_local_3_all_p <- randomForest(OO ~ , data = local_3_all, importance = TRUE, ntree = 1000)
rf_OO_local_3_all_p
summary(rf_OO_local_3_all_p)


#Shannon Index Random Forest models of local samples
#1.All local samples' Random Forest model
#local 1 Random Forest model T*P(***: )
#R2=0.4806, MSE=0.5293473
set.seed(123)
rf_Shan_local_1_all_Tp <- randomForest(Shannon ~ Temperature*pH, data = local_1_all, importance = TRUE, ntree = 150)
rf_Shan_local_1_all_Tp
summary(rf_Shan_local_1_all_Tp)

set.seed(123)
rfp_Shan_local_1_all_Tp <- rfPermute(Shannon~ Temperature*pH, data = local_1_all, importance = TRUE, ntree = 150, nrep = 1000, num.cores = 8)
rfp_Shan_local_1_all_Tp
importance_rfp_Shan_local_1_all_Tp.scale <- data.frame(importance(rfp_Shan_local_1_all_Tp, scale = TRUE), check.names = FALSE)
importance_rfp_Shan_local_1_all_Tp.scale

#local 2 Random Forest model T*P(***: )
#R2=, MSE=
set.seed(123)
rf_Shan_local_2_all_Tp <- randomForest(Shannon ~ , data = local_2_all, importance = TRUE, ntree = 750)
rf_Shan_local_2_all_Tp
summary(rf_Shan_local_2_all_Tp)

set.seed(123)
rfp_Shan_local_2_all_Tp <- rfPermute(Shannon~ , data = local_2_all, importance = TRUE, ntree = 750, nrep = 1000, num.cores = 8)
rfp_Shan_local_2_all_Tp
importance_rfp_Shan_local_2_all_Tp.scale <- data.frame(importance(rfp_Shan_local_2_all_Tp, scale = TRUE), check.names = FALSE)
importance_rfp_Shan_local_2_all_Tp.scale

#local 3 Random Forest model T*P(***: )
#R2=0.0098, MSE=2.230586
set.seed(123)
rf_Shan_local_3_all_Tp <- randomForest(Shannon ~ Temperature*pH, data = local_3_all, importance = TRUE, ntree = 1000)
rf_Shan_local_3_all_Tp
summary(rf_Shan_local_3_all_Tp)

set.seed(123)
rfp_Shan_local_3_all_Tp <- rfPermute(Shannon~ Temperature*pH, data = local_3_all, importance = TRUE, ntree = 1000, nrep = 1000, num.cores = 8)
rfp_Shan_local_3_all_Tp
importance_rfp_Shan_local_3_all_Tp.scale <- data.frame(importance(rfp_Shan_local_3_all_Tp, scale = TRUE), check.names = FALSE)
importance_rfp_Shan_local_3_all_Tp.scale

#local 1 Random Forest model T(***: )
#R2=0.4715, MSE=0.5385572
set.seed(123)
rf_Shan_local_1_all_T <- randomForest(Shannon ~ Temperature, data = local_1_all, importance = TRUE, ntree = 150)
rf_Shan_local_1_all_T
summary(rf_Shan_local_1_all_T)


#local 2 Random Forest model T(***: )
#R2=, MSE=
set.seed(123)
rf_Shan_local_2_all_T <- randomForest(Shannon ~ , data = local_2_all, importance = TRUE, ntree = 750)
rf_Shan_local_2_all_T
summary(rf_Shan_local_2_all_T)


#local 3 Random Forest model T(***: )
#R2=, MSE=
set.seed(123)
rf_Shan_local_3_all_T <- randomForest(Shannon ~ , data = local_3_all, importance = TRUE, ntree = 1000)
rf_Shan_local_3_all_T
summary(rf_Shan_local_3_all_T)

#local 1 Random Forest model P(***: )
#R2=0.4266, MSE=0.5843843
set.seed(123)
rf_Shan_local_1_all_p <- randomForest(Shannon ~ pH, data = local_1_all, importance = TRUE, ntree = 150)
rf_Shan_local_1_all_p
summary(rf_Shan_local_1_all_p)


#local 2 Random Forest model P(***: )
#R2=, MSE=
set.seed(123)
rf_Shan_local_2_all_p <- randomForest(Shannon ~ , data = local_2_all, importance = TRUE, ntree = 750)
rf_Shan_local_2_all_p
summary(rf_Shan_local_2_all_p)


#local 3 Random Forest model P(***: )
#R2=, MSE=
set.seed(123)
rf_Shan_local_3_all_p <- randomForest(Shannon ~ , data = local_3_all, importance = TRUE, ntree = 1000)
rf_Shan_local_3_all_p
summary(rf_Shan_local_3_all_p)
