rm(list = ls())
#loading package

library(car)

#loading files
local_1_all <- read.delim('C:/Users/lenovo/Desktop/Linear Model/local_925_all.txt',header = T)
local_2_all <- read.delim('C:/Users/lenovo/Desktop/Linear Model/local_945_all.txt',header = T)
local_3_all <- read.delim('C:/Users/lenovo/Desktop/Linear Model/local_1884_all.txt',header = T)


#Chao1 Index Polynomial regression models of local samples
#1.All local samples' Polynomial Regression model
#local 1 Polynomial Regression model T*P(***: )
#R2=0.4918, VIF( < 10), AIC=1728.08, MSE=7270.628
pm_chao_local_1_all_Tp <- lm(Chao1~ poly(Temperature, 2) + poly(pH, 2)+ Temperature:pH, data = local_1_all)
summary(pm_chao_local_1_all_Tp)
pm_chao_local_1_all_Tp.step <- step(pm_chao_local_1_all_Tp, direction = "backward")
vif(pm_chao_local_1_all_Tp, merge_coef=fause)
mean(pm_chao_local_1_all_Tp$residuals^2)

#local 2 Polynomial Regression model T*P(***: )
#R2=, VIF( < 10), AIC=, MSE=
pm_chao_local_2_all_Tp <- lm(Chao1~ , data = local_2_all)
summary(pm_chao_local_2_all_Tp)
pm_chao_local_2_all_Tp.step <- step(pm_chao_local_2_all_Tp, direction = "backward")
vif(pm_chao_local_2_all_Tp, merge_coef=fause)
mean(pm_chao_local_2_all_Tp$residuals^2)

#local 3 Polynomial Regression model T*P(***: )
#R2=0.08249, VIF(p < 10), AIC=19921.62, MSE=1750088
pm_chao_local_3_all_Tp <- lm(Chao1~ poly(Temperature, 3) + poly(pH, 1)+ Temperature:pH, data = local_3_all)
summary(pm_chao_local_3_all_Tp)
pm_chao_local_3_all_Tp.step <- step(pm_chao_local_3_all_Tp, direction = "backward")
vif(pm_chao_local_3_all_Tp, merge_coef=fause)
mean(pm_chao_local_3_all_Tp$residuals^2)


#local 1 Polynomial Regression model T(***: )
#R2=0.473, VIF( < 10), AIC=1733.11, MSE=7618.987
pm_chao_local_1_all_T <- lm(Chao1~ poly(Temperature, 3), data = local_1_all)
summary(pm_chao_local_1_all_T)
pm_chao_local_1_all_T.step <- step(pm_chao_local_1_all_T, direction = "backward")
mean(pm_chao_local_1_all_T$residuals^2)

#local 2 Polynomial Regression model T(***: )
#R2=0.003542, VIF( < 10), AIC=12573.92, MSE=177112.2
pm_chao_local_2_all_T <- lm(Chao1~ poly(Temperature, 2), data = local_2_all)
summary(pm_chao_local_2_all_T)
pm_chao_local_2_all_T.step <- step(pm_chao_local_2_all_T, direction = "backward")
mean(pm_chao_local_2_all_T$residuals^2)

#local 3 Polynomial Regression model T(***: )
#R2=0.06512, VIF( < 10), AIC=19945.6, MSE=1785802
pm_chao_local_3_all_T <- lm(Chao1~ poly(Temperature, 3), data = local_3_all)
summary(pm_chao_local_3_all_T)
pm_chao_local_3_all_T.step <- step(pm_chao_local_3_all_T, direction = "backward")
mean(pm_chao_local_3_all_T$residuals^2)

#local 1 Polynomial Regression model P(***: )
#R2=0.4996, VIF( < 10), AIC=1723.12, MSE=7234.551
pm_chao_local_1_all_p <- lm(Chao1~ poly(pH, 3), data = local_1_all)
summary(pm_chao_local_1_all_p)
pm_chao_local_1_all_p.step <- step(pm_chao_local_1_all_p, direction = "backward")
mean(pm_chao_local_1_all_p$residuals^2)

#local 2 Polynomial Regression model P(***: )
#R2=, VIF( < 10), AIC=, MSE=
pm_chao_local_2_all_p <- lm(Chao1~ , data = local_2_all)
summary(pm_chao_local_2_all_p)
pm_chao_local_2_all_p.step <- step(pm_chao_local_2_all_p, direction = "backward")
mean(pm_chao_local_2_all_p$residuals^2)

#local 3 Polynomial Regression model P(***: )
#R2=, VIF( < 10), AIC=, MSE=
pm_chao_local_3_all_p <- lm(Chao1~ poly(pH, 2), data = local_3_all)
summary(pm_chao_local_3_all_p)
pm_chao_local_3_all_p.step <- step(pm_chao_local_3_all_p, direction = "backward")
mean(pm_chao_local_3_all_p$residuals^2)


#Observed OTUs Polynomial regression models of local samples
#1.All local samples' Polynomial Regression model
#local 1 Polynomial Regression model T*P(***: )
#R2=0.433, VIF( < 10), AIC=1677.91, MSE=5606.458
pm_OO_local_1_all_Tp <- lm(OO~ poly(Temperature, 2) + poly(pH, 2)+ Temperature:pH, data = local_1_all)
summary(pm_OO_local_1_all_Tp)
pm_OO_local_1_all_Tp.step <- step(pm_OO_local_1_all_Tp, direction = "backward")
vif(pm_OO_local_1_all_Tp, merge_coef=fause)
mean(pm_OO_local_1_all_Tp$residuals^2)

#local 2 Polynomial Regression model T*P(***: )
#R2=0.0143, VIF( < 10), AIC=11340.47, MSE=53889.28
pm_OO_local_2_all_Tp <- lm(OO~ poly(Temperature, 2) + poly(pH, 2), data = local_2_all)
summary(pm_OO_local_2_all_Tp)
pm_OO_local_2_all_Tp.step <- step(pm_OO_local_2_all_Tp, direction = "backward")
vif(pm_OO_local_2_all_Tp, merge_coef=fause)
mean(pm_OO_local_2_all_Tp$residuals^2)

#local 3 Polynomial Regression model T*P(***: )
#R2=0.08191, VIF( < 10), AIC=17651.3, MSE=338766.6
pm_OO_local_3_all_Tp <- lm(OO~ poly(Temperature, 3) + poly(pH, 3)+ Temperature:pH, data = local_3_all)
summary(pm_OO_local_3_all_Tp)
pm_OO_local_3_all_Tp.step <- step(pm_OO_local_3_all_Tp, direction = "backward")
vif(pm_OO_local_3_all_Tp, merge_coef=fause)
mean(pm_OO_local_3_all_Tp$residuals^2)


#local 1 Polynomial Regression model T(***: )
#R2=0.4204, VIF( < 10), AIC=1680.24, MSE=5793.204
pm_OO_local_1_all_T <- lm(OO~ poly(Temperature, 3), data = local_1_all)
summary(pm_OO_local_1_all_T)
pm_OO_local_1_all_T.step <- step(pm_OO_local_1_all_T, direction = "backward")
mean(pm_OO_local_1_all_T$residuals^2)

#local 2 Polynomial Regression model T(***: )
#R2=0.003983, VIF( < 10), AIC=11349.31, MSE=54558.62
pm_OO_local_2_all_T <- lm(OO~ poly(Temperature, 2), data = local_2_all)
summary(pm_OO_local_2_all_T)
pm_OO_local_2_all_T.step <- step(pm_OO_local_2_all_T, direction = "backward")
mean(pm_OO_local_2_all_T$residuals^2)

#local 3 Polynomial Regression model T(***: )
#R2=0.0647, VIF( < 10), AIC=17673.04, MSE=346119.4
pm_OO_local_3_all_T <- lm(OO~ poly(Temperature, 3), data = local_3_all)
summary(pm_OO_local_3_all_T)
pm_OO_local_3_all_T.step <- step(pm_OO_local_3_all_T, direction = "backward")
mean(pm_OO_local_3_all_T$residuals^2)

#local 1 Polynomial Regression model P(***: )
#R2=0.4368, VIF( < 10), AIC=1674.69, MSE=5629.145
pm_OO_local_1_all_p <- lm(OO~ poly(pH, 3), data = local_1_all)
summary(pm_OO_local_1_all_p)
pm_OO_local_1_all_p.step <- step(pm_OO_local_1_all_p, direction = "backward")
mean(pm_OO_local_1_all_p$residuals^2)

#local 2 Polynomial Regression model P(***: )
#R2=0.01022, VIF( < 10), AIC=11343.78, MSE=54164.96
pm_OO_local_2_all_p <- lm(OO~ poly(pH, 3), data = local_2_all)
summary(pm_OO_local_2_all_p)
pm_OO_local_2_all_p.step <- step(pm_OO_local_2_all_p, direction = "backward")
mean(pm_OO_local_2_all_p$residuals^2)

#local 3 Polynomial Regression model P(***: )
#R2=0.001819, VIF( < 10), AIC=17762.16, MSE=369657.6
pm_OO_local_3_all_p <- lm(OO~ poly(pH, 2), data = local_3_all)
summary(pm_OO_local_3_all_p)
pm_OO_local_3_all_p.step <- step(pm_OO_local_3_all_p, direction = "backward")
mean(pm_OO_local_3_all_p$residuals^2)


#Shannon Index Polynomial regression models of local samples
#1.All local samples' Polynomial Regression model
#local 1 Polynomial Regression model T*P(***: )
#R2=0.3152, VIF( < 10), AIC=-62.53, MSE=0.6796665
pm_Shan_local_1_all_Tp <- lm(Shannon~ poly(Temperature, 2) + poly(pH, 2)+ Temperature:pH, data = local_1_all)
summary(pm_Shan_local_1_all_Tp)
pm_Shan_local_1_all_Tp.step <- step(pm_Shan_local_1_all_Tp, direction = "backward")
vif(pm_Shan_local_1_all_Tp, merge_coef=fause)
mean(pm_Shan_local_1_all_Tp$residuals^2)

#local 2 Polynomial Regression model T*P(***: )
#R2=0.04874, VIF( < 10), AIC=761.72, MSE=2.060217
pm_Shan_local_2_all_Tp <- lm(Shannon~ poly(Temperature, 2) + poly(pH, 2), data = local_2_all)
summary(pm_Shan_local_2_all_Tp)
pm_Shan_local_2_all_Tp.step <- step(pm_Shan_local_2_all_Tp, direction = "backward")
vif(pm_Shan_local_2_all_Tp, merge_coef=fause)
mean(pm_Shan_local_2_all_Tp$residuals^2)

#local 3 Polynomial Regression model T*P(***: )
#R2=0.05284, VIF( < 10), AIC=1056.56, MSE=2.125897
pm_Shan_local_3_all_Tp <- lm(Shannon~ poly(Temperature, 3) + poly(pH, 1)+ Temperature:pH, data = local_3_all)
summary(pm_Shan_local_3_all_Tp)
pm_Shan_local_3_all_Tp.step <- step(pm_Shan_local_3_all_Tp, direction = "backward")
vif(pm_Shan_local_3_all_Tp, merge_coef=fause)
mean(pm_Shan_local_3_all_Tp$residuals^2)


#local 1 Polynomial Regression model T(***: )
#R2=0.267 , VIF( < 10), AIC=-51.34, MSE=0.7353261
pm_Shan_local_1_all_T <- lm(Shannon~ poly(Temperature, 3), data = local_1_all)
summary(pm_Shan_local_1_all_T)
pm_Shan_local_1_all_T.step <- step(pm_Shan_local_1_all_T, direction = "backward")
mean(pm_Shan_local_1_all_T$residuals^2)

#local 2 Polynomial Regression model T(***: )
#R2=0.009679, VIF( < 10), AIC=800.59, MSE=2.151039
pm_Shan_local_2_all_T <- lm(Shannon~ poly(Temperature, 1), data = local_2_all)
summary(pm_Shan_local_2_all_T)
pm_Shan_local_2_all_T.step <- step(pm_Shan_local_2_all_T, direction = "backward")
mean(pm_Shan_local_2_all_T$residuals^2)

#local 3 Polynomial Regression model T(***: )
#R2=0.04849, VIF( < 10), AIC=1060.9, MSE=2.138746
pm_Shan_local_3_all_T <- lm(Shannon~ poly(Temperature, 3), data = local_3_all)
summary(pm_Shan_local_3_all_T)
pm_Shan_local_3_all_T.step <- step(pm_Shan_local_3_all_T, direction = "backward")
mean(pm_Shan_local_3_all_T$residuals^2)

#local 1 Polynomial Regression model P(***: )
#R2=0.289, VIF( < 10), AIC=-57.21, MSE=0.7132908
pm_Shan_local_1_all_p <- lm(Shannon~ poly(pH, 3), data = local_1_all)
summary(pm_Shan_local_1_all_p)
pm_Shan_local_1_all_p.step <- step(pm_Shan_local_1_all_p, direction = "backward")
mean(pm_Shan_local_1_all_p$residuals^2)

#local 2 Polynomial Regression model P(***: )
#R2=0.03629, VIF( < 10), AIC=774.26, MSE=2.089215
pm_Shan_local_2_all_p <- lm(Shannon~ poly(pH, 3), data = local_2_all)
summary(pm_Shan_local_2_all_p)
pm_Shan_local_2_all_p.step <- step(pm_Shan_local_2_all_p, direction = "backward")
mean(pm_Shan_local_2_all_p$residuals^2)

#local 3 Polynomial Regression model P(***: )
#R2=, VIF( < 10), AIC=, MSE=
pm_Shan_local_3_all_p <- lm(Shannon~ , data = local_3_all)
summary(pm_Shan_local_3_all_p)
pm_Shan_local_3_all_p.step <- step(pm_Shan_local_3_all_p, direction = "backward")
mean(pm_Shan_local_3_all_p$residuals^2)
