rm(list = ls())
#loading package

library(car)

#Chao1 Index Polynomial regression models
Chaofunc <- read.csv("C:/Users/lenovo/Desktop/Chao_envfactors/Func_Chao_T_pH_lati.csv", stringsAsFactors = T)

#1.All samples' Polynomial Regression model
#Polynomial Regression model without location(***: , .: )
#R2=0.1306, VIF(All > 10), AIC=42467.64, MSE=1028876
pm_chao_all_TpL <- lm(Chao1~ poly(Temperature , 1) + poly(pH, 2) + poly(Latitude, degree=3) + Temperature:pH + Temperature:Latitude + pH:Latitude + Temperature:pH:Latitude, data = Chaofunc)
summary(pm_chao_all_TpL)
vif(pm_chao_all_TpL)
pm_chao_all_TpL.step <- step(pm_chao_all_TpL, direction = "backward")
mean(pm_chao_all_TpL$residuals^2)

#Polynomial Regression without latitude(***: )
#R2=0.07243, VIF(pH < 10), AIC=42662.14, MSE=1099127
pm_chao_all_Tp <- lm(Chao1~ poly(Temperature, 3) + poly(pH, 3), data = Chaofunc)
summary(pm_chao_all_Tp)
pm_chao_all_Tp.step <- step(pm_chao_all_Tp, direction = "backward")
vif(pm_chao_all_Tp, merge_coef=fause)
mean(pm_chao_all_Tp$residuals^2)

#Polynomial Regression without pH(***: )
#R2=0.1177, VIF( < 10), AIC=42509.86, MSE=1045186
pm_chao_all_TL <- lm(Chao1~ poly(Temperature, 3) + poly(Latitude, 3) + Temperature:Latitude, data = Chaofunc)
summary(pm_chao_all_TL)
pm_chao_all_TL.step <- step(pm_chao_all_TL, direction = "backward")
vif(pm_chao_all_TL, merge_coef=fause)
mean(pm_chao_all_TL$residuals^2)

#Polynomial Regression without Temperature(*: )
#R2=0.1073, VIF( > 10), AIC=42544.81, MSE=1057861
pm_chao_all_pL <- lm(Chao1~ poly(pH, 3) + poly(Latitude, 3), data = Chaofunc)
summary(pm_chao_all_pL)
pm_chao_all_pL.step <- step(pm_chao_all_pL, direction = "backward")
vif(pm_chao_all_pL, merge_coef=fause)
mean(pm_chao_all_pL$residuals^2)


#Polynomial Regression only Temperature(***: All)
#R2=0.04382, VIF(Null), AIC=42752.31, MSE=1134149
pm_chao_all_T <- lm(Chao1~ poly(Temperature, 3), data = Chaofunc)
summary(pm_chao_all_T)
pm_chao_all_T_1.step <- step(pm_chao_all_T, direction = "backward")
vif(pm_chao_all_T, merge_coef=fause)
mean(pm_chao_all_T$residuals^2)

#Polynomial Regression model only pH(***: Intercept, **: pH)
#R2=0.05292, VIF(Null), AIC=42722.98, MSE=1123349
pm_chao_all_p <- lm(Chao1~ poly(pH, 3), data = Chaofunc)
summary(pm_chao_all_p)
pm_chao_all_p.step <- step(pm_chao_all_p, direction = "backward")
vif(pm_chao_all_p, merge_coef=fause)
mean(pm_chao_all_p$residuals^2)

#Polynomial Regression model only Latitude(***: All)
#R2=0.09948, VIF(Null),AIC=42568.41 , MSE=1068121
pm_chao_all_L <- lm(Chao1~ poly(Latitude, 3), data = Chaofunc)
summary(pm_chao_all_L)
pm_chao_all_L.step <- step(pm_chao_all_L, direction = "backward")
vif(pm_chao_all_L, merge_coef=fause)
mean(pm_chao_all_L$residuals^2)


#Observed OTUs Index Polynomial regression models
OOfunc <- read.csv("C:/Users/lenovo/Desktop/OO_envfactors/Func_OO_T_pH_lati.csv", stringsAsFactors = T)

#1.All samples' Polynomial Regression model
#Polynomial Regression model without location(***: , .: )
#R2=0.1271, VIF(All > 10), AIC=37655.37, MSE=214007.9
pm_OO_all_TpL <- lm(OO~ poly(Temperature ,2) + poly(pH,2) + poly(Latitude, 3) + Temperature:pH + Temperature:Latitude + pH:Latitude + Temperature:pH:Latitude, data = OOfunc)
summary(pm_OO_all_TpL)
vif(pm_OO_all_TpL)
pm_OO_all_TpL.step <- step(pm_OO_all_TpL, direction = "backward")
mean(pm_OO_all_TpL$residuals^2)

#Polynomial Regression without latitude(***: )
#R2=0.0757, VIF(pH < 10), AIC=37825.95, MSE=226991
pm_OO_all_Tp <- lm(OO~ poly(Temperature, 3) + poly(pH, 3), data = OOfunc)
summary(pm_OO_all_Tp)
pm_OO_all_Tp.step <- step(pm_OO_all_Tp, direction = "backward")
vif(pm_OO_all_Tp, merge_coef=fause)
mean(pm_OO_all_Tp$residuals^2)

#Polynomial Regression without pH(***: )
#R2=0.1136, VIF( < 10), AIC=37698.41, MSE=217599.8
pm_OO_all_TL <- lm(OO~ poly(Temperature, 3) + poly(Latitude, 3) + Temperature:Latitude, data = OOfunc)
summary(pm_OO_all_TL)
pm_OO_all_TL.step <- step(pm_OO_all_TL, direction = "backward")
vif(pm_OO_all_TL, merge_coef=fause)
mean(pm_OO_all_TL$residuals^2)

#Polynomial Regression without Temperature(*: )
#R2=0.0921, VIF( > 10), AIC=37771.06, MSE=222963.3
pm_OO_all_pL <- lm(OO~ poly(pH, 3) + poly(Latitude, 3), data = OOfunc)
summary(pm_OO_all_pL)
pm_OO_all_pL.step <- step(pm_OO_all_pL, direction = "backward")
vif(pm_OO_all_pL, merge_coef=fause)
mean(pm_OO_all_pL$residuals^2)


#Polynomial Regression only Temperature(***: All)
#R2=0.05232, VIF(Null), AIC=37899.53, MSE=232959.9
pm_OO_all_T <- lm(OO~ poly(Temperature, 3), data = OOfunc)
summary(pm_OO_all_T)
pm_OO_all_T_1.step <- step(pm_OO_all_T, direction = "backward")
vif(pm_OO_all_T, merge_coef=fause)
mean(pm_OO_all_T$residuals^2)

#Polynomial Regression model only pH(***: Intercept, **: pH)
#R2=0.04792, VIF(Null), AIC=37913.76, MSE=234043
pm_OO_all_p <- lm(OO~ poly(pH, 3), data = OOfunc)
summary(pm_OO_all_p)
pm_OO_all_p.step <- step(pm_OO_all_p, direction = "backward")
vif(pm_OO_all_p, merge_coef=fause)
mean(pm_OO_all_p$residuals^2)

#Polynomial Regression model only Latitude(***: All)
#R2=0.08313, VIF(Null),AIC=37798.2 , MSE=225386.3
pm_OO_all_L <- lm(OO~ poly(Latitude, 3), data = OOfunc)
summary(pm_OO_all_L)
pm_OO_all_L.step <- step(pm_OO_all_L, direction = "backward")
vif(pm_OO_all_L, merge_coef=fause)
mean(pm_OO_all_L$residuals^2)


#Shannon Index Polynomial regression models
Shanfunc <- read.csv("C:/Users/lenovo/Desktop/Shan_envfactors/Func_Shan_T_pH_lati.csv", stringsAsFactors = T)

#1.All samples' Polynomial Regression model
#Polynomial Regression model without location(***: , .: )
#R2=0.2193, VIF(All > 10), AIC=2650.25, MSE=2.352002
pm_Shan_all_TpL <- lm(Shannon~ poly(Temperature ,3) + poly(pH,3) + poly(Latitude, 3) + Temperature:pH + Temperature:Latitude + pH:Latitude + Temperature:pH:Latitude, data = Shanfunc)
summary(pm_Shan_all_TpL)
vif(pm_Shan_all_TpL)
pm_Shan_all_TpL.step <- step(pm_Shan_all_TpL, direction = "backward")
mean(pm_Shan_all_TpL$residuals^2)

#Polynomial Regression without latitude(***: )
#R2=0.1201, VIF(pH < 10), AIC=3011.3, MSE=2.656321
pm_Shan_all_Tp <- lm(Shannon~ poly(Temperature, 3) + poly(pH, 3)+Temperature:pH, data = Shanfunc)
summary(pm_Shan_all_Tp)
pm_Shan_all_Tp.step <- step(pm_Shan_all_Tp, direction = "backward")
vif(pm_Shan_all_Tp, merge_coef=fause)
mean(pm_Shan_all_Tp$residuals^2)

#Polynomial Regression without pH(***: )
#R2=0.1972, VIF( < 10), AIC=2729.21, MSE=2.424407
pm_Shan_all_TL <- lm(Shannon~ poly(Temperature, 3) + poly(Latitude, 3) , data = Shanfunc)
summary(pm_Shan_all_TL)
pm_Shan_all_TL.step <- step(pm_Shan_all_TL, direction = "backward")
vif(pm_Shan_all_TL, merge_coef=fause)
mean(pm_Shan_all_TL$residuals^2)

#Polynomial Regression without Temperature(*: )
#R2=0.1586, VIF( > 10), AIC=2872.13, MSE=2.541752
pm_Shan_all_pL <- lm(Shannon~ poly(pH, 2) + poly(Latitude, 3), data = Shanfunc)
summary(pm_Shan_all_pL)
pm_Shan_all_pL.step <- step(pm_Shan_all_pL, direction = "backward")
vif(pm_Shan_all_pL, merge_coef=fause)
mean(pm_Shan_all_pL$residuals^2)


#Polynomial Regression only Temperature(***: All)
#R2=0.081, VIF(Null), AIC=3140.51, MSE=2.777892
pm_Shan_all_T <- lm(Shannon~ poly(Temperature, 3), data = Shanfunc)
summary(pm_Shan_all_T)
pm_Shan_all_T_1.step <- step(pm_Shan_all_T, direction = "backward")
vif(pm_Shan_all_T, merge_coef=fause)
mean(pm_Shan_all_T$residuals^2)

#Polynomial Regression model only pH(***: Intercept, **: pH)
#R2=0.07006, VIF(Null), AIC=3176.78, MSE=2.810951
pm_Shan_all_p <- lm(Shannon~ poly(pH, 3), data = Shanfunc)
summary(pm_Shan_all_p)
pm_Shan_all_p.step <- step(pm_Shan_all_p, direction = "backward")
vif(pm_Shan_all_p, merge_coef=fause)
mean(pm_Shan_all_p$residuals^2)

#Polynomial Regression model only Latitude(***: All)
#R2=0.1445, VIF(Null),AIC=2920.89 , MSE=2.585872
pm_Shan_all_L <- lm(Shannon~ poly(Latitude, 3), data = Shanfunc)
summary(pm_Shan_all_L)
pm_Shan_all_L.step <- step(pm_Shan_all_L, direction = "backward")
vif(pm_Shan_all_L, merge_coef=fause)
mean(pm_Shan_all_L$residuals^2)
