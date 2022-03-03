rm(list = ls())
#loading package
library(car)

#Chao1 All Samples Linear Models
Chaofunc <- read.csv("C:/Users/lenovo/Desktop/Chao_envfactors/Func_Chao_T_pH_lati.csv", stringsAsFactors = T)

#1.All samples' linear model
#linear model without location(***: Temperature:pH, Temperature:Latitude, Temperature:pH:Latitude, .: Temperature)
#R2=0.1077, VIF(All > 10), AIC=42558.62, MSE=1056950
lm_chao_all_TpL <- lm(Chao1~ Temperature*pH*Latitude, data = Chaofunc)
summary(lm_chao_all_TpL)
vif(lm_chao_all_TpL)
lm_chao_all_TpL.step <- step(lm_chao_all_TpL, direction = "backward")
mean(lm_chao_all_TpL$residuals^2)

#linear model without latitude(***: Intercept)
#R2=0.03106, VIF(pH < 10), AIC=42789.23(Chao1 ~ Temperature), MSE=1149276
lm_chao_all_Tp <- lm(Chao1 ~ , data = Chaofunc)
summary(lm_chao_all_Tp)
vif(lm_chao_all_Tp, merge_coef=fause)
lm_chao_all_Tp.step <- step(lm_chao_all_Tp, direction = "backward")
mean(lm_chao_all_Tp$residuals^2)

#linear model without pH(***: All)
#R2=0.0959, VIF(Latitude < 10), AIC=42580.59, MSE=1072372
lm_chao_all_TL <- lm(Chao1 ~ Temperature*Latitude, data = Chaofunc)
summary(lm_chao_all_TL)
vif(lm_chao_all_TL, merge_coef=fause)
lm_chao_all_TL.step <- step(lm_chao_all_TL, direction = "backward")
mean(lm_chao_all_TL$residuals^2)

#linear model without Temperature(*: Latitude)
#R2=0.08393, VIF(All > 10), AIC=42619.93(pH + Latitude), MSE=1086930
lm_chao_all_pL <- lm(Chao1 ~ pH+Latitude, data = Chaofunc)
summary(lm_chao_all_pL)
vif(lm_chao_all_pL, merge_coef=fause)
lm_chao_all_pL.step <- step(lm_chao_all_pL, direction = "backward")
mean(lm_chao_all_pL$residuals^2)

#linear model only Temperature(***: All)
#R2=0.0316, VIF(Null), AIC=42789.23, MSE=1149388
lm_chao_all_T <- lm(Chao1 ~ Temperature, data = Chaofunc)
summary(lm_chao_all_T)
lm_chao_all_T.step <- step(lm_chao_all_T, direction = "backward")
mean(lm_chao_all_T$residuals^2)

#linear model only pH(***: Intercept, **: pH)
#R2=0.002732, VIF(Null), AIC=42879.3, MSE=1183654
lm_chao_all_p <- lm(Chao1 ~ pH, data = Chaofunc)
summary(lm_chao_all_p)
lm_chao_all_p.step <- step(lm_chao_all_p, direction = "backward")
mean(lm_chao_all_p$residuals^2)

#linear model only Latitude(***: All)
#R2=0.08333, VIF(Null), 42620.92, MSE=1087989
lm_chao_all_L <- lm(Chao1 ~ Latitude, data = Chaofunc)
summary(lm_chao_all_L)
lm_chao_all_L.step <- step(lm_chao_all_L, direction = "backward")
mean(lm_chao_all_L$residuals^2)

#Observed OTUs All Samples Linear Models
OOfunc <- read.csv("C:/Users/lenovo/Desktop/OO_envfactors/Func_OO_T_pH_lati.csv", stringsAsFactors = T)

#1.All samples' linear model
#linear model without location(***: Temperature:pH:Latitude, **: Temperature:pH, Temperature:Latitude)
#R2=0.09868, VIF(All > 10), AIC=37749.74, MSE=221273.7
lm_OO_all_TpL <- lm(OO~ Temperature*pH*Latitude, data = OOfunc)
summary(lm_OO_all_TpL)
vif(lm_OO_all_TpL)
lm_OO_all_TpL.step <- step(lm_OO_all_TpL, direction = "backward")
mean(lm_OO_all_TpL$residuals^2)

#linear model without latitude(***: Intercept)
#R2=0.03996, VIF(pH < 10), AIC=37936.24 (OO ~ Temperature), MSE=235997.8
lm_OO_all_Tp <- lm(OO ~ , data = OOfunc)
summary(lm_OO_all_Tp)
vif(lm_OO_all_Tp, merge_coef=fause)
lm_OO_all_Tp.step <- step(lm_OO_all_Tp, direction = "backward")
mean(lm_OO_all_Tp$residuals^2)

#linear model without pH(***: All)
#R2=0.08696, VIF(Latitude < 10), AIC=37785.37, MSE=224445
lm_OO_all_TL <- lm(OO ~ Temperature*Latitude, data = OOfunc)
summary(lm_OO_all_TL)
vif(lm_OO_all_TL, merge_coef=fause)
lm_OO_all_TL.step <- step(lm_OO_all_TL, direction = "backward")
mean(lm_OO_all_TL$residuals^2)

#linear model without Temperature(.: Latitude)
#R2=0.06803, VIF(All > 10), AIC=37847.28 (pH + Latitude), MSE=229172.8
lm_OO_all_pL <- lm(OO ~ pH+Latitude, data = OOfunc)
summary(lm_OO_all_pL)
vif(lm_OO_all_pL, merge_coef=fause)
lm_OO_all_pL.step <- step(lm_OO_all_pL, direction = "backward")
mean(lm_OO_all_pL$residuals^2)

#linear model only Temperature(***: All)
#R2=0.04028, VIF(Null), AIC=37936.24, MSE=236073.7
lm_OO_all_T <- lm(OO ~ Temperature, data = OOfunc)
summary(lm_OO_all_T)
vif(lm_OO_all_T, merge_coef=fause)
lm_OO_all_T.step <- step(lm_OO_all_T, direction = "backward")
mean(lm_OO_all_T$residuals^2)

#linear model only pH(***: Intercept, **: pH)
#R2=0.001798, VIF(Null), AIC=38056.79, MSE=245540
lm_OO_all_p <- lm(OO ~ pH, data = OOfunc)
summary(lm_OO_all_p)
vif(lm_OO_all_p, merge_coef=fause)
lm_OO_all_p.step <- step(lm_OO_all_p, direction = "backward")
mean(lm_OO_all_p$residuals^2)

#linear model only Latitude(***: All)
#R2=0.06739, VIF(Null), AIC=37848.38, MSE=229404.6
lm_OO_all_L <- lm(OO ~ Latitude, data = OOfunc)
summary(lm_OO_all_L)
vif(lm_OO_all_L, merge_coef=fause)
lm_OO_all_L.step <- step(lm_OO_all_L, direction = "backward")
mean(lm_OO_all_L$residuals^2)


#Shannon Index All Samples Linear Models
shanfunc <- read.csv("C:/Users/lenovo/Desktop/shan_envfactors/Func_shan_T_pH_lati.csv", stringsAsFactors = T)

#1.All samples' linear model
#linear model without location(***: Latitude, **: pH, Temperature:pH:Latitude, .: Temperature:Latitude)
#R2=0.1498, VIF(All > 10), AIC=2905.77, MSE=2.566441
lm_shan_all_TpL <- lm(Shannon~ Temperature*pH*Latitude, data = shanfunc)
summary(lm_shan_all_TpL)
vif(lm_shan_all_TpL)
lm_shan_all_TpL.step <- step(lm_shan_all_TpL, direction = "backward")
mean(lm_shan_all_TpL$residuals^2)

#linear model without latitude(***: Intercept, **: Temperature)
#R2=0.07004, VIF(pH < 10), AIC=3175.85(Shannon ~ Temperature + pH), MSE=2.811934
lm_shan_all_Tp <- lm(Shannon ~ Temperature+pH, data = shanfunc)
summary(lm_shan_all_Tp)
vif(lm_shan_all_Tp, merge_coef=fause)
lm_shan_all_Tp.step <- step(lm_shan_all_Tp, direction = "backward")
mean(lm_shan_all_Tp$residuals^2)

#linear model without pH(***: All)
#R2=0.1376, VIF(Latitude < 10), AIC=2945.58, MSE=2.606774
lm_shan_all_TL <- lm(Shannon ~ Temperature*Latitude, data = shanfunc)
summary(lm_shan_all_TL)
vif(lm_shan_all_TL, merge_coef=fause)
lm_shan_all_TL.step <- step(lm_shan_all_TL, direction = "backward")
mean(lm_shan_all_TL$residuals^2)

#linear model without Temperature(***: Latitude, .: pH)
#R2=0.1098, VIF(All > 10), AIC=3041.99, MSE=2.691809
lm_shan_all_pL <- lm(Shannon ~ pH+Latitude, data = shanfunc)
summary(lm_shan_all_pL)
vif(lm_shan_all_pL, merge_coef=fause)
lm_shan_all_pL.step <- step(lm_shan_all_pL, direction = "backward")
mean(lm_shan_all_pL$residuals^2)

#linear model only Temperature(***: All)
#R2=0.06943, VIF(Null), AIC=3176.87, MSE=2.814706
lm_shan_all_T <- lm(Shannon ~ Temperature, data = shanfunc)
summary(lm_shan_all_T)
vif(lm_shan_all_T, merge_coef=fause)
lm_shan_all_T.step <- step(lm_shan_all_T, direction = "backward")
mean(lm_shan_all_T$residuals^2)

#linear model only pH(***: Intercept, **: pH)
#R2=0.002394, VIF(Null), AIC=3390.14, MSE=3.017466
lm_shan_all_p <- lm(Shannon ~ pH, data = shanfunc)
summary(lm_shan_all_p)
vif(lm_shan_all_p, merge_coef=fause)
lm_shan_all_p.step <- step(lm_shan_all_p, direction = "backward")
mean(lm_shan_all_p$residuals^2)

#linear model only Latitude(***: All)
#R2=0.108, VIF(Null), AIC=3047.04, MSE=2.698003
lm_shan_all_L <- lm(Shannon ~ Latitude, data = shanfunc)
summary(lm_shan_all_L)
vif(lm_shan_all_L, merge_coef=fause)
lm_shan_all_L.step <- step(lm_shan_all_L, direction = "backward")
mean(lm_shan_all_L$residuals^2)
