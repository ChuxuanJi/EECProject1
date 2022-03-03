rm(list = ls())
#loading package
library(car)

Chaofunc <- read.csv("C:/Users/lenovo/Desktop/Chao_envfactors/Func_Chao_T_pH_lati.csv", stringsAsFactors = T)
OOfunc <- read.csv("C:/Users/lenovo/Desktop/OO_envfactors/Func_OO_T_pH_lati.csv", stringsAsFactors = T)
shanfunc <- read.csv("C:/Users/lenovo/Desktop/shan_envfactors/Func_shan_T_pH_lati.csv", stringsAsFactors = T)

#Chao1 Index Linear models of non-saline water samples
Chaofunc_ns_water <- Chaofunc[which(Chaofunc$Location == 'Water (ns)'), ]

#1.All samples' linear model
#linear model without location(***: (Intercept), Temperature, pH, Latitude, Temperature:pH, pH:Latitude, Temperature:Latitude, Temperature:pH:Latitude)
#R2=0.1781, VIF(ALL > 10), AIC=35127.23, MSE=1070296
lm_Chao_ns_water_TpL <- lm(Chao1~ Temperature*pH*Latitude, data = Chaofunc_ns_water)
summary(lm_Chao_ns_water_TpL)
vif(lm_Chao_ns_water_TpL)
lm_Chao_ns_water_TpL.step <- step(lm_Chao_ns_water_TpL, direction = "backward")
mean(lm_Chao_ns_water_TpL$residuals^2)

#linear model without latitude(**: Intercept)
#R2=0.002872, VIF(pH < 10), AIC=35612.1,(AIC=35609.15,~T), MSE=1300592
lm_Chao_ns_water_Tp <- lm(Chao1 ~ , data = Chaofunc_ns_water)
summary(lm_Chao_ns_water_Tp)
vif(lm_Chao_ns_water_Tp, merge_coef=fause)
lm_Chao_ns_water_Tp.step <- step(lm_Chao_ns_water_Tp, direction = "backward")
mean(lm_Chao_ns_water_Tp$residuals^2)

#linear model without pH(***: All)
#R2=0.1458, VIF(L < 10), AIC=35220.8, MSE=1114154
lm_Chao_ns_water_TL <- lm(Chao1 ~ Temperature*Latitude, data = Chaofunc_ns_water)
summary(lm_Chao_ns_water_TL)
vif(lm_Chao_ns_water_TL, merge_coef=fause)
lm_Chao_ns_water_TL.step <- step(lm_Chao_ns_water_TL, direction = "backward")
mean(lm_Chao_ns_water_TL$residuals^2)

#linear model without Temperature(*: Latitude)
#R2=0.1289, VIF(ALL > 10), AIC=35270.38, MSE=1136210
lm_Chao_ns_water_pL <- lm(Chao1 ~ , data = Chaofunc_ns_water)
summary(lm_Chao_ns_water_pL)
vif(lm_Chao_ns_water_pL, merge_coef=fause)
lm_Chao_ns_water_pL.step <- step(lm_Chao_ns_water_pL, direction = "backward")
mean(lm_Chao_ns_water_pL$residuals^2)

#linear model only Temperature(***: (Intercept), **: T)
#R2=0.003247, AIC=35609.15, MSE=1301134
lm_Chao_ns_water_T <- lm(Chao1 ~ Temperature, data = Chaofunc_ns_water)
summary(lm_Chao_ns_water_T)
vif(lm_Chao_ns_water_T, merge_coef=fause)
lm_Chao_ns_water_T.step <- step(lm_Chao_ns_water_T, direction = "backward")
mean(lm_Chao_ns_water_T$residuals^2)

#linear model only pH(***: Intercept)
#R2=-0.0000822, AIC=35617.58(AIC=35616.37, ~1), MSE=1305479
lm_Chao_ns_water_p <- lm(Chao1 ~ , data = Chaofunc_ns_water)
summary(lm_Chao_ns_water_p)
vif(lm_Chao_ns_water_p, merge_coef=fause)
lm_Chao_ns_water_p.step <- step(lm_Chao_ns_water_p, direction = "backward")
mean(lm_Chao_ns_water_p$residuals^2)

#linear model only Latitude(***: All)
#R2=0.1295, AIC=35266.7, MSE=1136356
lm_Chao_ns_water_L <- lm(Chao1 ~ Latitude, data = Chaofunc_ns_water)
summary(lm_Chao_ns_water_L)
vif(lm_Chao_ns_water_L, merge_coef=fause)
lm_Chao_ns_water_L.step <- step(lm_Chao_ns_water_L, direction = "backward")
mean(lm_Chao_ns_water_L$residuals^2)

#Observed OTUs Linear models of non-saline water samples
OOfunc_ns_water <- OOfunc[which(OOfunc$Location == 'Water (ns)'), ]

#1.All samples' linear model
#linear model without location(***: Temperature:pH:Latitude, **: Temperature, Temperature:pH, Temperature:Latitude, pH:Latitude, *: pH, (Intercept))
#R2=0.1665, VIF(ALL > 10), AIC=31099.97, MSE=217730.4
lm_OO_ns_water_TpL <- lm(OO~ Temperature*pH*Latitude, data = OOfunc_ns_water)
summary(lm_OO_ns_water_TpL)
vif(lm_OO_ns_water_TpL)
lm_OO_ns_water_TpL.step <- step(lm_OO_ns_water_TpL, direction = "backward")
mean(lm_OO_ns_water_TpL$residuals^2)

#linear model without latitude(**: Intercept)
#R2=0.004296, VIF(p < 10), AIC=31545.56(AIC=31542.01, ~T), MSE=260502.1
lm_OO_ns_water_Tp <- lm(OO ~ , data = OOfunc_ns_water)
summary(lm_OO_ns_water_Tp)
vif(lm_OO_ns_water_Tp, merge_coef=fause)
lm_OO_ns_water_Tp.step <- step(lm_OO_ns_water_Tp, direction = "backward")
mean(lm_OO_ns_water_Tp$residuals^2)

#linear model without pH(***:ALL)
#R2=0.1363, VIF(L < 10), AIC=31185.78, MSE=225958.7
lm_OO_ns_water_TL <- lm(OO ~ Temperature*Latitude, data = OOfunc_ns_water)
summary(lm_OO_ns_water_TL)
vif(lm_OO_ns_water_TL, merge_coef=fause)
lm_OO_ns_water_TL.step <- step(lm_OO_ns_water_TL, direction = "backward")
mean(lm_OO_ns_water_TL$residuals^2)

#linear model without Temperature(*: L, .: (Intercept))
#R2=0.1164 , VIF(ALL > 10), AIC=31242.44, MSE=231261.1
lm_OO_ns_water_pL <- lm(OO ~ , data = OOfunc_ns_water)
summary(lm_OO_ns_water_pL)
vif(lm_OO_ns_water_pL, merge_coef=fause)
lm_OO_ns_water_pL.step <- step(lm_OO_ns_water_pL, direction = "backward")
mean(lm_OO_ns_water_pL$residuals^2)

#linear model only Temperature(***: All)
#R2=0.004906, AIC=31542.01, MSE=260548.6
lm_OO_ns_water_T <- lm(OO ~ Temperature, data = OOfunc_ns_water)
summary(lm_OO_ns_water_T)
lm_OO_ns_water_T.step <- step(lm_OO_ns_water_T, direction = "backward")
mean(lm_OO_ns_water_T$residuals^2)

#linear model only pH(***: Intercept)
#R2=-0.0003907, AIC=31555.43(AIC=31553.45, ~1), MSE=261935.5
lm_OO_ns_water_p <- lm(OO ~, data = OOfunc_ns_water)
summary(lm_OO_ns_water_p)
lm_OO_ns_water_p.step <- step(lm_OO_ns_water_p, direction = "backward")
mean(lm_OO_ns_water_p$residuals^2)

#linear model only Latitude(***: L,(Intercept))
#R2=0.116, AIC=31242.76, MSE=231473
lm_OO_ns_water_L <- lm(OO ~ Latitude, data = OOfunc_ns_water)
summary(lm_OO_ns_water_L)
lm_OO_ns_water_L.step <- step(lm_OO_ns_water_L, direction = "backward")
mean(lm_OO_ns_water_L$residuals^2)


#Shannon Index Linear models of non-saline water samples
shanfunc_ns_water <- shanfunc[which(shanfunc$Location == 'Water (ns)'), ]

#1.All samples' linear model
#linear model without location(***: Temperature:pH:Latitude, **: Temperature, Temperature:pH, Temperature:Latitude, pH:Latitude, *: pH, (Intercept))
#R2=0.2236, VIF(ALL > 10), AIC=1963.4, MSE=2.159829
lm_shan_ns_water_TpL <- lm(Shannon~ Temperature*pH*Latitude, data = shanfunc_ns_water)
summary(lm_shan_ns_water_TpL)
vif(lm_shan_ns_water_TpL)
lm_shan_ns_water_TpL.step <- step(lm_shan_ns_water_TpL, direction = "backward")
mean(lm_shan_ns_water_TpL$residuals^2)

#linear model without latitude(***: Intercept)
#R2=0.01416, VIF(p < 10), AIC=2564.46,(AIC=2562.52, ~T+p), MSE=2.74802
lm_shan_ns_water_Tp <- lm(Shannon ~ Temperature+pH, data = shanfunc_ns_water)
summary(lm_shan_ns_water_Tp)
vif(lm_shan_ns_water_Tp, merge_coef=fause)
lm_shan_ns_water_Tp.step <- step(lm_shan_ns_water_Tp, direction = "backward")
mean(lm_shan_ns_water_Tp$residuals^2)

#linear model without pH(***: L, *:T:L)
#R2=0.2064, VIF(L < 10), AIC=2015.07, MSE=2.211393
lm_shan_ns_water_TL <- lm(Shannon ~ Temperature*Latitude, data = shanfunc_ns_water)
summary(lm_shan_ns_water_TL)
vif(lm_shan_ns_water_TL, merge_coef=fause)
lm_shan_ns_water_TL.step <- step(lm_shan_ns_water_TL, direction = "backward")
mean(lm_shan_ns_water_TL$residuals^2)

#linear model without Temperature(***: p, L, **: p:L, (Intercept))
#R2=0.1962, VIF(ALL > 10), AIC=2047.34, MSE=2.239786
lm_shan_ns_water_pL <- lm(Shannon ~ pH*Latitude, data = shanfunc_ns_water)
summary(lm_shan_ns_water_pL)
vif(lm_shan_ns_water_pL, merge_coef=fause)
lm_shan_ns_water_pL.step <- step(lm_shan_ns_water_pL, direction = "backward")
mean(lm_shan_ns_water_pL$residuals^2)

#linear model only Temperature(***: All)
#R2=0.0103, AIC=2571.42, MSE=2.759892
lm_shan_ns_water_T <- lm(Shannon ~ Temperature, data = shanfunc_ns_water)
summary(lm_shan_ns_water_T)
lm_shan_ns_water_T.step <- step(lm_shan_ns_water_T, direction = "backward")
mean(lm_shan_ns_water_T$residuals^2)

#linear model only pH(***: Intercept, **: pH)
#R2=0.003669, AIC=2588.3, MSE=2.778372
lm_shan_ns_water_p <- lm(Shannon ~ pH, data = shanfunc_ns_water)
summary(lm_shan_ns_water_p)
lm_shan_ns_water_p.step <- step(lm_shan_ns_water_p, direction = "backward")
mean(lm_shan_ns_water_p$residuals^2)

#linear model only Latitude(***: L, .:(Intercept))
#R2=0.1854, AIC=2078.97, MSE=2.27157
lm_shan_ns_water_L <- lm(Shannon ~ Latitude, data = shanfunc_ns_water)
summary(lm_shan_ns_water_L)
lm_shan_ns_water_L.step <- step(lm_shan_ns_water_L, direction = "backward")
mean(lm_shan_ns_water_L$residuals^2)


#Chao1 Index Linear models of non-saline surface samples
Chaofunc_ns_surf <- Chaofunc[which(Chaofunc$Location == 'Surface (ns)'), ]

#1.All samples' linear model
#linear model without location(***: Temperature:pH:Latitude, **: Temperature, Temperature:pH, Temperature:Latitude, pH:Latitude, *: pH, (Intercept))
#R2=0.6096, VIF(ALL > 10), AIC=1694.68, MSE=5478.734
lm_Chao_ns_surf_TpL <- lm(Chao1~ Temperature*pH*Latitude, data = Chaofunc_ns_surf)
summary(lm_Chao_ns_surf_TpL)
vif(lm_Chao_ns_surf_TpL)
lm_Chao_ns_surf_TpL.step <- step(lm_Chao_ns_surf_TpL, direction = "backward")
mean(lm_Chao_ns_surf_TpL$residuals^2)

#linear model without latitude(***: All)
#R2=0.4078, VIF(p < 10), AIC=1772.06, MSE=8488.529
lm_Chao_ns_surf_Tp <- lm(Chao1 ~ Temperature*pH, data = Chaofunc_ns_surf)
summary(lm_Chao_ns_surf_Tp)
vif(lm_Chao_ns_surf_Tp, merge_coef=fause)
lm_Chao_ns_surf_Tp.step <- step(lm_Chao_ns_surf_Tp, direction = "backward")
mean(lm_Chao_ns_surf_Tp$residuals^2)

#linear model without pH()
#R2=0.237, VIF(ALL > 10), AIC=1821.47(AIC=1818.66, ~T), MSE=10936.23
lm_Chao_ns_surf_TL <- lm(Chao1 ~ , data = Chaofunc_ns_surf)
summary(lm_Chao_ns_surf_TL)
vif(lm_Chao_ns_surf_TL, merge_coef=fause)
lm_Chao_ns_surf_TL.step <- step(lm_Chao_ns_surf_TL, direction = "backward")
mean(lm_Chao_ns_surf_TL$residuals^2)

#linear model without Temperature( **: L)
#R2=0.2897, VIF(ALL > 10), AIC=(AIC=1806.51,~p+L), MSE=10233.37
lm_Chao_ns_surf_pL <- lm(Chao1 ~ pH+Latitude, data = Chaofunc_ns_surf)
summary(lm_Chao_ns_surf_pL)
vif(lm_Chao_ns_surf_pL, merge_coef=fause)
lm_Chao_ns_surf_pL.step <- step(lm_Chao_ns_surf_pL, direction = "backward")
mean(lm_Chao_ns_surf_pL$residuals^2)

#linear model only Temperature(***: All)
#R2=0.2403, AIC=1818.66, MSE=11003.52
lm_Chao_ns_surf_T <- lm(Chao1 ~ Temperature, data = Chaofunc_ns_surf)
summary(lm_Chao_ns_surf_T)
lm_Chao_ns_surf_T.step <- step(lm_Chao_ns_surf_T, direction = "backward")
mean(lm_Chao_ns_surf_T$residuals^2)

#linear model only pH( **: pH,*:(Intercept))
#R2=0.038, AIC=1864.69, MSE=13932.8
lm_Chao_ns_surf_p <- lm(Chao1 ~ pH, data = Chaofunc_ns_surf)
summary(lm_Chao_ns_surf_p)
lm_Chao_ns_surf_p.step <- step(lm_Chao_ns_surf_p, direction = "backward")
mean(lm_Chao_ns_surf_p$residuals^2)

#linear model only Latitude(***: L, .:(Intercept))
#R2=-0.004045, AIC=1873.03(AIC=1871.25, ~1), MSE=14541.7
lm_Chao_ns_surf_L <- lm(Chao1 ~ Latitude, data = Chaofunc_ns_surf)
summary(lm_Chao_ns_surf_L)
lm_Chao_ns_surf_L.step <- step(lm_Chao_ns_surf_L, direction = "backward")
mean(lm_Chao_ns_surf_L$residuals^2)


#Observed OTUs Linear models of non-saline surface samples
OOfunc_ns_surf <- OOfunc[which(OOfunc$Location == 'Surface (ns)'), ]

#1.All samples' linear model
#linear model without location(***: All)
#R2=0.5749, VIF(ALL > 10), AIC=1639.48, MSE=4127.933
lm_OO_ns_surf_TpL <- lm(OO~ Temperature*pH*Latitude, data = OOfunc_ns_surf)
summary(lm_OO_ns_surf_TpL)
vif(lm_OO_ns_surf_TpL)
lm_OO_ns_surf_TpL.step <- step(lm_OO_ns_surf_TpL, direction = "backward")
mean(lm_OO_ns_surf_TpL$residuals^2)

#linear model without latitude(***: All)
#R2=0.3603, VIF(p < 10), AIC=1715.3, MSE=6344.755
lm_OO_ns_surf_Tp <- lm(OO ~ Temperature*pH, data = OOfunc_ns_surf)
summary(lm_OO_ns_surf_Tp)
vif(lm_OO_ns_surf_Tp, merge_coef=fause)
lm_OO_ns_surf_Tp.step <- step(lm_OO_ns_surf_Tp, direction = "backward")
mean(lm_OO_ns_surf_Tp$residuals^2)

#linear model without pH(***: L, *:T:L)
#R2=0.2031, VIF(L < 10), AIC=1758.16(AIC=1755.72, ~T), MSE=7904.524
lm_OO_ns_surf_TL <- lm(OO ~ , data = OOfunc_ns_surf)
summary(lm_OO_ns_surf_TL)
vif(lm_OO_ns_surf_TL, merge_coef=fause)
lm_OO_ns_surf_TL.step <- step(lm_OO_ns_surf_TL, direction = "backward")
mean(lm_OO_ns_surf_TL$residuals^2)

#linear model without Temperature( **: L)
#R2=0.2448, VIF(ALL > 10), AIC=(AIC=1746.7, ~p+L), MSE=7530.028
lm_OO_ns_surf_pL <- lm(OO ~ pH+Latitude, data = OOfunc_ns_surf)
summary(lm_OO_ns_surf_pL)
vif(lm_OO_ns_surf_pL, merge_coef=fause)
lm_OO_ns_surf_pL.step <- step(lm_OO_ns_surf_pL, direction = "backward")
mean(lm_OO_ns_surf_pL$residuals^2)

#linear model only Temperature(***: All)
#R2=0.205, AIC=1755.72, MSE=7967.891
lm_OO_ns_surf_T <- lm(OO ~ Temperature, data = OOfunc_ns_surf)
summary(lm_OO_ns_surf_T)
lm_OO_ns_surf_T.step <- step(lm_OO_ns_surf_T, direction = "backward")
mean(lm_OO_ns_surf_T$residuals^2)

#linear model only pH( **: pH, .:(Intercept))
#R2=0.03848, AIC=1792.81, MSE=9637.021
lm_OO_ns_surf_p <- lm(OO ~ pH, data = OOfunc_ns_surf)
summary(lm_OO_ns_surf_p)
lm_OO_ns_surf_p.step <- step(lm_OO_ns_surf_p, direction = "backward")
mean(lm_OO_ns_surf_p$residuals^2)

#linear model only Latitude()
#R2=-0.002689, AIC=1800.98(AIC=1799.47, ~1), MSE=10049.61
lm_OO_ns_surf_L <- lm(OO ~ , data = OOfunc_ns_surf)
summary(lm_OO_ns_surf_L)
lm_OO_ns_surf_L.step <- step(lm_OO_ns_surf_L, direction = "backward")
mean(lm_OO_ns_surf_L$residuals^2)


#Shannon Index Linear models of non-saline surface samples
shanfunc_ns_surf <- shanfunc[which(shanfunc$Location == 'Surface (ns)'), ]

#1.All samples' linear model
#linear model without location(***: ALL)
#R2=0.4957, VIF(ALL > 10), AIC=121.08, MSE=0.4951202
lm_shan_ns_surf_TpL <- lm(Shannon~ Temperature*pH*Latitude, data = shanfunc_ns_surf)
summary(lm_shan_ns_surf_TpL)
vif(lm_shan_ns_surf_TpL)
lm_shan_ns_surf_TpL.step <- step(lm_shan_ns_surf_TpL, direction = "backward")
mean(lm_shan_ns_surf_TpL$residuals^2)

#linear model without latitude(***: ALL)
#R2=0.273, VIF(p < 10), AIC=-53.63, MSE=0.7290274
lm_shan_ns_surf_Tp <- lm(Shannon ~ Temperature*pH, data = shanfunc_ns_surf)
summary(lm_shan_ns_surf_Tp)
vif(lm_shan_ns_surf_Tp, merge_coef=fause)
lm_shan_ns_surf_Tp.step <- step(lm_shan_ns_surf_Tp, direction = "backward")
mean(lm_shan_ns_surf_Tp$residuals^2)

#linear model without pH()
#R2=0.1428, VIF(L < 10), AIC=-21.51(AIC=-22.58, ~T+L), MSE=0.8595423
lm_shan_ns_surf_TL <- lm(Shannon ~ , data = shanfunc_ns_surf)
summary(lm_shan_ns_surf_TL)
vif(lm_shan_ns_surf_TL, merge_coef=fause)
lm_shan_ns_surf_TL.step <- step(lm_shan_ns_surf_TL, direction = "backward")
mean(lm_shan_ns_surf_TL$residuals^2)

#linear model without Temperature()
#R2=0.1716, VIF(ALL > 10), AIC=-29.16(AIC=-29.16, ~p+L), MSE=0.8350172
lm_shan_ns_surf_pL <- lm(Shannon ~ pH+Latitude, data = shanfunc_ns_surf)
summary(lm_shan_ns_surf_pL)
vif(lm_shan_ns_surf_pL, merge_coef=fause)
lm_shan_ns_surf_pL.step <- step(lm_shan_ns_surf_pL, direction = "backward")
mean(lm_shan_ns_surf_pL$residuals^2)

#linear model only Temperature(***: All)
#R2=0.1373, AIC=-22.24, MSE=0.8741111
lm_shan_ns_surf_T <- lm(Shannon ~ Temperature, data = shanfunc_ns_surf)
summary(lm_shan_ns_surf_T)
lm_shan_ns_surf_T.step <- step(lm_shan_ns_surf_T, direction = "backward")
mean(lm_shan_ns_surf_T$residuals^2)

#linear model only pH( **: pH)
#R2=0.03825, AIC=-1.04, MSE=0.9745058
lm_shan_ns_surf_p <- lm(Shannon ~ pH, data = shanfunc_ns_surf)
summary(lm_shan_ns_surf_p)
lm_shan_ns_surf_p.step <- step(lm_shan_ns_surf_p, direction = "backward")
mean(lm_shan_ns_surf_p$residuals^2)

#linear model only Latitude(**:(Intercept))
#R2=0.0008445, AIC=6.41(AIC=5.58, ~1), MSE=1.012411
lm_shan_ns_surf_L <- lm(Shannon ~ , data = shanfunc_ns_surf)
summary(lm_shan_ns_surf_L)
lm_shan_ns_surf_L.step <- step(lm_shan_ns_surf_L, direction = "backward")
mean(lm_shan_ns_surf_L$residuals^2)


#Chao1 Index Linear models of saline sediment samples
Chaofunc_s_sedi <- Chaofunc[which(Chaofunc$Location == 'Sediment (s)'), ]

#1.All samples' linear model
#linear model without location(***: ALL)
#R2=0.5044, VIF(ALL > 10), AIC=(AIC=1856.2, ~T+p+L+T:L), MSE=284704.7
lm_Chao_s_sedi_TpL <- lm(Chao1~ Temperature + pH + Latitude + Temperature:Latitude, data = Chaofunc_s_sedi)
summary(lm_Chao_s_sedi_TpL)
vif(lm_Chao_s_sedi_TpL)
lm_Chao_s_sedi_TpL.step <- step(lm_Chao_s_sedi_TpL, direction = "backward")
mean(lm_Chao_s_sedi_TpL$residuals^2)

#linear model without latitude(***: NULL)(NULL)
#R2=0.01739, VIF(), AIC=1955.86, MSE=568499.1
lm_Chao_s_sedi_Tp <- lm(Chao1 ~, data = Chaofunc_s_sedi)
summary(lm_Chao_s_sedi_Tp)
vif(lm_Chao_s_sedi_Tp, merge_coef=fause)
lm_Chao_s_sedi_Tp.step <- step(lm_Chao_s_sedi_Tp, direction = "backward")
mean(lm_Chao_s_sedi_Tp$residuals^2)

#linear model without pH()(NULL)
#R2=0.4482, VIF(L < 10), AIC=1871.04(1868.85, ~L), MSE=319263.9
lm_Chao_s_sedi_TL <- lm(Chao1 ~ , data = Chaofunc_s_sedi)
summary(lm_Chao_s_sedi_TL)
vif(lm_Chao_s_sedi_TL, merge_coef=fause)
lm_Chao_s_sedi_TL.step <- step(lm_Chao_s_sedi_TL, direction = "backward")
mean(lm_Chao_s_sedi_TL$residuals^2)

#linear model without Temperature()
#R2=0.5016, VIF(ALL > 10), AIC=1856.09, MSE=288380.8
lm_Chao_s_sedi_pL <- lm(Chao1 ~ pH*Latitude, data = Chaofunc_s_sedi)
summary(lm_Chao_s_sedi_pL)
vif(lm_Chao_s_sedi_pL, merge_coef=fause)
lm_Chao_s_sedi_pL.step <- step(lm_Chao_s_sedi_pL, direction = "backward")
mean(lm_Chao_s_sedi_pL$residuals^2)

#linear model only Temperature(***: All)
#R2=0.01735, AIC=1953.91, MSE=576472.1
lm_Chao_s_sedi_T <- lm(Chao1 ~ Temperature, data = Chaofunc_s_sedi)
summary(lm_Chao_s_sedi_T)
lm_Chao_s_sedi_T.step <- step(lm_Chao_s_sedi_T, direction = "backward")
mean(lm_Chao_s_sedi_T$residuals^2)

#linear model only pH( **: pH)
#R2=-0.005535, AIC=1957.29(AIC=1955.49, ~1), MSE=589898.1
lm_Chao_s_sedi_p <- lm(Chao1 ~ pH, data = Chaofunc_s_sedi)
summary(lm_Chao_s_sedi_p)
lm_Chao_s_sedi_p.step <- step(lm_Chao_s_sedi_p, direction = "backward")
mean(lm_Chao_s_sedi_p$residuals^2)

#linear model only Latitude(**:(Intercept))
#R2=0.449, AIC=1868.85, MSE=323218.7
lm_Chao_s_sedi_L <- lm(Chao1 ~ Latitude, data = Chaofunc_s_sedi)
summary(lm_Chao_s_sedi_L)
lm_Chao_s_sedi_L.step <- step(lm_Chao_s_sedi_L, direction = "backward")
mean(lm_Chao_s_sedi_L$residuals^2)


#Observed OTUs Linear models of saline sediment samples
OOfunc_s_sedi <- OOfunc[which(OOfunc$Location == 'Sediment (s)'), ]

#1.All samples' linear model
#linear model without location(***: )
#R2=0.578, VIF(ALL > 10), AIC=1619.2, MSE= 56781.8
lm_OO_s_sedi_TpL <- lm(OO~ Temperature + pH + Latitude + pH:Latitude, data = OOfunc_s_sedi)
summary(lm_OO_s_sedi_TpL)
vif(lm_OO_s_sedi_TpL)
lm_OO_s_sedi_TpL.step <- step(lm_OO_s_sedi_TpL, direction = "backward")
mean(lm_OO_s_sedi_TpL$residuals^2)

#linear model without latitude(***: ALL)
#R2=0.01269, VIF(p < 10), AIC=1743.18(AIC=1740.94, ~T), MSE=133778.9
lm_OO_s_sedi_Tp <- lm(OO ~ , data = OOfunc_s_sedi)
summary(lm_OO_s_sedi_Tp)
vif(lm_OO_s_sedi_Tp, merge_coef=fause)
lm_OO_s_sedi_Tp.step <- step(lm_OO_s_sedi_Tp, direction = "backward")
mean(lm_OO_s_sedi_Tp$residuals^2)

#linear model without pH()
#R2=0.5349, VIF(L < 10), AIC=1632.53(AIC=1631.5, ~T+L), MSE=63022.78
lm_OO_s_sedi_TL <- lm(OO ~ , data = OOfunc_s_sedi)
summary(lm_OO_s_sedi_TL)
vif(lm_OO_s_sedi_TL, merge_coef=fause)
lm_OO_s_sedi_TL.step <- step(lm_OO_s_sedi_TL, direction = "backward")
mean(lm_OO_s_sedi_TL$residuals^2)

#linear model without Temperature()
#R2=0.5717, VIF(ALL > 10), AIC=1620.42, MSE=58037.6
lm_OO_s_sedi_pL <- lm(OO ~ pH*Latitude, data = OOfunc_s_sedi)
summary(lm_OO_s_sedi_pL)
vif(lm_OO_s_sedi_pL, merge_coef=fause)
lm_OO_s_sedi_pL.step <- step(lm_OO_s_sedi_pL, direction = "backward")
mean(lm_OO_s_sedi_pL$residuals^2)

#linear model only Temperature(***: All)
#R2=0.01459, AIC=1740.94, MSE=135388.8
lm_OO_s_sedi_T <- lm(OO ~ Temperature, data = OOfunc_s_sedi)
summary(lm_OO_s_sedi_T)
lm_OO_s_sedi_T.step <- step(lm_OO_s_sedi_T, direction = "backward")
mean(lm_OO_s_sedi_T$residuals^2)

#linear model only pH( **: pH)
#R2=-0.006856, AIC=1744.1(AIC=1742.11, ~1), MSE=138335.7
lm_OO_s_sedi_p <- lm(OO ~ , data = OOfunc_s_sedi)
summary(lm_OO_s_sedi_p)
lm_OO_s_sedi_p.step <- step(lm_OO_s_sedi_p, direction = "backward")
mean(lm_OO_s_sedi_p$residuals^2)

#linear model only Latitude(**:(Intercept))
#R2=0.5302, AIC=1632.05, MSE=64547.2
lm_OO_s_sedi_L <- lm(OO ~ Latitude, data = OOfunc_s_sedi)
summary(lm_OO_s_sedi_L)
lm_OO_s_sedi_L.step <- step(lm_OO_s_sedi_L, direction = "backward")
mean(lm_OO_s_sedi_L$residuals^2)


#Shannon Index Linear models of saline sediment samples
Shanfunc_s_sedi <- Shanfunc[which(Shanfunc$Location == 'Sediment (s)'), ]

#1.All samples' linear model
#linear model without location(***: ALL)
#R2=0.4116, VIF(ALL > 10), AIC=163.59(AIC=160.18, ~T+p+L+p:L), MSE=2.766586
lm_Shan_s_sedi_TpL <- lm(Shannon~ , data = Shanfunc_s_sedi)
summary(lm_Shan_s_sedi_TpL)
vif(lm_Shan_s_sedi_TpL)
lm_Shan_s_sedi_TpL.step <- step(lm_Shan_s_sedi_TpL, direction = "backward")
mean(lm_Shan_s_sedi_TpL$residuals^2)

#linear model without latitude(***: ALL)
#R2=0.006927, VIF(p < 10), AIC=237.63(AIC=235.71, ~1), MSE=4.768983
lm_Shan_s_sedi_Tp <- lm(Shannon ~ , data = Shanfunc_s_sedi)
summary(lm_Shan_s_sedi_Tp)
vif(lm_Shan_s_sedi_Tp, merge_coef=fause)
lm_Shan_s_sedi_Tp.step <- step(lm_Shan_s_sedi_Tp, direction = "backward")
mean(lm_Shan_s_sedi_Tp$residuals^2)

#linear model without pH()
#R2=0.3964, VIF(L < 10), AIC=164.44(AIC=163.95, ~L), MSE=2.898521
lm_Shan_s_sedi_TL <- lm(Shannon ~ , data = Shanfunc_s_sedi)
summary(lm_Shan_s_sedi_TL)
vif(lm_Shan_s_sedi_TL, merge_coef=fause)
lm_Shan_s_sedi_TL.step <- step(lm_Shan_s_sedi_TL, direction = "backward")
mean(lm_Shan_s_sedi_TL$residuals^2)

#linear model without Temperature()
#R2=0.4109, VIF(ALL > 10), AIC=160.86, MSE=2.828816
lm_Shan_s_sedi_pL <- lm(Shannon ~ pH*Latitude, data = Shanfunc_s_sedi)
summary(lm_Shan_s_sedi_pL)
vif(lm_Shan_s_sedi_pL, merge_coef=fause)
lm_Shan_s_sedi_pL.step <- step(lm_Shan_s_sedi_pL, direction = "backward")
mean(lm_Shan_s_sedi_pL$residuals^2)

#linear model only Temperature(***: All)
#R2=0.006607, AIC=235.72(AIC=235.71, ~1), MSE=4.83724
lm_Shan_s_sedi_T <- lm(Shannon ~ , data = Shanfunc_s_sedi)
summary(lm_Shan_s_sedi_T)
lm_Shan_s_sedi_T.step <- step(lm_Shan_s_sedi_T, direction = "backward")
mean(lm_Shan_s_sedi_T$residuals^2)

#linear model only pH( **: pH)
#R2=0.002297, AIC=236.36(AIC=235.71, ~1), MSE=4.858225
lm_Shan_s_sedi_p <- lm(Shannon ~ , data = Shanfunc_s_sedi)
summary(lm_Shan_s_sedi_p)
lm_Shan_s_sedi_p.step <- step(lm_Shan_s_sedi_p, direction = "backward")
mean(lm_Shan_s_sedi_p$residuals^2)

#linear model only Latitude(**:(Intercept))
#R2=0.3903, AIC=163.95, MSE=2.968694
lm_Shan_s_sedi_L <- lm(Shannon ~ Latitude, data = Shanfunc_s_sedi)
summary(lm_Shan_s_sedi_L)
lm_Shan_s_sedi_L.step <- step(lm_Shan_s_sedi_L, direction = "backward")
mean(lm_Shan_s_sedi_L$residuals^2)


