rm(list = ls())
#loading package

library(car)
Chaofunc <- read.csv("C:/Users/lenovo/Desktop/Chao_envfactors/Func_Chao_T_pH_lati.csv", stringsAsFactors = T)
OOfunc <- read.csv("C:/Users/lenovo/Desktop/OO_envfactors/Func_OO_T_pH_lati.csv", stringsAsFactors = T)
Shanfunc <- read.csv("C:/Users/lenovo/Desktop/Shan_envfactors/Func_Shan_T_pH_lati.csv", stringsAsFactors = T)


#Chao1 Index Polynomial regression models of non-saline water samples 
chaofunc_ns_water <- Chaofunc[which(Chaofunc$Location == 'Water (ns)'), ]

#1.All samples' Polynomial Regression model
#Polynomial Regression model without location(***: , .: )
#R2=0.1957, VIF(All > 10), AIC=35076.7, MSE=1045809
pm_chao_ns_water_TpL <- lm(Chao1~ poly(Temperature , 3) + poly(pH, 1) + poly(Latitude, 3) + Temperature:pH + Temperature:Latitude + pH:Latitude + Temperature:pH:Latitude, data = chaofunc_ns_water)
summary(pm_chao_ns_water_TpL)
vif(pm_chao_ns_water_TpL)
pm_chao_ns_water_TpL.step <- step(pm_chao_ns_water_TpL, direction = "backward")
mean(pm_chao_ns_water_TpL$residuals^2)

#Polynomial Regression without latitude(***: )
#R2=0.1027, VIF(pH < 10), AIC=35348.33, MSE=1169000
pm_chao_ns_water_Tp <- lm(Chao1~ poly(Temperature, 3) + poly(pH, 3), data = chaofunc_ns_water)
summary(pm_chao_ns_water_Tp)
pm_chao_ns_water_Tp.step <- step(pm_chao_ns_water_Tp, direction = "backward")
vif(pm_chao_ns_water_Tp, merge_coef=fause)
mean(pm_chao_ns_water_Tp$residuals^2)

#Polynomial Regression without pH(***: )
#R2=0.1663, VIF( < 10), AIC=35163.51, MSE=1085759
pm_chao_ns_water_TL <- lm(Chao1~ poly(Temperature, 3) + poly(Latitude, 3) + Temperature:Latitude, data = chaofunc_ns_water)
summary(pm_chao_ns_water_TL)
pm_chao_ns_water_TL.step <- step(pm_chao_ns_water_TL, direction = "backward")
vif(pm_chao_ns_water_TL, merge_coef=fause)
mean(pm_chao_ns_water_TL$residuals^2)

#Polynomial Regression without Temperature(*: )
#R2=0.1344, VIF( > 10), AIC=35257.44, MSE=1127734
pm_chao_ns_water_pL <- lm(Chao1~ poly(pH, 3) + poly(Latitude, 3), data = chaofunc_ns_water)
summary(pm_chao_ns_water_pL)
pm_chao_ns_water_pL.step <- step(pm_chao_ns_water_pL, direction = "backward")
vif(pm_chao_ns_water_pL, merge_coef=fause)
mean(pm_chao_ns_water_pL$residuals^2)


#Polynomial Regression only Temperature(***: All)
#R2=0.06321, VIF(Null), AIC=35454.23, MSE=1221886
pm_chao_ns_water_T <- lm(Chao1~ poly(Temperature, 3), data = chaofunc_ns_water)
summary(pm_chao_ns_water_T)
pm_chao_ns_water_T_1.step <- step(pm_chao_ns_water_T, direction = "backward")
vif(pm_chao_ns_water_T, merge_coef=fause)
mean(pm_chao_ns_water_T$residuals^2)

#Polynomial Regression model only pH(***: Intercept, **: pH)
#R2=0.06568, VIF(Null), AIC=35447.55, MSE=1218665
pm_chao_ns_water_p <- lm(Chao1~ poly(pH, 3), data = chaofunc_ns_water)
summary(pm_chao_ns_water_p)
pm_chao_ns_water_p.step <- step(pm_chao_ns_water_p, direction = "backward")
vif(pm_chao_ns_water_p, merge_coef=fause)
mean(pm_chao_ns_water_p$residuals^2)

#Polynomial Regression model only Latitude(***: All)
#R2=0.1327, VIF(Null),AIC=35259.27, MSE=1131230
pm_chao_ns_water_L <- lm(Chao1~ poly(Latitude, 3), data = chaofunc_ns_water)
summary(pm_chao_ns_water_L)
pm_chao_ns_water_L.step <- step(pm_chao_ns_water_L, direction = "backward")
vif(pm_chao_ns_water_L, merge_coef=fause)
mean(pm_chao_ns_water_L$residuals^2)


#Observed OTUs Polynomial regression models of non-saline water samples 
OOfunc_ns_water <- OOfunc[which(OOfunc$Location == 'Water (ns)'), ]

#1.All samples' Polynomial Regression model
#Polynomial Regression model without location(***: , .: )
#R2=0.1829, VIF(All > 10), AIC=31053.67, MSE=213105.3
pm_OO_ns_water_TpL <- lm(OO~ poly(Temperature , 3) + poly(pH, 1) + poly(Latitude, 3)+Temperature:pH + Temperature:Latitude + pH:Latitude+Temperature:pH:Latitude, data = OOfunc_ns_water)
summary(pm_OO_ns_water_TpL)
vif(pm_OO_ns_water_TpL)
pm_OO_ns_water_TpL.step <- step(pm_OO_ns_water_TpL, direction = "backward")
mean(pm_OO_ns_water_TpL$residuals^2)

#Polynomial Regression without latitude(***: )
#R2=0.09218, VIF(pH < 10), AIC=31314.85, MSE=237226.2
pm_OO_ns_water_Tp <- lm(OO~ poly(Temperature, 3) + poly(pH, 3), data = OOfunc_ns_water)
summary(pm_OO_ns_water_Tp)
pm_OO_ns_water_Tp.step <- step(pm_OO_ns_water_Tp, direction = "backward")
vif(pm_OO_ns_water_Tp, merge_coef=fause)
mean(pm_OO_ns_water_Tp$residuals^2)

#Polynomial Regression without pH(***: )
#R2=0.1544 , VIF( < 10), AIC=31136.33, MSE=220883.3
pm_OO_ns_water_TL <- lm(OO~ poly(Temperature, 3) + poly(Latitude, 3)+ Temperature:Latitude, data = OOfunc_ns_water)
summary(pm_OO_ns_water_TL)
pm_OO_ns_water_TL.step <- step(pm_OO_ns_water_TL, direction = "backward")
vif(pm_OO_ns_water_TL, merge_coef=fause)
mean(pm_OO_ns_water_TL$residuals^2)

#Polynomial Regression without Temperature(*: )
#R2=0.1233, VIF( > 10), AIC=31226.76, MSE=229105.2
pm_OO_ns_water_pL <- lm(OO~ poly(pH, 3) + poly(Latitude, 3), data = OOfunc_ns_water)
summary(pm_OO_ns_water_pL)
pm_OO_ns_water_pL.step <- step(pm_OO_ns_water_pL, direction = "backward")
vif(pm_OO_ns_water_pL, merge_coef=fause)
mean(pm_OO_ns_water_pL$residuals^2)


#Polynomial Regression only Temperature(***: All)
#R2=0.05587, VIF(Null), AIC=31411.05, MSE=247009.5
pm_OO_ns_water_T <- lm(OO~ poly(Temperature, 3), data = OOfunc_ns_water)
summary(pm_OO_ns_water_T)
pm_OO_ns_water_T_1.step <- step(pm_OO_ns_water_T, direction = "backward")
mean(pm_OO_ns_water_T$residuals^2)

#Polynomial Regression model only pH(***: Intercept, **: pH)
#R2=0.06064, VIF(Null), AIC=31398.23, MSE=245760
pm_OO_ns_water_p <- lm(OO~ poly(pH, 3), data = OOfunc_ns_water)
summary(pm_OO_ns_water_p)
pm_OO_ns_water_p.step <- step(pm_OO_ns_water_p, direction = "backward")
mean(pm_OO_ns_water_p$residuals^2)

#Polynomial Regression model only Latitude(***: All)
#R2=0.1206, VIF(Null),AIC=31231.4, MSE=230071.2
pm_OO_ns_water_L <- lm(OO~ poly(Latitude, 3), data = OOfunc_ns_water)
summary(pm_OO_ns_water_L)
pm_OO_ns_water_L.step <- step(pm_OO_ns_water_L, direction = "backward")
mean(pm_OO_ns_water_L$residuals^2)


#Shannon Index Polynomial regression models of non-saline water samples 
Shanfunc_ns_water <- Shanfunc[which(Shanfunc$Location == 'Water (ns)'), ]

#1.All samples' Polynomial Regression model
#Polynomial Regression model without location(***: , .: )
#R2=0.2355, VIF(All > 10), AIC=1926.62, MSE=2.125275
pm_Shan_ns_water_TpL <- lm(Shannon~ poly(Temperature , 1) + poly(pH, 1) + poly(Latitude, 3)+Temperature:pH + Temperature:Latitude + pH:Latitude+Temperature:pH:Latitude, data = Shanfunc_ns_water)
summary(pm_Shan_ns_water_TpL)
vif(pm_Shan_ns_water_TpL)
pm_Shan_ns_water_TpL.step <- step(pm_Shan_ns_water_TpL, direction = "backward")
mean(pm_Shan_ns_water_TpL$residuals^2)

#Polynomial Regression without latitude(***: )
#R2=0.1145, VIF(pH < 10), AIC=2294.19, MSE=2.46553
pm_Shan_ns_water_Tp <- lm(Shannon~ poly(Temperature, 2) + poly(pH, 3), data = Shanfunc_ns_water)
summary(pm_Shan_ns_water_Tp)
pm_Shan_ns_water_Tp.step <- step(pm_Shan_ns_water_Tp, direction = "backward")
vif(pm_Shan_ns_water_Tp, merge_coef=fause)
mean(pm_Shan_ns_water_Tp$residuals^2)

#Polynomial Regression without pH(***: )
#R2=0.2165, VIF( < 10), AIC=1985.66, MSE=2.18064
pm_Shan_ns_water_TL <- lm(Shannon~ poly(Temperature, 2) + poly(Latitude, 3)+ Temperature:Latitude, data = Shanfunc_ns_water)
summary(pm_Shan_ns_water_TL)
pm_Shan_ns_water_TL.step <- step(pm_Shan_ns_water_TL, direction = "backward")
vif(pm_Shan_ns_water_TL, merge_coef=fause)
mean(pm_Shan_ns_water_TL$residuals^2)

#Polynomial Regression without Temperature(*: )
#R2=0.2064, VIF( > 10), AIC=2019.06, MSE=2.207885
pm_Shan_ns_water_pL <- lm(Shannon~ poly(pH, 3) + poly(Latitude, 3)+pH:Latitude, data = Shanfunc_ns_water)
summary(pm_Shan_ns_water_pL)
pm_Shan_ns_water_pL.step <- step(pm_Shan_ns_water_pL, direction = "backward")
vif(pm_Shan_ns_water_pL, merge_coef=fause)
mean(pm_Shan_ns_water_pL$residuals^2)


#Polynomial Regression only Temperature(***: All)
#R2=0.0522, VIF(Null), AIC=2463.02, MSE=2.642001
pm_Shan_ns_water_T <- lm(Shannon~ poly(Temperature, 2), data = Shanfunc_ns_water)
summary(pm_Shan_ns_water_T)
pm_Shan_ns_water_T_1.step <- step(pm_Shan_ns_water_T, direction = "backward")
mean(pm_Shan_ns_water_T$residuals^2)

#Polynomial Regression model only pH(***: Intercept, **: pH)
#R2=0.09646, VIF(Null), AIC=2343.07, MSE=2.517633
pm_Shan_ns_water_p <- lm(Shannon~ poly(pH, 3), data = Shanfunc_ns_water)
summary(pm_Shan_ns_water_p)
pm_Shan_ns_water_p.step <- step(pm_Shan_ns_water_p, direction = "backward")
mean(pm_Shan_ns_water_p$residuals^2)

#Polynomial Regression model only Latitude(***: All)
#R2=0.1961, VIF(Null),AIC=2047.63, MSE=2.240051
pm_Shan_ns_water_L <- lm(Shannon~ poly(Latitude, 3), data = Shanfunc_ns_water)
summary(pm_Shan_ns_water_L)
pm_Shan_ns_water_L.step <- step(pm_Shan_ns_water_L, direction = "backward")
mean(pm_Shan_ns_water_L$residuals^2)


#Chao1 Index Polynomial regression models of non-saline surface samples 
chaofunc_ns_surf <- Chaofunc[which(Chaofunc$Location == 'Surface (ns)'), ]

#1.All samples' Polynomial Regression model
#Polynomial Regression model without location(***: , .: )
#R2=0.6594, VIF(All > 10), AIC=1668.99, MSE=4753.432
pm_chao_ns_surf_TpL <- lm(Chao1~ poly(Temperature , 1) + poly(pH, 3) + poly(Latitude, 2) + Temperature:Latitude + pH:Latitude, data = chaofunc_ns_surf)
summary(pm_chao_ns_surf_TpL)
vif(pm_chao_ns_surf_TpL)
pm_chao_ns_surf_TpL.step <- step(pm_chao_ns_surf_TpL, direction = "backward")
mean(pm_chao_ns_surf_TpL$residuals^2)

#Polynomial Regression without latitude(***: )
#R2=0.4895, VIF(pH < 10), AIC=1746, MSE=7201.704
pm_chao_ns_surf_Tp <- lm(Chao1~ poly(Temperature, 3) + poly(pH, 2)+Temperature:pH, data = chaofunc_ns_surf)
summary(pm_chao_ns_surf_Tp)
pm_chao_ns_surf_Tp.step <- step(pm_chao_ns_surf_Tp, direction = "backward")
vif(pm_chao_ns_surf_Tp, merge_coef=fause)
mean(pm_chao_ns_surf_Tp$residuals^2)

#Polynomial Regression without pH(***: )
#R2=0.6011, VIF( < 10), AIC=1696.94, MSE=5657.283
pm_chao_ns_surf_TL <- lm(Chao1~ poly(Temperature, 3) + poly(Latitude, 2), data = chaofunc_ns_surf)
summary(pm_chao_ns_surf_TL)
pm_chao_ns_surf_TL.step <- step(pm_chao_ns_surf_TL, direction = "backward")
vif(pm_chao_ns_surf_TL, merge_coef=fause)
mean(pm_chao_ns_surf_TL$residuals^2)

#Polynomial Regression without Temperature(*: )
#R2=0.558, VIF( > 10), AIC=1717.93, MSE=6236.027
pm_chao_ns_surf_pL <- lm(Chao1~ poly(pH, 3) + poly(Latitude, 2)+pH:Latitude, data = chaofunc_ns_surf)
summary(pm_chao_ns_surf_pL)
pm_chao_ns_surf_pL.step <- step(pm_chao_ns_surf_pL, direction = "backward")
vif(pm_chao_ns_surf_pL, merge_coef=fause)
mean(pm_chao_ns_surf_pL$residuals^2)


#Polynomial Regression only Temperature(***: All)
#R2=0.4724, VIF(Null), AIC=1749.51, MSE=7561.41
pm_chao_ns_surf_T <- lm(Chao1~ poly(Temperature, 3), data = chaofunc_ns_surf)
summary(pm_chao_ns_surf_T)
pm_chao_ns_surf_T_1.step <- step(pm_chao_ns_surf_T, direction = "backward")
vif(pm_chao_ns_surf_T, merge_coef=fause)
mean(pm_chao_ns_surf_T$residuals^2)

#Polynomial Regression model only pH(***: Intercept, **: pH)
#R2=0.3264, VIF(Null), AIC=1797.15, MSE=9654.252
pm_chao_ns_surf_p <- lm(Chao1~ poly(pH, 3), data = chaofunc_ns_surf)
summary(pm_chao_ns_surf_p)
pm_chao_ns_surf_p.step <- step(pm_chao_ns_surf_p, direction = "backward")
vif(pm_chao_ns_surf_p, merge_coef=fause)
mean(pm_chao_ns_surf_p$residuals^2)

#Polynomial Regression model only Latitude(***: All)
#R2=0.285, VIF(Null),AIC=1807.8, MSE=10301.18
pm_chao_ns_surf_L <- lm(Chao1~ poly(Latitude, 2), data = chaofunc_ns_surf)
summary(pm_chao_ns_surf_L)
pm_chao_ns_surf_L.step <- step(pm_chao_ns_surf_L, direction = "backward")
vif(pm_chao_ns_surf_L, merge_coef=fause)
mean(pm_chao_ns_surf_L$residuals^2)


#Observed OTUs Polynomial regression models of non-saline surface samples 
OOfunc_ns_surf <- OOfunc[which(OOfunc$Location == 'Surface (ns)'), ]

#1.All samples' Polynomial Regression model
#Polynomial Regression model without location(***: , .: )
#R2=0.6241, VIF(All > 10), AIC=1616.47, MSE=3630.986
pm_OO_ns_surf_TpL <- lm(OO~ poly(Temperature , 1) + poly(pH, 3) + poly(Latitude, 2) + Temperature:Latitude + pH:Latitude, data = OOfunc_ns_surf)
summary(pm_OO_ns_surf_TpL)
vif(pm_OO_ns_surf_TpL)
pm_OO_ns_surf_TpL.step <- step(pm_OO_ns_surf_TpL, direction = "backward")
mean(pm_OO_ns_surf_TpL$residuals^2)

#Polynomial Regression without latitude(***: )
#R2=0.4297, VIF(pH < 10), AIC=1695.81, MSE=5567.375
pm_OO_ns_surf_Tp <- lm(OO~ poly(Temperature, 3) + poly(pH, 2)+Temperature:pH, data = OOfunc_ns_surf)
summary(pm_OO_ns_surf_Tp)
pm_OO_ns_surf_Tp.step <- step(pm_OO_ns_surf_Tp, direction = "backward")
vif(pm_OO_ns_surf_Tp, merge_coef=fause)
mean(pm_OO_ns_surf_Tp$residuals^2)

#Polynomial Regression without pH(***: )
#R2=0.5543, VIF( < 10), AIC=1646.81, MSE=4374.798
pm_OO_ns_surf_TL <- lm(OO~ poly(Temperature, 3) + poly(Latitude, 2), data = OOfunc_ns_surf)
summary(pm_OO_ns_surf_TL)
pm_OO_ns_surf_TL.step <- step(pm_OO_ns_surf_TL, direction = "backward")
vif(pm_OO_ns_surf_TL, merge_coef=fause)
mean(pm_OO_ns_surf_TL$residuals^2)

#Polynomial Regression without Temperature(*: )
#R2=0.5036, VIF( > 10), AIC=1668.77, MSE=4846.43
pm_OO_ns_surf_pL <- lm(OO~ poly(pH, 3) + poly(Latitude, 2)+pH:Latitude, data = OOfunc_ns_surf)
summary(pm_OO_ns_surf_pL)
pm_OO_ns_surf_pL.step <- step(pm_OO_ns_surf_pL, direction = "backward")
vif(pm_OO_ns_surf_pL, merge_coef=fause)
mean(pm_OO_ns_surf_pL$residuals^2)


#Polynomial Regression only Temperature(***: All)
#R2=0.4216, VIF(Null), AIC=1695.66, MSE=5736.868
pm_OO_ns_surf_T <- lm(OO~ poly(Temperature, 3), data = OOfunc_ns_surf)
summary(pm_OO_ns_surf_T)
pm_OO_ns_surf_T_1.step <- step(pm_OO_ns_surf_T, direction = "backward")
mean(pm_OO_ns_surf_T$residuals^2)

#Polynomial Regression model only pH(***: Intercept, **: pH)
#R2=0.276, VIF(Null), AIC=1739.44, MSE=7180.842
pm_OO_ns_surf_p <- lm(OO~ poly(pH, 3), data = OOfunc_ns_surf)
summary(pm_OO_ns_surf_p)
pm_OO_ns_surf_p.step <- step(pm_OO_ns_surf_p, direction = "backward")
mean(pm_OO_ns_surf_p$residuals^2)

#Polynomial Regression model only Latitude(***: All)
#R2=0.2773, VIF(Null),AIC=1738.12, MSE=7206.052
pm_OO_ns_surf_L <- lm(OO~ poly(Latitude, 2), data = OOfunc_ns_surf)
summary(pm_OO_ns_surf_L)
pm_OO_ns_surf_L.step <- step(pm_OO_ns_surf_L, direction = "backward")
mean(pm_OO_ns_surf_L$residuals^2)


#Shannon Index Polynomial regression models of non-saline surface samples 
Shanfunc_ns_surf <- Shanfunc[which(Shanfunc$Location == 'Surface (ns)'), ]

#1.All samples' Polynomial Regression model
#Polynomial Regression model without location(***: , .: )
#R2=0.5055, VIF(All > 10), AIC=-123.96, MSE=0.4828741
pm_Shan_ns_surf_TpL <- lm(Shannon~ poly(Temperature , 1) + poly(pH, 3) + poly(Latitude, 2) + Temperature:Latitude + pH:Latitude, data = Shanfunc_ns_surf)
summary(pm_Shan_ns_surf_TpL)
vif(pm_Shan_ns_surf_TpL)
pm_Shan_ns_surf_TpL.step <- step(pm_Shan_ns_surf_TpL, direction = "backward")
mean(pm_Shan_ns_surf_TpL$residuals^2)

#Polynomial Regression without latitude(***: )
#R2=0.2916, VIF(pH < 10), AIC=-57.71, MSE=0.7066545
pm_Shan_ns_surf_Tp <- lm(Shannon~ poly(Temperature, 1) + poly(pH, 2)+Temperature:pH, data = Shanfunc_ns_surf)
summary(pm_Shan_ns_surf_Tp)
pm_Shan_ns_surf_Tp.step <- step(pm_Shan_ns_surf_Tp, direction = "backward")
vif(pm_Shan_ns_surf_Tp, merge_coef=fause)
mean(pm_Shan_ns_surf_Tp$residuals^2)

#Polynomial Regression without pH(***: )
#R2=0.4443, VIF( < 10), AIC=-105.06, MSE=0.5543078
pm_Shan_ns_surf_TL <- lm(Shannon~ poly(Temperature, 2) + poly(Latitude, 2) , data = Shanfunc_ns_surf)
summary(pm_Shan_ns_surf_TL)
pm_Shan_ns_surf_TL.step <- step(pm_Shan_ns_surf_TL, direction = "backward")
vif(pm_Shan_ns_surf_TL, merge_coef=fause)
mean(pm_Shan_ns_surf_TL$residuals^2)

#Polynomial Regression without Temperature(*: )
#R2=0.393, VIF( > 10), AIC=-86.87, MSE=0.6022944
pm_Shan_ns_surf_pL <- lm(Shannon~ poly(pH, 3) + poly(Latitude, 2), data = Shanfunc_ns_surf)
summary(pm_Shan_ns_surf_pL)
pm_Shan_ns_surf_pL.step <- step(pm_Shan_ns_surf_pL, direction = "backward")
vif(pm_Shan_ns_surf_pL, merge_coef=fause)
mean(pm_Shan_ns_surf_pL$residuals^2)


#Polynomial Regression only Temperature(***: All)
#R2=0.2693, VIF(Null), AIC=-52.65, MSE=0.7326885
pm_Shan_ns_surf_T <- lm(Shannon~ poly(Temperature, 3), data = Shanfunc_ns_surf)
summary(pm_Shan_ns_surf_T)
pm_Shan_ns_surf_T_1.step <- step(pm_Shan_ns_surf_T, direction = "backward")
mean(pm_Shan_ns_surf_T$residuals^2)

#Polynomial Regression model only pH(***: Intercept, **: pH)
#R2=0.1939, VIF(Null), AIC=-33.49, MSE=0.8083348
pm_Shan_ns_surf_p <- lm(Shannon~ poly(pH, 3), data = Shanfunc_ns_surf)
summary(pm_Shan_ns_surf_p)
pm_Shan_ns_surf_p.step <- step(pm_Shan_ns_surf_p, direction = "backward")
vif(pm_Shan_ns_surf_p, merge_coef=fause)
mean(pm_Shan_ns_surf_p$residuals^2)

#Polynomial Regression model only Latitude(***: All)
#R2=0.2714, VIF(Null),AIC=-54.19, MSE=0.734434
pm_Shan_ns_surf_L <- lm(Shannon~ poly(Latitude, 2), data = Shanfunc_ns_surf)
summary(pm_Shan_ns_surf_L)
pm_Shan_ns_surf_L.step <- step(pm_Shan_ns_surf_L, direction = "backward")
vif(pm_Shan_ns_surf_L, merge_coef=fause)
mean(pm_Shan_ns_surf_L$residuals^2)


#Chao1 Index Polynomial regression models of saline sediment samples 
chaofunc_s_sedi <- Chaofunc[which(Chaofunc$Location == 'Sediment (s)'), ]

#1.All samples' Polynomial Regression model(NULL)
#Polynomial Regression model without location(***: , .: )
#R2=0.5044, VIF(All > 10), AIC=1856.2, MSE=284704.7
pm_chao_s_sedi_TpL <- lm(Chao1~ poly(Temperature , 1) + poly(pH, 1) + poly(Latitude, degree=1) + Temperature:Latitude, data = chaofunc_s_sedi)
summary(pm_chao_s_sedi_TpL)
vif(pm_chao_s_sedi_TpL)
pm_chao_s_sedi_TpL.step <- step(pm_chao_s_sedi_TpL, direction = "backward")
mean(pm_chao_s_sedi_TpL$residuals^2)

#Polynomial Regression without latitude(***: )
#R2=0.4832, VIF(pH < 10), AIC=1862.38, MSE=296913.2
pm_chao_s_sedi_Tp <- lm(Chao1~ poly(Temperature, 3) + poly(pH, 1), data = chaofunc_s_sedi)
summary(pm_chao_s_sedi_Tp)
pm_chao_s_sedi_Tp.step <- step(pm_chao_s_sedi_Tp, direction = "backward")
vif(pm_chao_s_sedi_Tp, merge_coef=fause)
mean(pm_chao_s_sedi_Tp$residuals^2)

#Polynomial Regression without pH(***: )
#R2=0.4758, VIF( < 10), AIC=1866.38, MSE=296923.8
pm_chao_s_sedi_TL <- lm(Chao1~ poly(Temperature, 3) + poly(Latitude, 3), data = chaofunc_s_sedi)
summary(pm_chao_s_sedi_TL)
pm_chao_s_sedi_TL.step <- step(pm_chao_s_sedi_TL, direction = "backward")
vif(pm_chao_s_sedi_TL, merge_coef=fause)
mean(pm_chao_s_sedi_TL$residuals^2)

#Polynomial Regression without Temperature(*: )
#R2=0.5018, VIF( > 10), AIC=1856.03, MSE=288267
pm_chao_s_sedi_pL <- lm(Chao1~ poly(pH, 1) + poly(Latitude, 2), data = chaofunc_s_sedi)
summary(pm_chao_s_sedi_pL)
pm_chao_s_sedi_pL.step <- step(pm_chao_s_sedi_pL, direction = "backward")
vif(pm_chao_s_sedi_pL, merge_coef=fause)
mean(pm_chao_s_sedi_pL$residuals^2)


#Polynomial Regression only Temperature(***: All)
#R2=0.4322, VIF(Null), AIC=1874.26, MSE=330786.9
pm_chao_s_sedi_T <- lm(Chao1~ poly(Temperature, 2), data = chaofunc_s_sedi)
summary(pm_chao_s_sedi_T)
pm_chao_s_sedi_T_1.step <- step(pm_chao_s_sedi_T, direction = "backward")
vif(pm_chao_s_sedi_T, merge_coef=fause)
mean(pm_chao_s_sedi_T$residuals^2)

#Polynomial Regression model only pH(***: Intercept, **: pH)
#R2=0.09837, VIF(Null), AIC=1943.22, MSE=521645.2
pm_chao_s_sedi_p <- lm(Chao1~ poly(pH, 3), data = chaofunc_s_sedi)
summary(pm_chao_s_sedi_p)
pm_chao_s_sedi_p.step <- step(pm_chao_s_sedi_p, direction = "backward")
vif(pm_chao_s_sedi_p, merge_coef=fause)
mean(pm_chao_s_sedi_p$residuals^2)

#Polynomial Regression model only Latitude(***: All)
#R2=0.4559, VIF(Null),AIC=1867.99, MSE=316980.5
pm_chao_s_sedi_L <- lm(Chao1~ poly(Latitude, 2), data = chaofunc_s_sedi)
summary(pm_chao_s_sedi_L)
pm_chao_s_sedi_L.step <- step(pm_chao_s_sedi_L, direction = "backward")
vif(pm_chao_s_sedi_L, merge_coef=fause)
mean(pm_chao_s_sedi_L$residuals^2)


#Observed OTUs Polynomial regression models of saline sediment samples 
OOfunc_s_sedi <- OOfunc[which(OOfunc$Location == 'Sediment (s)'), ]

#1.All samples' Polynomial Regression model
#Polynomial Regression model without location(***: , .: )
#R2=0.578, VIF(ALL > 10), AIC=1619.2, MSE= 56781.8
pm_OO_s_sedi_TpL <- lm(OO~ poly(Temperature , 1) + poly(pH, 1) + poly(Latitude, 1) + pH:Latitude, data = OOfunc_s_sedi)
summary(pm_OO_s_sedi_TpL)
vif(pm_OO_s_sedi_TpL)
pm_OO_s_sedi_TpL.step <- step(pm_OO_s_sedi_TpL, direction = "backward")
mean(pm_OO_s_sedi_TpL$residuals^2)

#Polynomial Regression without latitude(***: )
#R2=0.5499, VIF(pH < 10), AIC=1628.69, MSE=60567.6
pm_OO_s_sedi_Tp <- lm(OO~ poly(Temperature, 3) + poly(pH, 1), data = OOfunc_s_sedi)
summary(pm_OO_s_sedi_Tp)
pm_OO_s_sedi_Tp.step <- step(pm_OO_s_sedi_Tp, direction = "backward")
vif(pm_OO_s_sedi_Tp, merge_coef=fause)
mean(pm_OO_s_sedi_Tp$residuals^2)

#Polynomial Regression without pH(***: )
#R2=0.5554, VIF( < 10), AIC=1628.8, MSE=58983.48
pm_OO_s_sedi_TL <- lm(OO~ poly(Temperature, 3) + poly(Latitude, 3), data = OOfunc_s_sedi)
summary(pm_OO_s_sedi_TL)
pm_OO_s_sedi_TL.step <- step(pm_OO_s_sedi_TL, direction = "backward")
vif(pm_OO_s_sedi_TL, merge_coef=fause)
mean(pm_OO_s_sedi_TL$residuals^2)

#Polynomial Regression without Temperature(*: )
#R2=0.5795, VIF( > 10), AIC=1618.69, MSE=56584.47
pm_OO_s_sedi_pL <- lm(OO~ poly(pH, 1) + poly(Latitude, 3), data = OOfunc_s_sedi)
summary(pm_OO_s_sedi_pL)
pm_OO_s_sedi_pL.step <- step(pm_OO_s_sedi_pL, direction = "backward")
vif(pm_OO_s_sedi_pL, merge_coef=fause)
mean(pm_OO_s_sedi_pL$residuals^2)


#Polynomial Regression only Temperature(***: All)
#R2=0.5115, VIF(Null), AIC=1638.76, MSE=66650.77
pm_OO_s_sedi_T <- lm(OO~ poly(Temperature, 2), data = OOfunc_s_sedi)
summary(pm_OO_s_sedi_T)
pm_OO_s_sedi_T_1.step <- step(pm_OO_s_sedi_T, direction = "backward")
mean(pm_OO_s_sedi_T$residuals^2)

#Polynomial Regression model only pH(***: Intercept, **: pH)
#R2=0.1161, VIF(Null), AIC=1726.91, MSE=119763.6
pm_OO_s_sedi_p <- lm(OO~ poly(pH, 3), data = OOfunc_s_sedi)
summary(pm_OO_s_sedi_p)
pm_OO_s_sedi_p.step <- step(pm_OO_s_sedi_p, direction = "backward")
mean(pm_OO_s_sedi_p$residuals^2)

#Polynomial Regression model only Latitude(***: All)
#R2=0.545, VIF(Null),AIC=1629.32, MSE=61658.33
pm_OO_s_sedi_L <- lm(OO~ poly(Latitude, 3), data = OOfunc_s_sedi)
summary(pm_OO_s_sedi_L)
pm_OO_s_sedi_L.step <- step(pm_OO_s_sedi_L, direction = "backward")
mean(pm_OO_s_sedi_L$residuals^2)


#Shannon Index Polynomial regression models of saline sediment samples 
Shanfunc_s_sedi <- Shanfunc[which(Shanfunc$Location == 'Sediment (s)'), ]

#1.All samples' Polynomial Regression model
#Polynomial Regression model without location(***: , .: )
#R2=, VIF(All > 10), AIC=, MSE=
pm_Shan_s_sedi_TpL <- lm(Shannon~   , data = Shanfunc_s_sedi)
summary(pm_Shan_s_sedi_TpL)
vif(pm_Shan_s_sedi_TpL)
pm_Shan_s_sedi_TpL.step <- step(pm_Shan_s_sedi_TpL, direction = "backward")
mean(pm_Shan_s_sedi_TpL$residuals^2)

#Polynomial Regression without latitude(***: )
#R2=, VIF(pH < 10), AIC=, MSE=
pm_Shan_s_sedi_Tp <- lm(Shannon~ , data = Shanfunc_s_sedi)
summary(pm_Shan_s_sedi_Tp)
pm_Shan_s_sedi_Tp.step <- step(pm_Shan_s_sedi_Tp, direction = "backward")
vif(pm_Shan_s_sedi_Tp, merge_coef=fause)
mean(pm_Shan_s_sedi_Tp$residuals^2)

#Polynomial Regression without pH(***: )
#R2=, VIF( < 10), AIC=, MSE=
pm_Shan_s_sedi_TL <- lm(Shannon~ , data = Shanfunc_s_sedi)
summary(pm_Shan_s_sedi_TL)
pm_Shan_s_sedi_TL.step <- step(pm_Shan_s_sedi_TL, direction = "backward")
vif(pm_Shan_s_sedi_TL, merge_coef=fause)
mean(pm_Shan_s_sedi_TL$residuals^2)

#Polynomial Regression without Temperature(*: )
#R2=, VIF( > 10), AIC=, MSE=
pm_Shan_s_sedi_pL <- lm(Shannon~ , data = Shanfunc_s_sedi)
summary(pm_Shan_s_sedi_pL)
pm_Shan_s_sedi_pL.step <- step(pm_Shan_s_sedi_pL, direction = "backward")
vif(pm_Shan_s_sedi_pL, merge_coef=fause)
mean(pm_Shan_s_sedi_pL$residuals^2)


#Polynomial Regression only Temperature(***: All)
#R2=0.4102, VIF(Null), AIC=160.06, MSE=2.851994
pm_Shan_s_sedi_T <- lm(Shannon~ poly(Temperature, 2), data = Shanfunc_s_sedi)
summary(pm_Shan_s_sedi_T)
pm_Shan_s_sedi_T_1.step <- step(pm_Shan_s_sedi_T, direction = "backward")
mean(pm_Shan_s_sedi_T$residuals^2)

#Polynomial Regression model only pH(***: Intercept, **: pH)
#R2=0.1067, VIF(Null), AIC=222.08, MSE=4.290067
pm_Shan_s_sedi_p <- lm(Shannon~ poly(pH, 3), data = Shanfunc_s_sedi)
summary(pm_Shan_s_sedi_p)
pm_Shan_s_sedi_p.step <- step(pm_Shan_s_sedi_p, direction = "backward")
mean(pm_Shan_s_sedi_p$residuals^2)

#Polynomial Regression model only Latitude(***: All)
#R2=0.4102, VIF(Null),AIC=160.07, MSE=2.852136
pm_Shan_s_sedi_L <- lm(Shannon~ poly(Latitude, 2), data = Shanfunc_s_sedi)
summary(pm_Shan_s_sedi_L)
pm_Shan_s_sedi_L.step <- step(pm_Shan_s_sedi_L, direction = "backward")
mean(pm_Shan_s_sedi_L$residuals^2)
