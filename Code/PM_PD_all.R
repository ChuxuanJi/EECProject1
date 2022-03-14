rm(list = ls())
#loading package

library(car)
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
